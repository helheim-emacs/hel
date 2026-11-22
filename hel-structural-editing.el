;;; hel-structural-editing.el --- Syntax tree editing -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.13.0
;; Homepage: https://github.com/helheim-emacs/hel
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; Hel is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Hel is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The tree level of the thing protocol: the operations on nodes, the
;; producers of nodes, and the algorithms that walk a syntax tree. The node
;; commands in `hel-commands.el' call them: `M-o' expands the selection to
;; the enclosing node, `M-n' selects the next sibling.
;;
;; The run level in `hel-text-objects.el' gives things one after another.
;; The tree level knows the parent and the children of a node. The two do
;; not overlap, and a node is not a thing: nothing dispatches on a kind of
;; node. The first function in `hel-node-source-functions' that returns
;; a producer gives the tree of the buffer, so a new tree is a new function
;; in that hook. The operations on a node take only the producer and
;; dispatch on it.
;;
;; A node is whatever the producer uses for it: a tree-sitter node, or the
;; (BEGIN . END) range of a sexp. `hel-node-bounds' gives its bounds.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'thingatpt)
(require 'treesit)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-cursors)
(require 'hel-core)
(require 'hel-text-objects)

;;; Operations on nodes

(cl-defgeneric hel-node-on (producer range)
  "Return the smallest node that covers RANGE, or nil.")

(cl-defgeneric hel-node-bounds (producer node)
  "Return the (BEGIN . END) bounds of NODE.")

(cl-defgeneric hel-node-parent (producer node)
  "Return the parent of NODE, or nil.")

(cl-defgeneric hel-node-children (producer node)
  "Return the children of NODE, in buffer order.")

(cl-defgeneric hel-node-sibling (producer node dir)
  "Return the sibling of NODE in direction DIR, or nil.
DIR is 1 for forward or -1 for backward.")

(cl-defgeneric hel-node-enclosing (producer pos)
  "Return the smallest node with POS inside it, not on its boundary, or nil.")

(cl-defgeneric hel-node-enclosing-leaf (producer pos)
  "Return the node without children that has POS inside it, or nil.
POS on the boundary of the node does not count.

By default, this is the enclosing node of POS when it has no
children, so a producer that defines `hel-node-enclosing' and
`hel-node-children' gets this operation without a method of its own."
  (when-let* ((enclosing (hel-node-enclosing producer pos))
              ((null (hel-node-children producer enclosing))))
    enclosing))

(cl-defgeneric hel-node-touching (producer pos side)
  "Return the smallest node with POS on its boundary, not inside it, or nil.
With SIDE 1 the node starts at POS, and with SIDE -1 it ends there.")

(cl-defgeneric hel-node-side-child (producer pos side)
  "Return the nearest node next to POS on SIDE, or nil.
With SIDE 1 the node starts at or after POS, and with SIDE -1 it ends at
or before POS. The node lies on the same level as POS: it is a child of
the node that encloses POS.

By default, the children are those of the enclosing node of POS, so
a producer that defines `hel-node-enclosing' and `hel-node-children'
gets this operation without a method of its own."
  (when-let* ((enclosing (hel-node-enclosing producer pos))
              (children (hel-node-children producer enclosing)))
    (if (< 0 side)
        (-first (lambda (child)
                  (<= pos (car (hel-node-bounds producer child))))
                children)
      (-last (lambda (child)
               (<= (cdr (hel-node-bounds producer child)) pos))
             children))))

(cl-defgeneric hel-node-expand-steps (producer range-or-pos)
  "Return the ranges that expanding the selection can select.
The resulting ranges may come in any order: each step of the expansion
selects the smallest of them that encloses the selection. RANGE-OR-POS is
the selection that the expansion starts from, or a position, which stands
for the character after it.

By default, the steps are the node that covers the selection and each
node above it, without the root."
  (let ((node (hel-node-on producer (pcase-exhaustive range-or-pos
                                      ((and (pred numberp) pos)
                                       (hel-char-range pos))
                                      ((pred -cons-pair-p)
                                       range-or-pos))))
        ranges)
    (while-let ((node)
                (parent (hel-node-parent producer node)))
      (push (hel-node-bounds producer node) ranges)
      (setq node parent))
    ranges))

;;; Node producers
;;;; Tree-sitter

(cl-defstruct (hel-tree-treesit-producer
               (:constructor hel-tree-treesit-producer-create)
               (:copier nil))
  "The producer of the named nodes of the tree-sitter parser.")

(defun hel-tree-treesit-source ()
  "Return the producer of tree-sitter nodes, or nil without a parser."
  (when (hel-treesit-active-p)
    (hel-tree-treesit-producer-create)))

(cl-defmethod hel-node-on ((_producer hel-tree-treesit-producer) range)
  "Return the smallest named node that covers RANGE."
  (treesit-node-on (car range) (cdr range) nil t))

(cl-defmethod hel-node-bounds ((_producer hel-tree-treesit-producer) node)
  (hel-treesit-node-bounds node))

(cl-defmethod hel-node-parent ((_producer hel-tree-treesit-producer) node)
  (treesit-node-parent node))

(cl-defmethod hel-node-children ((_producer hel-tree-treesit-producer) node)
  "Return the named children of NODE."
  (treesit-node-children node t))

(cl-defmethod hel-node-sibling ((_producer hel-tree-treesit-producer) node dir)
  "Return the named sibling of NODE in direction DIR.
When NODE has no sibling, but a parent has the same bounds as NODE,
return the sibling of that parent. The user sees such nodes as one
selection, so the motion must not stop at the one without a sibling."
  (let ((range (hel-treesit-node-bounds node)))
    (cl-loop for same = node
             then (treesit-node-parent same)
             while (and same
                        (equal range (hel-treesit-node-bounds same)))
             thereis (if (< 0 dir)
                         (treesit-node-next-sibling same t)
                       (treesit-node-prev-sibling same t)))))

(cl-defmethod hel-node-enclosing ((_producer hel-tree-treesit-producer) pos)
  "Return the smallest named node with POS inside it, not on its boundary."
  (cl-loop for node = (treesit-node-on pos pos nil t)
           then (treesit-node-parent node)
           while node
           when (and (treesit-node-check node 'named)
                     (< (treesit-node-start node) pos (treesit-node-end node)))
           return node))

(cl-defmethod hel-node-touching ((_producer hel-tree-treesit-producer) pos side)
  "Return the smallest named node with POS on its boundary, on SIDE."
  (when-let* ((char (if (< 0 side) pos (1- pos)))
              ((<= (point-min) char))
              ((< char (point-max)))
              (node (treesit-node-on char (1+ char) nil t))
              ((= pos (if (< 0 side)
                          (treesit-node-start node)
                        (treesit-node-end node)))))
    node))

;;;; Sexps

(cl-defstruct (hel-tree-sexp-producer
               (:constructor hel-tree-syntax-source)
               (:copier nil))
  "The producer of the sexps of the syntax table as nodes.
A node is the (BEGIN . END) range of its sexp. The parent of a sexp is the
innermost bracket pair around it, and the parent of a top-level sexp is the
whole buffer. The expand steps come from the syntax table: words, strings,
comments, the insides of bracket pairs, sentences and paragraphs.")

(cl-defstruct (hel-tree-lisp-producer
               (:include hel-tree-sexp-producer)
               (:constructor hel-tree-lisp-source)
               (:copier nil))
  "The producer of the sexps of a Lisp buffer as nodes.
In Lisp the code is the syntax tree, so the expand steps are the sexps
alone, as if a parser gave them.")

(defun hel-use-lisp-nodes ()
  "Make the node commands take sexps as nodes in the current buffer.
Add this function to the hook of a major mode whose code is its own syntax
tree. The sexps take priority over a tree-sitter parser."
  (add-hook 'hel-node-source-functions #'hel-tree-lisp-source nil t))

(defun hel--sexp-root ()
  "Return the root of the sexp tree: the bounds of the whole buffer."
  (cons (point-min) (point-max)))

(cl-defmethod hel-node-on ((_producer hel-tree-sexp-producer) range)
  "Return the sexp at the start of RANGE, stretched to cover RANGE.
The sexp is a comment that starts there, the list that a closing bracket
there ends, or the sexp that starts there or holds the symbol there.
When RANGE goes past the end of that sexp, the result ends at the end
of RANGE, so it may be no single sexp. Return nil when no sexp is at the
start of RANGE."
  (-let [(begin . end) range]
    (save-excursion
      (goto-char begin)
      (ignore-errors
        (cond (;; A comment is a node of its own. The sexp scan would skip it.
               (and (char-after)
                    (not (hel-blank-p (char-after)))
                    (forward-comment 1))
               (cons begin (max (->> (cons begin (point))
                                     (hel-trim-whitespace-from-range)
                                     (cdr))
                                end)))
              ((and (char-after)
                    (eq ?\) (char-syntax (char-after))))
               (let ((stop (1+ begin)))
                 (cons (hel--sexp-begin stop)
                       (max stop end))))
              (t
               (when (memq (char-syntax (char-after)) '(?w ?_))
                 (skip-syntax-backward "w_"))
               (when-let* ((stop (scan-sexps (point) 1))
                           (start (hel--sexp-begin stop))
                           ((< start end))
                           ((< begin stop)))
                 (cons start (max stop end)))))))))

(cl-defmethod hel-node-bounds ((_producer hel-tree-sexp-producer) node)
  node)

(cl-defmethod hel-node-parent ((_producer hel-tree-sexp-producer) node)
  "Return the innermost bracket pair around NODE, or the whole buffer."
  (or (-first-item (hel--bracket-sexps (car node)))
      (unless (equal node (hel--sexp-root))
        (hel--sexp-root))))

(cl-defmethod hel-node-children ((_producer hel-tree-sexp-producer) node)
  "Return the sexps inside NODE.
Of a bracketed form, these are the sexps between its brackets. NODE itself
is never among them."
  (-let* (((begin . end) node)
          (open (ignore-errors (scan-sexps end -1))))
    (if (and open
             (eq ?\( (char-syntax (char-after open)))
             (eql begin (hel--prefix-begin open)))
        (hel--sexps-in (cons (1+ open) (1- end)))
      ;; else
      (->> (hel--sexps-in node)
           (-remove (-partial #'equal node))))))

(cl-defmethod hel-node-sibling ((_producer hel-tree-sexp-producer) node dir)
  "Return the sexp next to NODE inside the same bracket pair."
  (-let [(begin . end) node]
    (if (< 0 dir)
        (hel--sexp-after end)
      (hel--sexp-before begin))))

(cl-defmethod hel-node-enclosing ((_producer hel-tree-sexp-producer) pos)
  "Return the innermost bracket pair around POS, or the whole buffer."
  (or (-first-item (hel--bracket-sexps pos))
      (if (< (point-min) pos (point-max))
          (hel--sexp-root))))

(cl-defmethod hel-node-enclosing-leaf ((_producer hel-tree-sexp-producer) pos)
  "Return the bounds of the symbol, number, string or comment around POS.
POS inside an empty bracket pair does not count."
  (hel--sexp-leaf pos))

(cl-defmethod hel-node-touching ((_producer hel-tree-sexp-producer) pos side)
  "Return the sexp with POS on its boundary, on SIDE."
  (if (< 0 side)
      (when-let* ((sexp (hel--sexp-after pos))
                  ((= pos (car sexp))))
        sexp)
    ;; else
    (when-let* ((sexp (hel--sexp-before pos))
                ((= pos (cdr sexp))))
      sexp)))

(cl-defmethod hel-node-side-child ((_producer hel-tree-sexp-producer) pos side)
  "Return the sexp next to POS on SIDE, inside the same bracket pair."
  (if (< side 0)
      (hel--sexp-before pos)
    (hel--sexp-after pos)))

(cl-defmethod hel-node-expand-steps ((_producer hel-tree-sexp-producer)
                                     range-or-pos)
  "Return what expanding can select around the start of RANGE-OR-POS.
This is the word and the symbol there, the string, the comment and the
bracket pairs around it, the inside of the string and of the innermost
pair, the sentence in `text-mode' and the paragraph."
  (save-excursion
    (let* ((pos (pcase-exhaustive range-or-pos
                  ((and (pred numberp) pos) pos)
                  (`(,begin . ,_) begin)))
           (on-word (memq (char-syntax (or (char-after pos) ?\s))
                          '(?w ?_))))
      (append (->> `(,@(if on-word '(word))
                     ,@(if (derived-mode-p 'text-mode) '(sentence))
                     paragraph)
                   (-keep (lambda (thing)
                            (when-let* ((producer (hel-thing-producer thing pos))
                                        (instance (hel-thing-at thing producer pos)))
                              (hel-thing-instance-bounds instance)))))
              (when-let* ((on-word)
                          (symbol (progn (goto-char pos)
                                         (bounds-of-thing-at-point 'symbol))))
                (list symbol))
              (hel--syntax-ppss-bounds pos)
              ;; The cursor sits on the character after `pos'. When that
              ;; character opens a bracket pair, a string or a comment, the
              ;; scan from the next position finds it.
              (if (< pos (point-max))
                  (hel--syntax-ppss-bounds (1+ pos)))))))

(cl-defmethod hel-node-expand-steps ((_producer hel-tree-lisp-producer)
                                     range-or-pos)
  "Return what expanding can select around the start of RANGE-OR-POS.
This is the symbol, the string or the comment there, and each list around
it as a whole. The inside of a list or of a string is not a step,
because `mi(' and `mi\"' select those."
  (let ((begin (pcase-exhaustive range-or-pos
                 ((and (pred numberp) pos) pos)
                 (`(,begin . ,_) begin))))
    ;; The cursor sits on the character after `begin'. When that
    ;; character opens a list, a string or a comment, or starts a symbol,
    ;; the scan from the next position finds that sexp.
    (cons (or (hel--sexp-leaf begin)
              (if (< begin (point-max))
                  (hel--sexp-leaf (1+ begin))))
          (append (hel--bracket-sexps begin)
                  (if (< begin (point-max))
                      (hel--bracket-sexps (1+ begin)))))))

(defun hel--syntax-ppss-bounds (pos)
  "Return the bounds of the strings, comments and bracket pairs around POS.
For a string, the result holds its bounds with the quotes and without
them. For each bracket pair it holds the bounds with the brackets, and
for the innermost pair also the bounds of its content, without the
brackets and without the whitespace at the edges of the content. The
bounds of a comment leave out the whitespace at its edges."
  (let* ((ppss (save-excursion (syntax-ppss pos)))
         ranges)
    (-when-let* ((brackets (hel--bracket-bounds pos))
                 ((beg . end) (car brackets)))
      (setq ranges (cons (hel-trim-whitespace-from-range (cons (1+ beg) (1- end)))
                         brackets)))
    (when-let* ((begin (nth 8 ppss))) ; comment or string
      (if (nth 3 ppss) ; string
          (when-let* ((end (ignore-errors (scan-sexps begin 1))))
            (push (cons begin end) ranges)
            (push (cons (1+ begin) (1- end)) ranges))
        ;; else comment
        (push (-some-> (hel-bounds-of-comment-at pos)
                (hel-trim-whitespace-from-range))
              ranges)))
    (delq nil ranges)))

(defun hel--prefix-begin (pos)
  "Return POS moved back over the prefix characters before it.
A prefix character stands before a sexp and belongs to it, such as a quote."
  (save-excursion
    (goto-char pos)
    (backward-prefix-chars)
    (point)))

(defun hel--sexp-begin (end)
  "Return the start of the sexp that ends at END, with its prefix characters."
  (hel--prefix-begin (scan-sexps end -1)))

(defun hel--sexp-after (pos)
  "Return the bounds of the first sexp that starts at or after POS, or nil.
The sexp lies in the bracket pair around POS."
  (when-let* ((end (ignore-errors (scan-sexps pos 1)))
              (begin (hel--sexp-begin end))
              ((<= pos begin)))
    (cons begin end)))

(defun hel--sexp-before (pos)
  "Return the bounds of the last sexp that ends at or before POS, or nil.
The sexp lies in the bracket pair around POS."
  (when-let* ((begin (ignore-errors (scan-sexps pos -1)))
              (end   (ignore-errors (scan-sexps begin 1)))
              ((<= end pos)))
    (cons (hel--prefix-begin begin) end)))

(cl-defun hel--sexps-in ((begin . end))
  "Return the bounds of the sexps from BEGIN to END, in buffer order."
  (let ((pos begin)
        sexps)
    (while-let ((sexp (hel--sexp-after pos))
                ((<= (cdr sexp) end)))
      (push sexp sexps)
      (setq pos (cdr sexp)))
    (nreverse sexps)))

(defun hel--bracket-bounds (pos)
  "Return the bounds of the bracket pairs around POS, the innermost first."
  (save-excursion
    (->> (syntax-ppss pos)
         (nth 9)
         (reverse)
         (-keep (lambda (begin)
                  (if-let* ((end (ignore-errors (scan-lists begin 1 0))))
                      (cons begin end)))))))

(defun hel--bracket-sexps (pos)
  "Return the bounds of the bracketed sexps around POS, the innermost first.
The bounds include the prefix characters of each sexp."
  (->> (hel--bracket-bounds pos)
       (-map (-lambda ((begin . end))
               (cons (hel--prefix-begin begin) end)))))

(defun hel--sexp-leaf (pos)
  "Return the bounds of the symbol, number, string or comment around POS.
Return nil when POS lies in none of them."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (cond ((nth 3 ppss) ; in string
             (when-let* ((begin (nth 8 ppss))
                         (end (ignore-errors (scan-sexps begin 1)))
                         ((< begin pos end)))
               (cons begin end)))
            ((nth 4 ppss) ; in comment
             (when-let* ((comment (-some-> (hel-bounds-of-comment-at pos)
                                    (hel-trim-whitespace-from-range)))
                         ((< (car comment) pos (cdr comment))))
               comment))
            (t
             (when-let* ((end (ignore-errors (scan-sexps pos 1)))
                         (begin (hel--sexp-begin end))
                         ((< begin pos)))
               (cons begin end)))))))

;;; Algorithms over trees

(defun hel--node-on-selection (producer)
  "Return the node that covers the selection, or nil.
The whitespace at the edges of the selection does not count. A bare
cursor inside a node without children, such as on a space in a string,
gives that node."
  (when-let* ((selection (hel-trimmed-selection)))
    (or (when (hel-bare-cursor-p selection)
          (hel-node-enclosing-leaf producer (hel-selection-head selection)))
        (hel-node-on producer (hel-selection-bounds selection)))))

(defun hel--selection-between-nodes-p (producer)
  "Return non-nil when no node is under the selection.
This is a bare cursor between the children of a node, such as on the
space or the comma between two arguments. A character at the edge of
a node, such as its bracket, is under that node, and so is a character
inside a node without children, such as a space in a string. The
whitespace at the edges of the selection does not count, as in
`hel--node-on-selection', so the two agree on which node is under the
selection. The tree commands start from the nodes around a selection
between nodes."
  (-when-let ((selection &as (begin . end)) (hel-trimmed-selection))
    (and (hel-bare-cursor-p selection)
         (not (hel-node-enclosing-leaf producer begin))
         (not (hel-node-enclosing-leaf producer end))
         (-let [(node-begin . node-end)
                (-some->> (hel-node-on producer (hel-selection-bounds selection))
                  (hel-node-bounds producer))]
           (or (null node-begin)
               (and (< node-begin begin)
                    (< end node-end)))))))

(defun hel--node-beside-selection (producer dir)
  "Return the nearest node in direction DIR next to the selection, or nil.
With DIR 1 this is the first node that starts after the selection, and
with DIR -1 the last node that ends before it. The node lies on the same
level as the selection."
  (-let [(begin . end) (hel-selection-bounds)]
    (hel-node-side-child producer (if (< 0 dir) end begin) dir)))

(defun hel--node-single-child-chain-end (producer node)
  "Return the innermost node that has the same bounds as NODE.
Go down from NODE while its only child has the same bounds."
  (let ((range (hel-node-bounds producer node))
        children)
    (while (and (setq children (hel-node-children producer node))
                (null (cdr children))
                (equal range (hel-node-bounds producer (car children))))
      (setq node (car children)))
    node))

(defun hel--node-all-children (producer node)
  "Return the bounds of the children of NODE.
When NODE wraps only one child with the same bounds, use the innermost
such child, see `hel--node-single-child-chain-end', because the user
sees such nodes as one selection."
  (->> (hel--node-single-child-chain-end producer node)
       (hel-node-children producer)
       (-map (-partial #'hel-node-bounds producer))))

(defun hel--node-all-siblings (producer node)
  "Return the bounds of NODE and of all its siblings.
When NODE is an only child, use the nearest parent with more than one
child."
  (cl-loop for parent = (hel-node-parent producer node)
           then (hel-node-parent producer parent)
           while parent
           for children = (hel-node-children producer parent)
           when (cdr children)
           return (-map (-partial #'hel-node-bounds producer)
                        children)))

(cl-defun hel--smallest-larger-range (ranges (begin . end))
  "Return the smallest of RANGES that contains BEGIN..END and is larger."
  (-some->> ranges
    (-non-nil)
    (-filter (-lambda ((range &as range-begin . range-end))
               (and (<= range-begin begin end range-end)
                    (not (equal (cons begin end)
                                range)))))
    (-min-by (-on #'> #'hel-range-length))))

;;; Selecting nodes

(defun hel--tree-select-first-child (producer)
  "Select the first child of the node under the selection.
Keep the direction of the selection. At a bare cursor, which has no
direction, the node is the one under the cursor character, and the child
is selected forward.
Return non-nil if the selection changed."
  (-let [(range dir) (hel-selection)]
    (when-let* (((not (hel--selection-between-nodes-p producer)))
                (node (hel--node-on-selection producer))
                (child (-first-item (hel--node-all-children producer node)))
                ((not (equal child range))))
      (hel-select child (or dir 1))
      t)))

(defun hel--tree-shrink-1 (producer)
  "Shrink the selection one step. Return non-nil if it changed.
Return to the selection before the last expand step. When the selection
does not come from an expand step, select the first child of the node
under the selection."
  (hel--sync-selection-history)
  (if hel-selection-history
      (progn
        (pop hel-selection-history)
        (-let [(selection) (car hel-selection-history)]
          (apply #'hel-select selection))
        ;; The oldest entry alone is not a chain any more.
        (unless (cdr hel-selection-history)
          (setq hel-selection-history nil))
        t)
    ;; else
    (hel--tree-select-first-child producer)))

(defconst hel--tree-expand-commands '(hel-expand-selection-backward
                                      hel-expand-selection-forward
                                      hel-shrink-selection)
  "The commands of one series of expand and shrink steps.
The first command of a series pushes the old point to the mark ring.")

(defun hel--tree-expand (producer count side)
  "Expand the selection COUNT times to the enclosing node.
Each step selects the smallest node that holds the selection: a
tree-sitter node, or a sexp in a buffer without a parser. A negative
COUNT shrinks the selection back. SIDE is the side of the expanded
selection where point goes: 1 for its end, -1 for its beginning.
PRODUCER finds the nodes."
  (if (< count 0)
      (dotimes (_ (- count))
        (hel--tree-shrink-1 producer))
    ;; else
    (let ((origin (point))
          changed)
      (dotimes (_ count)
        (when (hel--tree-expand-1 producer side)
          (setq changed t)))
      (when (and changed
                 (not (memq last-command hel--tree-expand-commands)))
        (hel-push-point origin))))
  (hel-reveal-point-when-on-top))

(defun hel--tree-expand-1 (producer side)
  "Expand the selection one step to the enclosing node.
At a bare cursor, the selection is the cursor character, so the first step
selects the smallest node that holds it and is larger. Return non-nil if
the selection changed. SIDE is the side of the new selection where point
goes: 1 for its end, -1 for its beginning."
  (hel--sync-selection-history)
  (let* ((before (hel--selection-entry))
         (chain-start (or (-last-item hel-selection-history) before)))
    (when-let*
        ((range (hel-selection-nonempty-bounds (car before)))
         (new-range
          ;; The steps come from the selection where this chain of
          ;; expands began, so the inside of an outer bracket pair
          ;; never becomes a step.
          (-> (hel-node-expand-steps producer
                                     (hel-selection-nonempty-bounds
                                      (car chain-start)))
              (hel--smallest-larger-range range))))
      (hel-select new-range side)
      (hel--record-selection before)
      t)))

(defun hel--node-under-cursor (producer)
  "Return the largest node inside the selection that holds its head.
The head and the selection leave out the whitespace at the edges of the
selection, see `hel-trimmed-selection'."
  (-when-let* (((selection &as (begin . end)) (hel-trimmed-selection))
               (head (hel-selection-head selection))

               (node (hel-node-on producer (hel-char-range head))))
    (while-let ((node)
                (parent (hel-node-parent producer node))
                ((-let [(parent-begin . parent-end)
                        (hel-node-bounds producer parent)]
                   (<= begin parent-begin parent-end end))))
      (setq node parent))
    node))

(defun hel--tree-extend-to-sibling (producer dir)
  "Move point of the extended selection over one sibling in direction DIR.
Toward the side of point, the selection takes the next sibling. Away from
it, the selection gives back its last node, and when only one node is left,
it keeps that node and turns around. When no node is under the selection,
it takes the nearest node in DIR. Return non-nil if the selection changed."
  (cl-flet ((near (range) (if (< dir 0) (cdr range) (car range)))
            (far  (range) (if (< dir 0) (car range) (cdr range))))
    (if (hel--selection-between-nodes-p producer)
        (when-let* ((anchor (hel-selection-anchor (hel-selection) dir))
                    (node (hel--node-beside-selection producer dir)))
          (hel-select (hel-range anchor (far (hel-node-bounds producer node)))
                      dir)
          t)
      ;; else
      (when-let* ((node (hel--node-under-cursor producer))
                  (range (hel-node-bounds producer node))
                  (sibling (hel-node-sibling producer node dir))
                  (target (hel-node-bounds producer sibling)))
        (-let* ((((begin . end) side) (hel-selection))
                (side (or side dir)))
          (cond ((= dir side)
                 (hel-select (hel-range (if (< 0 side) begin end) (far target))
                             side))
                ;; The sibling still overlaps the selection: give back NODE.
                ((if (< dir 0)
                     (< begin (near target))
                   (< (near target) end))
                 (hel-select (hel-range (if (< 0 side) begin end) (near target))
                             side))
                ;; NODE is the last one: keep it, and go on past the other end.
                (t
                 (hel-select (hel-range (near range) (far target)) dir))))
        t))))

(defun hel--tree-select-sibling (producer count extend)
  "Select the COUNT-th sibling of the node under the selection.
A negative COUNT goes backward. At the last sibling, keep the selection.
Keep the direction of the selection; at a bare cursor, which has no
direction, the direction is that of the motion. When no node is under
the selection, first select the nearest node in the direction of the
motion. With EXTEND, extend the selection instead of replacing it.
PRODUCER finds the nodes."
  (if extend
      (let ((origin (point)))
        (when (/= count
                  (hel-motion-loop (dir count)
                    (hel--tree-extend-to-sibling producer dir)))
          (hel-push-point origin)
          (hel-reveal-point-when-on-top)))
    (let* ((origin (point))
           (motion-dir (hel-sign count))
           (selection-dir (or (hel-selection-direction) motion-dir))
           (between? (hel--selection-between-nodes-p producer))
           (node (if between?
                     (hel--node-beside-selection producer motion-dir)
                   (hel--node-on-selection producer)))
           moved?)
      (when node
        (cl-flet ((select (node)
                    (hel-select (hel-node-bounds producer node) selection-dir)))
          (when between?
            (select node)
            (setq moved? t)
            (cl-callf - count motion-dir))
          (when (/= count
                    (hel-motion-loop (dir count)
                      (when-let* ((sibling (hel-node-sibling producer node dir)))
                        (setq node sibling)
                        (select node))))
            (setq moved? t)))
        (when moved?
          (hel-push-point origin)
          (hel-reveal-point-when-on-top))))))

(defun hel--select-ranges-for-each-cursor (fn)
  "Put a cursor on each range that FN returns, for every cursor.
FN takes no arguments and runs at each cursor with its selection."
  (hel-with-each-cursor
    (hel-extend-selection -1)
    (when-let* ((ranges (funcall fn)))
      (hel-select-ranges ranges (or (hel-selection-direction) 1))))
  (hel-set-emacs-cursor))

(defun hel--tree-select-all-siblings (producer)
  "Put a cursor on the node at each cursor and on its siblings.
When no node is under the selection, use the children of the node around
it. PRODUCER finds the nodes."
  (hel--select-ranges-for-each-cursor
   (lambda ()
     (if-let* (((not (hel--selection-between-nodes-p producer)))
               (node (hel--node-on-selection producer)))
         (hel--node-all-siblings producer node)
       (-some->> (hel-node-enclosing producer (car (hel-selection-bounds)))
         (hel--node-all-children producer))))))

(defun hel--tree-select-all-children (producer)
  "Put a cursor on each child of the node at each cursor.
PRODUCER finds the nodes."
  (hel--select-ranges-for-each-cursor
   (lambda ()
     (when-let* ((node (if (hel--selection-between-nodes-p producer)
                           (hel-node-enclosing producer
                                               (car (hel-selection-bounds)))
                         (hel--node-on-selection producer))))
       (hel--node-all-children producer node)))))

;;; The producer of nodes

(defvar hel-node-source-functions '(hel-tree-treesit-source
                                    hel-tree-syntax-source)
  "The functions that find the producer of nodes for the current buffer.
Each function takes no arguments and returns a producer, or nil when it
does not apply. The first producer wins. A major mode adds its own function
with a buffer-local `add-hook', which runs it before the global functions.")

(defun hel-node-producer ()
  "Return the producer of nodes for the current buffer, or nil.
The first function in `hel-node-source-functions' that returns
a producer wins. The `hel-node-' operations take this producer as their
first argument."
  (run-hook-with-args-until-success 'hel-node-source-functions))

;;; .
(provide 'hel-structural-editing)
;;; hel-structural-editing.el ends here
