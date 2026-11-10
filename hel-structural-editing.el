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
(require 'hel-multiple-cursors-core)
(require 'hel-core)
(require 'hel-text-objects)

;;; Operations on nodes

(cl-defgeneric hel-node-on (producer range)
  "Return the smallest node that covers RANGE, or nil.")

(cl-defgeneric hel-node-bounds (producer node)
  "Return the (BEGIN . END) bounds of NODE.")

(cl-defgeneric hel-node-parent (producer node)
  "Return the parent of NODE, or nil at the root.")

(cl-defgeneric hel-node-children (producer node)
  "Return the children of NODE, in buffer order.")

(cl-defgeneric hel-node-sibling (producer node dir)
  "Return the sibling of NODE in direction DIR, 1 or -1, or nil.")

(cl-defgeneric hel-node-enclosing (producer pos)
  "Return the smallest node with POS inside it, not on its boundary, or nil.")

(cl-defgeneric hel-node-enclosing-leaf (producer pos)
  "Return the node without children that has POS inside it, or nil.
POS on the boundary of the node does not count.

By default, this is the enclosing node of POS when it has no
children, so a producer that defines `hel-node-enclosing' and
`hel-node-children' gets this operation without a method of its own.")

(cl-defmethod hel-node-enclosing-leaf (producer pos)
  (when-let* ((enclosing (hel-node-enclosing producer pos))
              ((null (hel-node-children producer enclosing))))
    enclosing))

(cl-defgeneric hel-node-touching (producer pos side)
  "Return the smallest node with POS on its boundary, not inside it, or nil.
With SIDE 1 the node starts at POS, and with SIDE -1 it ends there.")

(cl-defgeneric hel-node-side-child (producer pos side)
  "Return the nearest node after POS, or before POS when SIDE is -1, or nil.
The node lies on the same level as POS: inside the same brackets or parent.

By default, the children are those of the enclosing node of POS, so
a producer that defines `hel-node-enclosing' and `hel-node-children'
gets this operation without a method of its own.")

(cl-defmethod hel-node-side-child (producer pos side)
  (when-let* ((enclosing (hel-node-enclosing producer pos))
              (children (hel-node-children producer enclosing)))
    (if (< 0 side)
        (-first (lambda (child)
                  (<= pos (car (hel-node-bounds producer child))))
                children)
      (-last (lambda (child)
               (<= (cdr (hel-node-bounds producer child)) pos))
             children))))

(cl-defgeneric hel-node-expand-steps (producer range)
  "Return the ranges that expanding the selection from RANGE can select.
The ranges may come in any order: each step of the expansion selects the
smallest of them that encloses the selection.

By default, the ranges are those of the node on RANGE and of its parents,
without the root. An empty RANGE stands for the character after it.")

(cl-defmethod hel-node-expand-steps (producer range)
  (-let* (((begin . end) range)
          (node (hel-node-on producer
                             (if (= begin end)
                                 (cons begin (min (1+ end) (point-max)))
                               range)))
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
  (hel-treesit--node-bounds node))

(cl-defmethod hel-node-parent ((_producer hel-tree-treesit-producer) node)
  (treesit-node-parent node))

(cl-defmethod hel-node-children ((_producer hel-tree-treesit-producer) node)
  "Return the named children of NODE."
  (treesit-node-children node t))

(cl-defmethod hel-node-sibling ((_producer hel-tree-treesit-producer) node dir)
  "Return the named sibling of NODE in direction DIR.
When NODE has no sibling, but a parent has the same bounds as NODE,
return the sibling of that parent."
  (let ((range (hel-treesit--node-bounds node)))
    (cl-loop for same = node
             then (treesit-node-parent same)
             while (and same
                        (equal range (hel-treesit--node-bounds same)))
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
  "Return the sexp that covers RANGE.
When RANGE spans several sexps, the result runs from the start of the
first one to the end of RANGE. Return nil if no sexp is at its start."
  (-let [(begin . end) range]
    (save-excursion
      (goto-char begin)
      (ignore-errors
        (cond
         ;; A comment is a node of its own. The sexp scan would skip it.
         ((and (char-after)
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
  "Return the sexp after POS, or before POS when SIDE is negative."
  (if (< 0 side)
      (hel--sexp-after pos)
    (hel--sexp-before pos)))

(cl-defmethod hel-node-expand-steps ((_producer hel-tree-sexp-producer) range)
  "Return what expanding can select around the start of RANGE.
This is the word and the symbol there, the string, the comment and the
bracket pairs around it, the inside of the string and of the innermost
pair, the sentence in `text-mode' and the paragraph."
  (save-excursion
    (let* ((pos (car range))
           (on-word (memq (char-syntax (or (char-after pos) ?\s))
                          '(?w ?_))))
      (append (->> `(,@(if on-word '(word))
                     ,@(if (derived-mode-p 'text-mode) '(sentence))
                     paragraph)
                   (-keep (lambda (thing)
                            (when-let* ((producer (hel-producer thing)))
                              (-some->> (hel--thing-under thing producer pos 1)
                                (hel-thing-instance-bounds))))))
              (when-let* ((on-word)
                          (symbol (progn (goto-char pos)
                                         (bounds-of-thing-at-point 'symbol))))
                (list symbol))
              (hel--syntax-ppss-bounds pos)
              ;; The cursor sits on the character after `pos'. When that character
              ;; opens a bracket pair, a string or a comment, the scan from the
              ;; next position finds it.
              (if (< pos (point-max))
                  (hel--syntax-ppss-bounds (1+ pos)))))))

(cl-defmethod hel-node-expand-steps ((_producer hel-tree-lisp-producer) range)
  "Return what expanding can select around the start of RANGE.
This is the symbol, the string or the comment there, and each list around
it as a whole. The inside of a list or of a string is not a step,
because `mi(' and `mi\"' select those."
  (let ((begin (car range)))
    (cons (hel--sexp-leaf begin)
          (append (hel--bracket-sexps begin)
                  ;; The cursor sits on the character after `begin'. When
                  ;; that character opens a list, the list is a step too.
                  (if (< begin (point-max))
                      (hel--bracket-sexps (1+ begin)))))))

(defun hel--syntax-ppss-bounds (pos)
  "Return the bounds of the strings, comments and bracket pairs around POS.
A string gives its inside and its whole, and a comment gives its whole.
Each bracket pair gives its whole, and the innermost one also gives its
inside."
  (let* ((ppss (save-excursion (syntax-ppss pos)))
         (brackets (hel--bracket-bounds pos))
         (ranges brackets))
    (-when-let (((begin . end)) brackets)
      (push (hel-trim-whitespace-from-range (cons (1+ begin) (1- end)))
            ranges))
    (when-let* ((begin (nth 8 ppss)))
      (if (nth 3 ppss)
          (when-let* ((end (ignore-errors (scan-sexps begin 1))))
            (push (cons begin end) ranges)
            (push (cons (1+ begin) (1- end)) ranges))
        ;; else
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
      (cond ((nth 3 ppss)
             (when-let* ((begin (nth 8 ppss))
                         (end (ignore-errors (scan-sexps begin 1)))
                         ((< begin pos end)))
               (cons begin end)))
            ((nth 4 ppss)
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
  "Return the node under the selection, or nil without one.
The node covers the selection without the whitespace at its edges."
  (when-let* ((range (hel-trimmed-selection-bounds)))
    (hel-node-on producer range)))

(defun hel--node-touching-point (producer)
  "Return the node with point on its boundary but not inside it, or nil.
When two nodes meet at point, return the one after point."
  (unless (hel-node-enclosing-leaf producer (point))
    (or (hel-node-touching producer (point) 1)
        (hel-node-touching producer (point) -1))))

(defun hel--node-at-point (producer)
  "Return the node that a tree command starts from without a selection.
This is the node without children that has point inside it, or else
the node with point on its boundary. Return nil if there is neither."
  (or (hel-node-enclosing-leaf producer (point))
      (hel-node-touching producer (point) 1)
      (hel-node-touching producer (point) -1)))

(defun hel--sibling-start-node (producer dir)
  "Return the node that a sibling command starts from without a selection.
This is the node without children that has point inside it, or else
the nearest node in direction DIR, so that the next press goes on from
sibling to sibling."
  (or (hel-node-enclosing-leaf producer (point))
      (hel-node-side-child producer (point) dir)))

(defun hel--node-single-child-chain-end (producer node)
  "Go down from NODE while its only child has the same bounds, and
return the last node."
  (let ((range (hel-node-bounds producer node))
        children)
    (while (and (setq children (hel-node-children producer node))
                (null (cdr children))
                (equal range (hel-node-bounds producer (car children))))
      (setq node (car children)))
    node))

(defun hel--node-all-children (producer node)
  "Return the bounds of the children of NODE.
When NODE only wraps one child with the same bounds, use that child."
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

;;; Drivers

(defun hel--tree-select-first-child (producer)
  "Select the first child of the node under the selection.
Keep the direction of the selection. Without a selection, select the
first child of the node after point, forward. Return non-nil if the
selection changed."
  (-let [(range dir) (hel-selection)]
    (when-let* ((node (if range
                          (hel--node-on-selection producer)
                        (hel-node-touching producer (point) 1)))
                (child (-first-item (hel--node-all-children producer node)))
                ((not (equal child range))))
      (hel-select child (or dir 1))
      t)))

(defun hel--tree-shrink-1 (producer)
  "Shrink the selection one step. Return non-nil if it changed.
Return to the previous selection of this chain of expands. Without one,
select the first child of the node under the selection."
  (hel--sync-selection-history)
  (if hel-selection-history
      (progn
        (pop hel-selection-history)
        (hel--restore-selection (car hel-selection-history))
        ;; The oldest entry alone is not a chain any more.
        (unless (cdr hel-selection-history)
          (setq hel-selection-history nil))
        t)
    ;; else
    (hel--tree-select-first-child producer)))

(defconst hel--tree-expand-commands
  '(hel-expand-selection-backward
    hel-expand-selection-forward
    hel-shrink-selection)
  "The commands of one series of expand and shrink steps.
The first command of a series pushes the old point to the mark ring.")

(defun hel--tree-expand (producer count dir)
  "Expand the selection COUNT times to the enclosing node.
Each step selects the smallest node that holds the selection: a
tree-sitter node, or a sexp in a buffer without a parser. A negative
COUNT shrinks the selection back. DIR, 1 or -1, is the direction of
the expanded selection. PRODUCER finds the nodes."
  (if (< count 0)
      (dotimes (_ (- count))
        (hel--tree-shrink-1 producer))
    (let ((origin (point))
          changed)
      (dotimes (_ count)
        (when (hel--tree-expand-1 producer dir)
          (setq changed t)))
      (when (and changed
                 (not (memq last-command hel--tree-expand-commands)))
        (hel-push-point origin))))
  (hel-reveal-point-when-on-top))

(defun hel--tree-expand-1 (producer dir)
  "Expand the selection one step to the enclosing node.
DIR, 1 or -1, is the direction of the new selection. Without a
selection, the first step selects the node with point on its boundary.
Return non-nil if the selection changed."
  (hel--sync-selection-history)
  (let* ((before (hel--selection-before))
         (range (car before)))
    (when-let*
        ((new-range
          (or (unless (use-region-p)
                (-some->> (hel--node-touching-point producer)
                  (hel-node-bounds producer)))
              ;; The steps come from the selection where this chain
              ;; of expands began, so the inside of an outer bracket
              ;; pair never becomes a step.
              (-> (hel-node-expand-steps
                   producer
                   (-if-let ((chain-start) (-last-item hel-selection-history))
                       chain-start
                     range))
                  (hel--smallest-larger-range range)))))
      (hel-select new-range dir)
      (hel--record-selection before)
      t)))

(defun hel--node-under-cursor (producer)
  "Return the largest node inside the selection on the end where point is."
  (-let* (((begin . end) (hel-trimmed-selection-bounds))
          (forward? (< 0 (hel-region-direction)))
          (char (if forward? (1- end) begin))
          (node (hel-node-on producer (cons char (1+ char)))))
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
it keeps that node and turns around. Return non-nil if the selection
changed."
  (cl-flet ((near (range) (if (< 0 dir) (car range) (cdr range)))
            (far  (range) (if (< 0 dir) (cdr range) (car range))))
    (if (not (use-region-p))
        (when-let* ((node (hel--sibling-start-node producer dir))
                    (range (hel-node-bounds producer node)))
          (goto-char (far range))
          t)
      (when-let* ((node (hel--node-under-cursor producer))
                  (range (hel-node-bounds producer node))
                  (sibling (hel-node-sibling producer node dir))
                  (target (hel-node-bounds producer sibling)))
        (cond ((= dir (hel-region-direction))
               (goto-char (far target)))
              ;; The sibling still overlaps the selection: give back NODE.
              ((if (< 0 dir)
                   (< (near target) (region-end))
                 (< (region-beginning) (near target)))
               (goto-char (near target)))
              ;; NODE is the last one: keep it, and go on past the other end.
              (t
               (hel-select (hel-range (near range) (far target)) dir)))
        t))))

(defun hel--tree-select-sibling (producer count)
  "Select the COUNT-th sibling of the node under the selection.
A negative COUNT goes backward. At the last sibling, keep the selection.
Keep the direction of the selection. Without a selection, first select the
node at point on the side of the motion, in the direction of the motion.
With the selection extended, extend it. PRODUCER finds the nodes."
  (if hel--extend-selection
      (let ((origin (point)))
        (when (/= count
                  (hel-motion-loop (dir count)
                    (hel--tree-extend-to-sibling producer dir)))
          (hel-push-point origin)
          (hel-reveal-point-when-on-top)))
    (let* ((origin (point))
           (motion-dir (hel-sign count))
           (region? (use-region-p))
           (region-dir (if region? (hel-region-direction) motion-dir))
           (node (if region?
                     (hel--node-on-selection producer)
                   (hel--sibling-start-node producer motion-dir)))
           moved?)
      (when node
        (cl-flet ((select (node)
                    (hel-select (hel-node-bounds producer node) region-dir)))
          (unless region?
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
    (setq hel--extend-selection nil)
    (when-let* ((ranges (funcall fn)))
      (hel-select-ranges ranges (if (use-region-p)
                                     (hel-region-direction)
                                   1))))
  (hel-update-cursor))

(defun hel--tree-select-all-siblings (producer)
  "Put a cursor on the node at each cursor and on its siblings.
Without a selection, when point touches no node, use the children of the
node around point. PRODUCER finds the nodes."
  (hel--select-ranges-for-each-cursor
   (lambda ()
     (if-let* ((node (if (use-region-p)
                         (hel--node-on-selection producer)
                       (hel--node-at-point producer))))
         (hel--node-all-siblings producer node)
       (-some->> (hel-node-enclosing producer (point))
         (hel--node-all-children producer))))))

(defun hel--tree-select-all-children (producer)
  "Put a cursor on each child of the node at each cursor.
PRODUCER finds the nodes."
  (hel--select-ranges-for-each-cursor
   (lambda ()
     (when-let* ((node (if (use-region-p)
                           (hel--node-on-selection producer)
                         (or (hel--node-at-point producer)
                             (hel-node-enclosing producer (point))))))
       (hel--node-all-children producer node)))))

;;; The producer of nodes

(defvar hel-node-source-functions '(hel-tree-treesit-source
                                    hel-tree-syntax-source)
  "The functions that find the producer of nodes for the current buffer.
Each function takes no arguments and returns a producer, or nil when it
does not apply. The first producer wins. A major mode adds its own function
with a buffer-local `add-hook', which runs it before the global functions.")

(defun hel--node-producer ()
  "Return the producer of nodes for the current buffer, or nil."
  (run-hook-with-args-until-success 'hel-node-source-functions))

;;; .
(provide 'hel-structural-editing)
;;; hel-structural-editing.el ends here
