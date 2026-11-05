;;; hel-structural-editing.el --- Selection by syntax -*- lexical-binding: t -*-
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
;; The protocol and the algorithms that select by the structure of the text.
;; The commands in `hel-commands.el' call them, and every one of these commands
;; works in every buffer.
;;
;; Every command runs an algorithm over one kind of /thing/, named by
;; a symbol such as `word', `function' or `node'. As in `thingatpt' and
;; `treesit-thing-next', "thing" names both the kind and a piece of text
;; of that kind: "the next THING" with THING `word' is the next word. The
;; property `hel-thing-sources' of a thing holds a list of /sources/. A source
;; is a function of no arguments that returns a /producer/ at the cursor, or nil
;; when it does not apply there. The first source that returns a producer wins.
;;
;; Every operation of the protocol takes the thing and the producer, and
;; dispatches on the argument that its answer depends on:
;;
;; - How the things are found depends on the producer: a motion such as
;;   `hel--forward-word', a tree-sitter query, a finder of comments, the
;;   nodes of a parser, or the sexps of the syntax table.
;; - Which rules hold for a kind of thing depends on the thing: a word
;;   gap stays on its line, a paragraph covers whole lines, a function
;;   owns the comment above it.
;;
;; The finding operations have two levels. A run producer gives things one after
;; another (`hel-things-at', `hel-thing-next'). A tree producer also knows the
;; parent and the children of a node (`hel-node-parent', `hel-node-children').
;; The text object keys need a run, and the node keys need a tree.
;;
;; The algorithms are generic functions with default methods, so a thing with
;; different rules gets a method of its own: words specialize `hel-thing-edge'.
;; A command is a thin function that chooses the thing and the parameters of
;; one algorithm.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'thingatpt)
(require 'newcomment)
(require 'treesit)
(require 'pcre2el)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-multiple-cursors-core)
(require 'hel-core)

(declare-function hel-extend-selection "hel-commands" (arg) t)

;;; Common

(defun hel-treesit-active-p ()
  "Return non-nil if the current buffer has a tree-sitter parser."
  (and (treesit-available-p)
       (treesit-parser-list)
       t))

(defun hel--selection ()
  "Return the selection that the structural commands start from, or nil.
Return the (BEGIN . END) range of the region without the whitespace at its
edges. Without a region, return nil: point is a position, and each command
decides what a bare point means for it."
  (when (use-region-p)
    (let ((begin (region-beginning))
          (end   (region-end)))
      (or (hel-trim-whitespace-from-range begin end)
          (cons begin end)))))

(defun hel--cursor-position ()
  "Return the position of the character under the cursor.
This is the last character of a forward selection, and point otherwise."
  (if (and (use-region-p)
           (< (mark) (point)))
      (1- (point))
    (point)))

(cl-defun hel--nearest (items dir &optional (key #'identity))
  "Return the first of ITEMS in direction DIR, 1 or -1, or nil.
KEY maps an item to its (BEGIN . END) range; without KEY the items are
the ranges. Forward, the first item starts first; of two that start
there, the larger one. Backward, the first item ends last; of two that
end there, the larger one."
  (cond ((null items) nil)
        ((< 0 dir)
         (-min-by (lambda (a b)
                    (-let (((a-beg . a-end) (funcall key a))
                           ((b-beg . b-end) (funcall key b)))
                      (or (> a-beg b-beg)
                          (and (= a-beg b-beg)
                               (< a-end b-end)))))
                  items))
        (t
         (-max-by (lambda (a b)
                    (-let (((a-beg . a-end) (funcall key a))
                           ((b-beg . b-end) (funcall key b)))
                      (or (> a-end b-end)
                          (and (= a-end b-end) (< a-beg b-beg)))))
                  items))))

(defun hel--only-whitespace-p (begin end)
  "Return non-nil if the text from BEGIN to END is only spaces and tabs."
  (save-excursion
    (goto-char begin)
    (skip-chars-forward " \t" end)
    (<= end (point))))

(defun hel--skip-whitespace (pos dir)
  "Return the end of the whitespace from POS in direction DIR."
  (save-excursion
    (goto-char pos)
    (if (< 0 dir)
        (skip-chars-forward " \t\r\n")
      (skip-chars-backward " \t\r\n"))
    (point)))

(defun hel--skip-blanks (pos dir)
  "Return the end of the spaces and tabs from POS in direction DIR."
  (save-excursion
    (goto-char pos)
    (if (< 0 dir)
        (skip-chars-forward " \t")
      (skip-chars-backward " \t"))
    (point)))

(defun hel--line-range (begin end)
  "Return the range of the whole lines from BEGIN to END.
The range runs from the start of the line of BEGIN to the end of the line
of END, with its line end."
  (save-excursion
    (cons (progn
            (goto-char begin)
            (line-beginning-position))
          (progn
            (goto-char end)
            (min (1+ (line-end-position))
                 (point-max))))))

(defun hel--fills-lines-p (begin end)
  "Return non-nil if BEGIN..END is the only text on its lines."
  (save-excursion
    (and (hel--only-whitespace-p (progn (goto-char begin)
                                        (line-beginning-position))
                                 begin)
         (hel--only-whitespace-p end
                                 (progn (goto-char end)
                                        (line-end-position))))))

;;; Selection history

(defun hel--sync-selection-history ()
  "Clear `hel-selection-history' if the region is not its last selection."
  (unless (equal (nth 1 (car hel-selection-history))
                 (hel-region))
    (setq hel-selection-history nil)))

(defun hel--record-selection (before)
  "Push BEFORE and the active region to `hel-selection-history'.
BEFORE is the selection before the command, see `hel-selection-history'."
  (push (list before (hel-region) hel--text-object-run)
        hel-selection-history))

(defun hel--restore-selection (before)
  "Restore the selection BEFORE from `hel-selection-history'."
  (-let [(begin _end dir) before]
    (if dir
        (hel-restore-region before)
      (deactivate-mark)
      (goto-char begin))))

(defun hel--selection-before ()
  "Return the selection before a command, in the form of `hel-region'.
Without a region, return (POINT POINT nil)."
  (or (hel-region)
      (list (point) (point) nil)))

;;; The protocol
;;;; Things and sources

(defun hel--producer (thing)
  "Return the producer of THING at the cursor, or nil.
The producer comes from the first source in the property
`hel-thing-sources' of THING that returns one."
  (-some #'funcall (get thing 'hel-thing-sources)))

(defun hel--no-producer-message (thing)
  "Report that no source of THING applies at the cursor."
  (message "No %s here" thing))

;;;; The run level
;;
;; A found thing is a plist with these properties:
;;
;;   :begin :end  the range of the thing;
;;   :parent      the parent that the producer knows: a tree-sitter node,
;;                a position, or nil.
;;
;; A position POS stands for the character after it, as the cursor does.

(cl-defgeneric hel-things-at (thing producer begin end)
  "Return the THINGs that PRODUCER finds around BEGIN..END.
These are the things that contain BEGIN..END, and the neighbours whose
gap or header comment may hold it.")

(cl-defgeneric hel-thing-next (thing producer pos dir)
  "Return the nearest THING from POS in direction DIR, or nil.
PRODUCER finds the things. Forward, the thing starts after the
character at POS. Backward, it ends at or before POS. Of two things
that start, or end, at the same place, return the larger one.")

(cl-defgeneric hel-thing-skip-separator (thing producer pos dir)
  "Return the far edge of the separator at POS in direction DIR, or nil.
A separator lies between two THINGs and belongs to neither, as
the comma between two arguments. PRODUCER finds the things.")

(cl-defmethod hel-thing-skip-separator (_thing _producer _pos _dir)
  "Return nil: the things have no separators."
  nil)

(cl-defgeneric hel-thing-sibling (thing producer instance dir &optional limit)
  "Return the next THING after INSTANCE in direction DIR, or nil.
The result has the run parent of INSTANCE. With LIMIT, the result starts
at or before LIMIT forward, and ends at or after LIMIT backward.")

(cl-defmethod hel-thing-sibling (thing producer instance dir &optional limit)
  "Return the next thing from the edge of INSTANCE, when its parent matches."
  (when-let* ((next (hel-thing-next thing producer
                                    (if (< 0 dir)
                                        (1- (plist-get instance :end))
                                      (plist-get instance :begin))
                                    dir))
              ((hel--same-parent-p (hel-thing-run-parent thing producer instance)
                                   (hel-thing-run-parent thing producer next)))
              ((or (not limit)
                   (if (< dir 0)
                       (<= limit (plist-get next :end))
                     (<= (plist-get next :begin) limit)))))
    next))

(defun hel--thing-range (instance)
  "Return the (BEGIN . END) range of INSTANCE."
  (cons (plist-get instance :begin) (plist-get instance :end)))

(defun hel--same-parent-p (a b)
  "Return non-nil if A and B are the same run parent."
  (if (treesit-node-p a)
      (and (treesit-node-p b)
           (treesit-node-eq a b))
    (equal a b)))

;;;; The tree level
;;
;; A node is whatever the producer uses for it: a tree-sitter node, or the
;; (BEGIN . END) range of a sexp. `hel-node-range' turns it into a range.

(cl-defgeneric hel-node-at (thing producer begin end)
  "Return the smallest node of THING that covers BEGIN..END, or nil.")

(cl-defgeneric hel-node-range (thing producer node)
  "Return the (BEGIN . END) range of NODE.")

(cl-defgeneric hel-node-parent (thing producer node)
  "Return the parent of NODE, or nil at the root.")

(cl-defgeneric hel-node-children (thing producer node)
  "Return the children of NODE, in buffer order.")

(cl-defgeneric hel-node-sibling (thing producer node dir)
  "Return the sibling of NODE in direction DIR, 1 or -1, or nil.")

(cl-defgeneric hel-node-enclosing (thing producer pos)
  "Return the smallest node that contains POS strictly, or nil.
POS lies inside the node and not at its edge. No child of the node
contains POS strictly.")

(cl-defgeneric hel-node-token (thing producer pos)
  "Return the token with POS inside it, or nil.
A token is a node without children. POS lies inside it, not at its edge.")

(cl-defmethod hel-node-token (thing producer pos)
  "Return the enclosing node of POS when it has no children."
  (when-let* ((enclosing (hel-node-enclosing thing producer pos))
              ((null (hel-node-children thing producer enclosing))))
    enclosing))

(cl-defgeneric hel-node-touching (thing producer pos side)
  "Return the smallest node that touches POS on SIDE, or nil.
With SIDE 1 the node starts at POS, and with SIDE -1 it ends there.")

(cl-defgeneric hel-node-side-child (thing producer pos dir)
  "Return the child of the node around POS on the side DIR of POS, or nil.
Forward, this is the first child that starts at or after POS. Backward,
it is the last child that ends at or before POS.")

(cl-defmethod hel-node-side-child (thing producer pos dir)
  "Return the child from the children of the enclosing node of POS."
  (when-let* ((enclosing (hel-node-enclosing thing producer pos))
              (children (hel-node-children thing producer enclosing)))
    (if (< 0 dir)
        (-first (lambda (child)
                  (<= pos (car (hel-node-range thing producer child))))
                children)
      (-last (lambda (child)
               (<= (cdr (hel-node-range thing producer child)) pos))
             children))))

(cl-defgeneric hel-node-select-side (thing producer count)
  "Select toward the side of COUNT from the node of THING at the selection.
This is the command of `H' and `L'.")

(cl-defmethod hel-node-select-side (thing _producer count)
  "Select to the edge of the parent, as `M-b' and `M-e' do."
  (hel--select-to-parent-edge thing count))

(cl-defgeneric hel-node-expand-steps (thing producer begin end)
  "Return the regions that expand steps through from BEGIN..END.
The regions are (BEGIN . END) ranges, in any order.")

(cl-defmethod hel-node-expand-steps (thing producer begin end)
  "Return the ranges of the node at BEGIN..END and of its parents.
The root is not among them. An empty BEGIN..END stands for the character
after it."
  (let ((node (hel-node-at thing producer begin
                           (if (= begin end)
                               (min (1+ end) (point-max))
                             end)))
        regions)
    (while-let ((node)
                (parent (hel-node-parent thing producer node)))
      (push (hel-node-range thing producer node) regions)
      (setq node parent))
    regions))

;;;; Rules about a kind of thing

(cl-defgeneric hel-thing-header-p (thing)
  "Return non-nil if the comment lines above a THING belong to it.")

(cl-defmethod hel-thing-header-p (_thing)
  "Return nil: the things have no header comment."
  nil)

(cl-defmethod hel-thing-header-p ((_thing (eql 'function)))
  "Return t: a function owns the comment above it."
  t)

(cl-defmethod hel-thing-header-p ((_thing (eql 'class)))
  "Return t: a class owns the comment above it."
  t)

(cl-defmethod hel-thing-header-p ((_thing (eql 'argument)))
  "Return t: an argument on its own line owns the comment above it."
  t)

(cl-defgeneric hel-thing-linewise-p (thing)
  "Return non-nil if a THING covers whole lines.
Such a thing runs from the start of its first line to the end of its
last line, with the line end.")

(cl-defmethod hel-thing-linewise-p (_thing)
  "Return nil."
  nil)

(cl-defmethod hel-thing-linewise-p ((_thing (eql 'paragraph)))
  "Return t: a paragraph covers whole lines."
  t)

(cl-defgeneric hel-thing-skip-gap (thing producer pos dir)
  "Return the position where the gap from POS in direction DIR ends.
The gap separates two THING's that PRODUCER finds.")

(cl-defmethod hel-thing-skip-gap (thing producer pos dir)
  "Return the end of the whitespace and the separators from POS in DIR."
  (let (next)
    (while (progn
             (setq pos (hel--skip-whitespace pos dir))
             (setq next (hel-thing-skip-separator thing producer pos dir)))
      (setq pos next))
    pos))

(cl-defmethod hel-thing-skip-gap ((_thing (eql 'word)) _producer pos dir)
  "Return the end of the spaces and tabs from POS in DIR.
A word gap stays on its line."
  (hel--skip-blanks pos dir))

(cl-defmethod hel-thing-skip-gap ((_thing (eql 'WORD)) _producer pos dir)
  "Return the end of the spaces and tabs from POS in DIR.
A WORD gap stays on its line."
  (hel--skip-blanks pos dir))

(cl-defmethod hel-thing-skip-gap ((_thing (eql 'comment)) _producer pos dir)
  "Return the end of the gap of a comment run from POS in DIR.
At the start of a line, the edge of a run of whole comment lines, the gap
is the blank lines. Elsewhere, at the edge of a comment after code, the
gap is the spaces and tabs on the line, so it never joins the line of
code to the next line."
  (if (save-excursion (goto-char pos) (bolp))
      (hel--skip-whitespace pos dir)
    (hel--skip-blanks pos dir)))

(cl-defgeneric hel-thing-run-parent (thing producer instance)
  "Return the parent that a run of THINGs stays inside.
INSTANCE is the first thing of the run, and PRODUCER found it. Two
things with equal parents are siblings. A nil parent is the whole
buffer.")

(cl-defmethod hel-thing-run-parent (_thing _producer instance)
  "Return the parent that the producer of INSTANCE knows."
  (plist-get instance :parent))

(cl-defmethod hel-thing-run-parent ((_thing (eql 'sentence)) _producer instance)
  "Return the start of the paragraph of INSTANCE: a sentence stays in it."
  (save-excursion
    (goto-char (min (1+ (plist-get instance :begin)) (point-max)))
    (hel--forward-paragraph -1)
    (point)))

(cl-defgeneric hel-thing-jump-p (thing)
  "Return non-nil if a motion over THINGs is a jump.
A jump pushes the old point to the mark ring.")

(cl-defmethod hel-thing-jump-p (_thing)
  "Return t."
  t)

(cl-defmethod hel-thing-jump-p ((_thing (eql 'word)))
  "Return nil: word motions run on most keystrokes."
  nil)

(cl-defmethod hel-thing-jump-p ((_thing (eql 'WORD)))
  "Return nil: WORD motions run on most keystrokes."
  nil)

;;; Sexps

(defun hel--sexp-start (end)
  "Return the start of the sexp that ends at END, with its prefix characters."
  (save-excursion
    (goto-char (scan-sexps end -1))
    (backward-prefix-chars)
    (point)))

(defun hel--sexp-at (begin end)
  "Return the range of the sexp that covers BEGIN..END, as (BEGIN . END).
When BEGIN..END spans several sexps, the range runs from the start of the
first one to END. Return nil if no sexp is at BEGIN."
  (save-excursion
    (goto-char begin)
    (ignore-errors
      (cond
       ;; A comment is a node of its own. The sexp scan would skip it.
       ((and (char-after)
             (not (hel-whitespace-p (char-after)))
             (forward-comment 1))
        (cons begin (max end (cdr (hel-trim-whitespace-from-range begin (point))))))
       ((and (char-after)
             (eq ?\) (char-syntax (char-after))))
        (cons (scan-sexps (1+ begin) -1) (1+ begin)))
       (t
        (when (memq (char-syntax (char-after)) '(?w ?_))
          (skip-syntax-backward "w_"))
        (when-let* ((stop (scan-sexps (point) 1))
                    (start (hel--sexp-start stop))
                    ((< start end))
                    ((< begin stop)))
          (cons start (max stop end))))))))

(defun hel--sexp-sibling (sexp dir)
  "Return the range of the sexp after SEXP in direction DIR, 1 or -1.
The sibling lies inside the same bracket pair. Return nil if there is
none. SEXP is a (BEGIN . END) range."
  (ignore-errors
    (-let [(begin . end) sexp]
      (if (< 0 dir)
          (when-let* ((stop (scan-sexps end 1))
                      (start (hel--sexp-start stop))
                      ((<= end start)))
            (cons start stop))
        (when-let* ((start (scan-sexps begin -1))
                    (stop (scan-sexps start 1))
                    ((<= stop begin)))
          (cons (hel--sexp-start stop) stop))))))

(defun hel--sexps-in (begin end)
  "Return the ranges of the sexps from BEGIN to END, in buffer order."
  (let ((pos begin)
        sexps)
    (while-let ((stop (ignore-errors (scan-sexps pos 1)))
                ((<= stop end)))
      (push (cons (hel--sexp-start stop) stop) sexps)
      (setq pos stop))
    (nreverse sexps)))

(defun hel--bracket-pairs (pos)
  "Return the bracket pairs that enclose POS, the innermost first.
Each pair is the (OPEN . CLOSE) range of the whole bracketed form."
  (->> (nth 9 (save-excursion (syntax-ppss pos)))
       (reverse)
       (-keep (lambda (open)
                (if-let* ((close (ignore-errors (scan-lists open 1 0))))
                    (cons open close))))))

(defun hel--bracket-parent (pos)
  "Return the start of the innermost bracket pair around POS, or nil."
  (nth 1 (save-excursion (syntax-ppss pos))))

(defun hel--sexp-token (pos)
  "Return the range of the sexp, string or comment with POS inside it, or nil.
POS lies inside it and not at its edge. Point lies inside a sexp when
the sexp after it starts before it, as in a symbol or after the quote of
'sym."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (cond ((nth 3 ppss)
             (when-let* ((start (nth 8 ppss))
                         (end (ignore-errors (scan-sexps start 1)))
                         ((< start pos end)))
               (cons start end)))
            ((nth 4 ppss)
             (goto-char pos)
             (when-let* ((comment (hel-bounds-of-comment-at-point-ppss))
                         ((cdr comment))
                         ((< (car comment) pos)))
               (hel-trim-whitespace-from-range (car comment) (cdr comment))))
            (t
             (when-let* ((end (ignore-errors (scan-sexps pos 1)))
                         (start (hel--sexp-start end))
                         ((< start pos)))
               (cons start end)))))))

(defun hel--sexp-after (pos)
  "Return the range of the first sexp that starts at or after POS, or nil.
The sexp lies in the bracket pair around POS."
  (when-let* ((end (ignore-errors (scan-sexps pos 1)))
              (start (hel--sexp-start end))
              ((<= pos start)))
    (cons start end)))

(defun hel--sexp-before (pos)
  "Return the range of the last sexp that ends at or before POS, or nil.
The sexp lies in the bracket pair around POS."
  (when-let* ((start (ignore-errors (scan-sexps pos -1)))
              (end (ignore-errors (scan-sexps start 1)))
              ((<= end pos)))
    (cons (hel--sexp-start end) end)))

(defun hel--whole-sexp (pair)
  "Return the range of the bracket PAIR with its prefix characters."
  (cons (hel--sexp-start (cdr pair)) (cdr pair)))

;;; Node producers
;;;; Tree-sitter

(cl-defstruct (hel-treesit-node-producer
               (:constructor hel-treesit-node-producer-create)
               (:copier nil))
  "The producer of the named nodes of the tree-sitter parser.")

(defun hel-treesit-node-source ()
  "Return the producer of tree-sitter nodes, or nil without a parser."
  (when (hel-treesit-active-p)
    (hel-treesit-node-producer-create)))

(defun hel-treesit--node-region (node)
  "Return (BEGIN . END) positions of NODE."
  (cons (treesit-node-start node)
        (treesit-node-end node)))

(defun hel-treesit--node-length (node)
  "Return the length of NODE."
  (- (treesit-node-end node)
     (treesit-node-start node)))

(defun hel-treesit--enclosing-node (pos)
  "Return the smallest named node that contains POS strictly, or nil."
  (cl-loop for node = (treesit-node-on pos pos nil t)
           then (treesit-node-parent node)
           while node
           when (and (treesit-node-check node 'named)
                     (< (treesit-node-start node) pos (treesit-node-end node)))
           return node))

(cl-defmethod hel-node-at (_thing (_producer hel-treesit-node-producer) begin end)
  "Return the smallest named node that covers BEGIN..END."
  (treesit-node-on begin end nil t))

(cl-defmethod hel-node-range (_thing (_producer hel-treesit-node-producer) node)
  "Return the range of NODE."
  (hel-treesit--node-region node))

(cl-defmethod hel-node-parent (_thing (_producer hel-treesit-node-producer) node)
  "Return the parent of NODE."
  (treesit-node-parent node))

(cl-defmethod hel-node-children (_thing (_producer hel-treesit-node-producer) node)
  "Return the named children of NODE."
  (treesit-node-children node t))

(cl-defmethod hel-node-sibling (_thing (_producer hel-treesit-node-producer) node dir)
  "Return the named sibling of NODE in direction DIR.
A parent with the same range as NODE counts as NODE, so when NODE has no
sibling there, return the sibling of such a parent."
  (let ((region (hel-treesit--node-region node)))
    (cl-loop for same = node then (treesit-node-parent same)
             while (and same
                        (equal region (hel-treesit--node-region same)))
             thereis (if (< 0 dir)
                         (treesit-node-next-sibling same t)
                       (treesit-node-prev-sibling same t)))))

(cl-defmethod hel-node-enclosing (_thing (_producer hel-treesit-node-producer) pos)
  "Return the smallest named node that contains POS strictly."
  (hel-treesit--enclosing-node pos))

(cl-defmethod hel-node-touching (_thing (_producer hel-treesit-node-producer)
                                        pos side)
  "Return the smallest named node that starts or ends at POS on SIDE."
  (when-let* ((char (if (< 0 side) pos (1- pos)))
              ((<= (point-min) char))
              ((< char (point-max)))
              (node (treesit-node-on char (1+ char) nil t))
              ((= pos (if (< 0 side)
                          (treesit-node-start node)
                        (treesit-node-end node)))))
    node))

;;;; Sexps

(cl-defstruct (hel-sexp-node-producer
               (:constructor hel-sexp-node-producer-create)
               (:copier nil))
  "The producer of the sexps of the syntax table as nodes.
The parent of a sexp is the innermost bracket pair around it, and the
parent of a top-level sexp is the whole buffer. The expand steps come from
the syntax table: words, strings, comments, the insides of bracket pairs,
sentences and paragraphs.")

(cl-defstruct (hel-lisp-node-producer
               (:include hel-sexp-node-producer)
               (:constructor hel-lisp-node-producer-create)
               (:copier nil))
  "The producer of the sexps of a Lisp buffer as nodes.
In Lisp the code is the syntax tree, so the expand steps are the sexps
alone, as if a parser gave them.")

(defun hel-lisp-node-source ()
  "Return the producer of Lisp sexps in a Lisp buffer, or nil."
  (when (apply #'derived-mode-p hel-lisp-modes)
    (hel-lisp-node-producer-create)))

(defun hel-syntax-node-source ()
  "Return the producer of the sexps of the syntax table."
  (hel-sexp-node-producer-create))

(defun hel--buffer-node ()
  "Return the range of the whole buffer, the root of the sexps."
  (cons (point-min) (point-max)))

(cl-defmethod hel-node-at (_thing (_producer hel-sexp-node-producer) begin end)
  "Return the sexp that covers BEGIN..END."
  (hel--sexp-at begin end))

(cl-defmethod hel-node-range (_thing (_producer hel-sexp-node-producer) node)
  "Return NODE: a sexp is its range."
  node)

(cl-defmethod hel-node-parent (_thing (_producer hel-sexp-node-producer) node)
  "Return the innermost bracket pair around NODE, or the whole buffer."
  (if-let* ((pair (car (hel--bracket-pairs (car node)))))
      (hel--whole-sexp pair)
    (unless (equal node (hel--buffer-node))
      (hel--buffer-node))))

(cl-defmethod hel-node-children (_thing (_producer hel-sexp-node-producer) node)
  "Return the sexps inside NODE.
The children of a bracketed form are the sexps between its brackets. The
children of other ranges are the sexps in them, except the range itself."
  (-let* (((begin . end) node)
          (open (ignore-errors (scan-sexps end -1))))
    (if (and open
             (eq ?\( (char-syntax (char-after open)))
             (eql begin (ignore-errors (hel--sexp-start end))))
        (hel--sexps-in (1+ open) (1- end))
      (-remove (lambda (sexp) (equal sexp node))
               (hel--sexps-in begin end)))))

(cl-defmethod hel-node-sibling (_thing (_producer hel-sexp-node-producer) node dir)
  "Return the sexp next to NODE inside the same bracket pair."
  (hel--sexp-sibling node dir))

(cl-defmethod hel-node-enclosing (_thing (_producer hel-sexp-node-producer) pos)
  "Return the innermost bracket pair around POS, or the whole buffer."
  (if-let* ((pair (car (hel--bracket-pairs pos))))
      (hel--whole-sexp pair)
    (when (< (point-min) pos (point-max))
      (hel--buffer-node))))

(cl-defmethod hel-node-token (_thing (_producer hel-sexp-node-producer) pos)
  "Return the symbol, string or comment with POS inside it."
  (hel--sexp-token pos))

(cl-defmethod hel-node-touching (_thing (_producer hel-sexp-node-producer) pos side)
  "Return the sexp that starts at POS on side 1, or ends there on side -1."
  (if (< 0 side)
      (when-let* ((sexp (hel--sexp-after pos))
                  ((= pos (car sexp))))
        sexp)
    (when-let* ((sexp (hel--sexp-before pos))
                ((= pos (cdr sexp))))
      sexp)))

(cl-defmethod hel-node-side-child (_thing (_producer hel-sexp-node-producer) pos dir)
  "Return the sexp after POS, or before POS when DIR is negative."
  (if (< 0 dir)
      (hel--sexp-after pos)
    (hel--sexp-before pos)))

(cl-defmethod hel-node-expand-steps (_thing (_producer hel-sexp-node-producer)
                                            begin _end)
  "Return the regions of the syntax table around BEGIN."
  (hel--syntax-regions begin))

(cl-defmethod hel-node-select-side (thing (_producer hel-lisp-node-producer) count)
  "Select the enclosing sexp, with point on the side of COUNT.
A list has its brackets as its edges, so the edge of the parent and the
parent itself are one step."
  (hel--expand thing (abs count))
  (when (use-region-p)
    (hel-ensure-region-direction (hel-sign count))))

(cl-defmethod hel-node-expand-steps (_thing (_producer hel-lisp-node-producer)
                                            begin _end)
  "Return the regions of the sexps around BEGIN.
These are the atom, the string or the comment with BEGIN inside it, and
each list around BEGIN as a whole sexp, with its prefix characters. No
region is the inside of a list or of a string: `mi(' and `mi\"' select
those."
  (cons (hel--sexp-token begin)
        (-map #'hel--whole-sexp (hel--bracket-pairs begin))))

(defun hel--syntax-regions (pos)
  "Return the regions that the syntax table gives around POS.
The result is a list of (BEGIN . END) pairs. It holds the word and the
symbol at POS, the inside and the whole of each string, comment and
bracket pair that encloses POS or starts at POS, the sentence in
`text-mode' and the paragraph. The word, the sentence and the paragraph
come from the things of these names."
  (save-excursion
    (let ((on-word (memq (char-syntax (or (char-after pos) ?\s)) '(?w ?_))))
      (append
       (->> `(,@(if on-word '(word))
              ,@(if (derived-mode-p 'text-mode) '(sentence))
              paragraph)
            (-keep (lambda (thing)
                     (when-let* ((producer (hel--producer thing)))
                       (-some->> (hel--thing-under thing producer pos 1)
                         (hel--thing-range))))))
       (when-let* ((on-word)
                   (symbol (progn (goto-char pos)
                                  (bounds-of-thing-at-point 'symbol))))
         (list symbol))
       (hel--syntax-ppss-regions pos)
       ;; The cursor sits on the character after POS. When that character
       ;; opens a bracket pair, a string or a comment, the scan from the
       ;; next position finds it.
       (if (< pos (point-max))
           (hel--syntax-ppss-regions (1+ pos)))))))

(defun hel--syntax-ppss-regions (pos)
  "Return the strings, comments and bracket pairs that enclose POS.
Each of them gives two (BEGIN . END) pairs: its inside and its whole."
  (let ((ppss (save-excursion (syntax-ppss pos)))
        regions)
    (-each (hel--bracket-pairs pos)
      (-lambda ((open . close))
        (push (cons open close) regions)
        (push (hel-trim-whitespace-from-range (1+ open) (1- close))
              regions)))
    (when-let* ((start (nth 8 ppss)))
      (if (nth 3 ppss)
          (when-let* ((end (ignore-errors (scan-sexps start 1))))
            (push (cons start end) regions)
            (push (cons (1+ start) (1- end)) regions))
        (when-let* ((end (cdr (save-excursion
                                (goto-char pos)
                                (hel-bounds-of-comment-at-point-ppss)))))
          (setq regions (append (hel--comment-regions start end) regions)))))
    (delq nil regions)))

(defun hel--comment-regions (start end)
  "Return the whole and the inside of the comment from START to END.
The whole excludes the trailing whitespace and newline. The inside also
excludes the comment delimiters, and is nil when nothing remains."
  (comment-normalize-vars t)
  (save-excursion
    (let* ((end (progn
                  (goto-char end)
                  (skip-chars-backward " \t\r\n" start)
                  (point)))
           (inner-begin (progn
                          (goto-char start)
                          (if (and comment-start-skip
                                   (looking-at comment-start-skip))
                              (match-end 0)
                            start)))
           (inner-end (progn
                        (goto-char end)
                        (if (and comment-end-skip
                                 (not (string-empty-p comment-end))
                                 (looking-back comment-end-skip inner-begin))
                            (match-beginning 0)
                          end))))
      (list (cons start end)
            (hel-trim-whitespace-from-range inner-begin inner-end)))))

;;; Run producers
;;;; The queries

(defvar hel-treesit-queries-directory
  (->> (or load-file-name
           (bound-and-true-p byte-compile-current-file)
           buffer-file-name)
       (file-name-directory)
       (expand-file-name "queries/"))
  "The directory that holds the text object queries.
It holds a directory for each language, named after the language of its
tree-sitter parser. That directory holds the Helix file `textobjects.scm',
and may hold Hel's own file `hel-textobjects.scm'.")

(defvar hel-treesit--queries (make-hash-table :test #'eq)
  "The compiled text object queries, by language.
A language without a query file or without valid patterns maps to nil.")

(defconst hel-treesit--query-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (dolist (char '(?@ ?. ?_ ?- ?! ?? ?# ?+ ?* ?:))
      (modify-syntax-entry char "_" table))
    table)
  "The syntax table of tree-sitter query files.")

(defconst hel-treesit--grouped-capture-p
  (and (fboundp 'treesit-query-capture)
       (<= 6 (cdr (func-arity 'treesit-query-capture))))
  "Non-nil if `treesit-query-capture' takes the GROUPED argument.")

(defun hel-treesit-text-objects-p ()
  "Return non-nil if the tree-sitter text objects work at the cursor.
They need a parser, a query for the language at the cursor, and the GROUPED
argument of `treesit-query-capture', which exists in Emacs 31 and later."
  (and (hel-treesit-active-p)
       hel-treesit--grouped-capture-p
       (hel-treesit--query (treesit-language-at (hel--cursor-position)))
       t))

(defun hel-treesit--file-contents (file)
  "Return the contents of FILE, or nil if FILE is not readable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun hel-treesit--query-source (name)
  "Return the text of the query of NAME with the files that it inherits.
The query is the Helix file \"textobjects.scm\" of NAME, after the files
that it inherits, and then Hel's own file \"hel-textobjects.scm\" of NAME.
Return nil if NAME has neither file."
  (let* ((dir (expand-file-name (format "%s/" name) hel-treesit-queries-directory))
         (helix (hel-treesit--file-contents (expand-file-name "textobjects.scm" dir)))
         (own (hel-treesit--file-contents (expand-file-name "hel-textobjects.scm" dir))))
    (when (or helix own)
      (let ((inherited (when (and helix
                                  (string-match "^;+ *inherits: *\\(.*\\)$" helix))
                         (split-string (match-string 1 helix) "," t "[ \t]+"))))
        (concat (mapconcat (lambda (parent)
                             (or (hel-treesit--query-source parent) ""))
                           inherited "\n")
                "\n" helix "\n" own)))))

(defun hel-treesit--query-patterns (source)
  "Split the query SOURCE into its top-level patterns, a list of strings.
A pattern keeps the quantifiers and the captures that follow it."
  (with-temp-buffer
    (set-syntax-table hel-treesit--query-syntax-table)
    (insert source)
    (goto-char (point-min))
    (let (patterns)
      (while (progn
               (forward-comment (buffer-size))
               (not (eobp)))
        (let ((start (point))
              (end (progn (forward-sexp) (point))))
          (while (progn
                   (skip-chars-forward " \t\n")
                   (memq (char-after) '(?@ ?+ ?* ??)))
            (forward-sexp)
            (setq end (point)))
          (push (buffer-substring-no-properties start end)
                patterns)))
      (nreverse patterns))))

(defun hel-treesit--emacs-regexps (pattern)
  "Return PATTERN with the regex of each `#match?' in Emacs syntax.
The Helix files write these regexes in the syntax of the Rust `regex'
crate, and Emacs matches them as Emacs regexes. A regex that
`rxt-pcre-to-elisp' cannot translate stays as it is."
  (with-temp-buffer
    (set-syntax-table hel-treesit--query-syntax-table)
    (insert pattern)
    (goto-char (point-min))
    (while (re-search-forward "#\\(?:not-\\)?match\\?" nil t)
      (forward-sexp)                    ; the capture
      (skip-chars-forward " \t\n")
      (when (eq (char-after) ?\")
        (let* ((start (point))
               (regex (read (current-buffer)))
               (emacs (ignore-errors (rxt-pcre-to-elisp regex))))
          (when emacs
            (delete-region start (point))
            (insert (prin1-to-string emacs))))))
    (buffer-string)))

(defun hel-treesit--query (language)
  "Return the compiled text object query of LANGUAGE, or nil.
Each top-level pattern compiles on its own first, and the query holds
the patterns that compile. A pattern fails when it names a node type or
a field that the installed grammar lacks, and a single such pattern
would make the whole query fail. A query file that cannot be split into
patterns gives no query, and a message names the language."
  (pcase (gethash language hel-treesit--queries 'unknown)
    ('unknown
     (let ((query (condition-case err
                      (when-let*
                          ((patterns
                            (-some->> (hel-treesit--query-source language)
                              (hel-treesit--query-patterns)
                              (-map #'hel-treesit--emacs-regexps)
                              (-filter (lambda (pattern)
                                         (ignore-errors
                                           (treesit-query-compile language pattern t)))))))
                        (treesit-query-compile language (string-join patterns "\n") t))
                    (error
                     (message "Hel: no text objects for %s: %s"
                              language (error-message-string err))
                     nil))))
       (puthash language query hel-treesit--queries)))
    (query query)))

(defun hel-treesit--match (match around inside)
  "Return the plist of MATCH, or nil if it has no capture of the thing.
MATCH is a list of (CAPTURE . NODE) pairs. AROUND and INSIDE are the
capture names of the thing. The plist holds:

  :nodes    the nodes of those captures: the named ones in a match of the
            Helix patterns, and all of them in a match of Hel's own
            patterns, which capture exactly the range of the thing;
  :node     the largest of these nodes;
  :extends  the node of the `_extends' capture of Hel's own patterns."
  (cl-flet ((nodes (name)
              (-keep (-lambda ((capture . node))
                       (if (eq capture name) node))
                     match)))
    (when-let* ((nodes (or (nodes around) (nodes inside))))
      (let* ((extends (car (nodes '_extends)))
             (kept (or (and (not extends)
                            (-filter (lambda (node) (treesit-node-check node 'named))
                                     nodes))
                       nodes)))
        (list :nodes kept
              :node (-max-by (-on #'> #'hel-treesit--node-length)
                             kept)
              :extends extends)))))

(defun hel-treesit--thing-matches (capture begin end)
  "Return the matches of CAPTURE that have nodes in BEGIN..END.
CAPTURE is a capture name of the Helix queries without its suffix, such as
\"function\". Each match is a plist from `hel-treesit--match'.

A match of Hel's own patterns replaces the match whose node is its
`_extends' node. That match may come from a Helix pattern or from another
of Hel's own patterns, so the replacements chain: an `export' statement
replaces a `const' declaration, which replaces the arrow function in it.
A match never replaces itself: in a pattern of siblings, such as a struct
and the `;' after it, the node of the match is its own `_extends' node."
  (when-let* ((node (treesit-node-on begin end))
              (parser (treesit-node-parser node))
              (query (hel-treesit--query (treesit-parser-language parser))))
    (let* ((around (intern (concat capture ".around")))
           (inside (intern (concat capture ".inside")))
           ;; The sixth argument — GROUPED — exists only in Emacs 31 and later.
           ;; `apply' keeps the byte compiler on Emacs 29 and 30 from warning
           ;; about the argument count.
           (matches (->> (apply #'treesit-query-capture
                                (treesit-parser-root-node parser)
                                query (list begin end nil t))
                         (-keep (lambda (match)
                                  (hel-treesit--match match around inside))))))
      (-remove (lambda (match)
                 (-any? (lambda (other)
                          (and (not (eq other match))
                               (plist-get other :extends)
                               (treesit-node-eq (plist-get other :extends)
                                                (plist-get match :node))))
                        matches))
               matches))))

(defun hel-treesit--match-thing (match)
  "Return the thing of MATCH from `hel-treesit--thing-matches'.
The thing spans the kept nodes of the match, so it leaves out the
anonymous nodes at the edges of a Helix match, such as the trailing comma
of an argument."
  (let ((nodes (plist-get match :nodes)))
    (list :begin (-min (-map #'treesit-node-start nodes))
          :end (-max (-map #'treesit-node-end nodes))
          :parent (treesit-node-parent (plist-get match :node)))))

(defun hel-treesit--capture-things (capture begin end)
  "Return the things of CAPTURE that have nodes in BEGIN..END."
  (-map #'hel-treesit--match-thing
        (hel-treesit--thing-matches capture begin end)))

(defvar hel-treesit--search-window 4096
  "The size in characters of the first range that a text object search scans.")

(defun hel-treesit--search-in-windows (fn pos limit)
  "Call FN on growing ranges between POS and LIMIT, return its first value.
FN takes the BEGIN and END of a range. It returns the thing nearest to
POS among the things whose edge that faces POS lies in the range, or nil.
The first range spans `hel-treesit--search-window' characters from POS
toward LIMIT, and each next range is twice as long. The search stops at
the first non-nil value, or after the range that reaches LIMIT.

The value is the nearest thing in all of POS..LIMIT. A thing whose
facing edge lies past the range end is farther from POS than any thing
that FN returns for the range."
  (let ((size hel-treesit--search-window)
        result)
    (while (let ((far (if (<= pos limit)
                          (min limit (+ pos size))
                        (max limit (- pos size)))))
             (setq result (funcall fn (min pos far) (max pos far))
                   size (* 2 size))
             (and (not result)
                  (/= far limit))))
    result))

(defun hel-treesit--separator-end (pos dir)
  "Return the far edge of the separator node at POS in direction DIR, or nil.
A separator is an anonymous node of punctuation characters, such as
a comma."
  (let ((char (if (< 0 dir) pos (1- pos))))
    (when-let* (((<= (point-min) char))
                ((< char (point-max)))
                (node (treesit-node-at char))
                ((not (treesit-node-check node 'named)))
                (begin (treesit-node-start node))
                (end (treesit-node-end node))
                ((<= begin char))
                ((< char end))
                ((save-excursion
                   (goto-char begin)
                   (skip-syntax-forward "." end)
                   (= end (point)))))
      (if (< 0 dir) end begin))))

(defun hel-treesit--skip-gap (pos dir &optional comments)
  "Return the end of the whitespace and separators from POS in DIR.
With COMMENTS non-nil, also skip comments, and start with the comment
that holds POS."
  (save-excursion
    (goto-char pos)
    (when-let* ((comments)
                (ppss (syntax-ppss))
                ((nth 4 ppss)))
      (goto-char (nth 8 ppss))
      (when (< 0 dir)
        (forward-comment 1)))
    (while (let ((old (point)))
             (goto-char (hel--skip-whitespace (point) dir))
             (when-let* ((end (hel-treesit--separator-end (point) dir)))
               (goto-char end))
             (when comments
               (forward-comment dir))
             (/= old (point))))
    (point)))

;;;; The query producer

(cl-defstruct (hel-treesit-producer (:constructor hel-treesit-producer-create)
                                    (:copier nil))
  "The producer of the things of a capture name of the Helix queries.
It is a run producer: the parent of a thing is the parent node of its
match."
  (capture nil :documentation "The capture name without its suffix, such as
\"function\"."))

(defun hel-treesit-source (capture)
  "Return the source of the things of CAPTURE in the Helix queries.
The source applies where `hel-treesit-text-objects-p' returns non-nil."
  (lambda ()
    (when (hel-treesit-text-objects-p)
      (hel-treesit-producer-create :capture capture))))

(cl-defmethod hel-things-at (_thing (producer hel-treesit-producer) begin end)
  "Return the things whose around range may contain BEGIN..END.
These are the things that overlap BEGIN..END, the neighbours that only
whitespace and separators separate from it, and the thing after the
comments behind it, which may be a header comment."
  (let ((from (max (point-min)
                   (1- (hel-treesit--skip-gap begin -1))))
        (to (min (point-max)
                 (1+ (hel-treesit--skip-gap end 1 t)))))
    (hel-treesit--capture-things (hel-treesit-producer-capture producer)
                                 from to)))

(cl-defmethod hel-thing-next (_thing (producer hel-treesit-producer) pos dir)
  "Return the next thing of the query of PRODUCER from POS in DIR."
  (when (or (< 0 dir) (< (point-min) pos))
    (let ((capture (hel-treesit-producer-capture producer)))
      (hel-treesit--search-in-windows
       (lambda (begin end)
         (-> (->> (hel-treesit--capture-things capture begin end)
                  (-filter (lambda (instance)
                             (-let [(object-begin . object-end)
                                    (hel--thing-range instance)]
                               (if (< 0 dir)
                                   (and (< pos object-begin) (< object-begin end))
                                 (and (< begin object-end) (<= object-end pos)))))))
             (hel--nearest dir #'hel--thing-range)))
       pos
       (if (< 0 dir) (point-max) (point-min))))))

(cl-defmethod hel-thing-sibling (thing (producer hel-treesit-producer) instance dir
                                       &optional limit)
  "Return the next THING after INSTANCE in DIR with the same run parent.
Things nest, so the nearest thing may lie deeper: the method of a class
comes between two top-level functions. The search skips such things and
stays inside the parent node. DIR and LIMIT are as in the generic function."
  (let* ((capture (hel-treesit-producer-capture producer))
         (parent (hel-thing-run-parent thing producer instance))
         (forward (< 0 dir))
         (bound (if forward
                    (plist-get instance :end)
                  (hel--header-start (plist-get instance :begin))))
         (far (cond (limit
                     (if forward
                         (min (point-max) (1+ limit))
                       (max (point-min) (1- limit))))
                    ((treesit-node-p parent)
                     (if forward
                         (treesit-node-end parent)
                       (treesit-node-start parent)))
                    (forward (point-max))
                    (t (point-min)))))
    (hel-treesit--search-in-windows
     (lambda (begin end)
       (-> (->> (hel-treesit--capture-things capture begin end)
                (-filter
                 (lambda (candidate)
                   (and (hel--same-parent-p parent
                                            (hel-thing-run-parent thing producer candidate))
                        (if forward
                            (<= bound (plist-get candidate :begin) (1- end))
                          (<= (1+ begin) (plist-get candidate :end) bound))))))
           (hel--nearest dir #'hel--thing-range)))
     bound far)))

(cl-defmethod hel-thing-skip-separator (_thing (_producer hel-treesit-producer)
                                               pos dir)
  "Return the far edge of the separator node at POS in DIR."
  (hel-treesit--separator-end pos dir))

;;;; The motion producer

(cl-defstruct (hel-motion-producer (:constructor hel-motion-producer-create)
                                   (:copier nil))
  "The producer of the things that a motion moves over.
A thing is the text between two stops of the motion, without the
whitespace at its edges."
  (motion nil :documentation "A function of COUNT that moves point over
COUNT things, backward when COUNT is negative, and returns the part of
COUNT that it could not move, as `hel--forward-word' does."))

(defun hel-motion-source (motion)
  "Return the source of the things that MOTION moves over."
  (let ((producer (hel-motion-producer-create :motion motion)))
    (lambda () producer)))

(defun hel--motion-thing (thing begin end)
  "Return the THING from BEGIN to END, or nil.
The thing leaves out the whitespace at the edges of BEGIN..END, and it
covers whole lines when THING is linewise."
  (when-let* ((range (hel-trim-whitespace-from-range begin end)))
    (-let [(begin . end) (if (hel-thing-linewise-p thing)
                             (hel--line-range (car range) (cdr range))
                           range)]
      (list :begin begin :end end))))

(defun hel--motion-at (thing producer begin end)
  "Return the THING that contains BEGIN..END, or nil.
The thing starts where the motion of PRODUCER stops when it goes back
from the end of the thing. The first backward move can stop elsewhere:
from a header comment, `beginning-of-defun' goes back to the function
before it."
  (let ((motion (hel-motion-producer-motion producer)))
    (when-let* ((instance (save-excursion
                            (goto-char (min (1+ begin) (point-max)))
                            (and (zerop (funcall motion -1))
                                 (zerop (funcall motion 1))
                                 (let ((end (point)))
                                   (funcall motion -1)
                                   (hel--motion-thing thing (point) end)))))
                ((<= (plist-get instance :begin) begin))
                ((<= end (plist-get instance :end))))
      instance)))

(cl-defmethod hel-things-at (thing (producer hel-motion-producer) begin end)
  "Return the thing that holds BEGIN..END, and its neighbours."
  (-non-nil (list (hel--motion-at thing producer begin end)
                  (hel-thing-next thing producer begin -1)
                  (hel-thing-next thing producer (max begin (1- end)) 1))))

(cl-defmethod hel-thing-next (thing (producer hel-motion-producer) pos dir)
  "Return the next thing that the motion of PRODUCER finds from POS in DIR."
  (let ((motion (hel-motion-producer-motion producer)))
    (save-excursion
      (goto-char pos)
      (let (result)
        (while (and (not result)
                    (let ((old (point)))
                      (and (zerop (funcall motion dir))
                           (/= old (point)))))
          (let* ((stop (point))
                 (other (save-excursion
                          (funcall motion (- dir))
                          (point)))
                 (instance (hel--motion-thing thing
                                              (min stop other)
                                              (max stop other))))
            (when (and instance
                       (if (< 0 dir)
                           (< pos (plist-get instance :begin))
                         (<= (plist-get instance :end) pos)))
              (setq result instance))))
        result))))

(cl-defmethod hel-thing-run-parent ((_thing (eql 'function))
                                    (_producer hel-motion-producer) instance)
  "Return the start of the bracket pair around INSTANCE.
A function that a motion finds has no parent node. Its run stays inside
the bracket pair around it, as a sexp does."
  (hel--bracket-parent (plist-get instance :begin)))

;;;; The comment producer

(defun hel--adjacent-comment (comment-at comment dir)
  "Return the comment on the line next to COMMENT in direction DIR, or nil.
The comment must fill its line. COMMENT-AT is a function of a position
that returns the comment there."
  (save-excursion
    (goto-char (plist-get comment (if (< 0 dir) :end :begin)))
    (beginning-of-line)
    (when (and (if (< 0 dir) (not (eobp)) (not (bobp)))
               (zerop (forward-line dir))
               (progn (back-to-indentation)
                      (not (eolp))))
      (let ((pos (if (< 0 dir)
                     (point)
                   (end-of-line)
                   (skip-chars-backward " \t")
                   (1- (point)))))
        (when-let* ((other (funcall comment-at pos))
                    ((hel--fills-lines-p (plist-get other :begin)
                                         (plist-get other :end)))
                    ((if (< 0 dir)
                         (= pos (plist-get other :begin))
                       (save-excursion
                         (goto-char (plist-get other :end))
                         (forward-line 1)
                         (back-to-indentation)
                         (= (point) (plist-get comment :begin))))))
          other)))))

(defun hel--comment-run (comment-at comment)
  "Return the run of comment lines that holds COMMENT, as a thing.
A run is a sequence of comments, each of which fills its line, with no
blank line between them. A run covers whole lines, as a paragraph does,
with the line end of its last line. A comment after code is a run of
its own, and it covers only the comment. COMMENT-AT is a function of
a position that returns the comment there."
  (if (not (hel--fills-lines-p (plist-get comment :begin)
                               (plist-get comment :end)))
      comment
    (let ((first comment)
          (last comment))
      (while-let ((previous (hel--adjacent-comment comment-at first -1)))
        (setq first previous))
      (while-let ((next (hel--adjacent-comment comment-at last 1)))
        (setq last next))
      (-let [(begin . end) (hel--line-range (plist-get first :begin)
                                            (plist-get last :end))]
        (list :begin begin
              :end end
              :parent (plist-get first :parent))))))

(cl-defstruct (hel-comment-producer (:constructor hel-comment-producer-create)
                                    (:copier nil))
  "The producer of the runs of comment lines."
  (comment-at nil :documentation "A function of a position that returns the
comment that holds the character there, or nil.")
  (next-comment nil :documentation "A function of a position and
a direction that returns the start of the next comment, or nil."))

(defun hel--comment-run-from (producer start)
  "Return the comment run of PRODUCER that holds the comment at START."
  (when-let* ((start)
              (comment-at (hel-comment-producer-comment-at producer))
              (comment (funcall comment-at start)))
    (hel--comment-run comment-at comment)))

(cl-defmethod hel-things-at (_thing (producer hel-comment-producer) begin _end)
  "Return the comment run of PRODUCER at BEGIN, as a list."
  (-non-nil (list (hel--comment-run-from producer begin))))

(cl-defmethod hel-thing-next (_thing (producer hel-comment-producer) pos dir)
  "Return the next comment run of PRODUCER from POS in direction DIR."
  (let ((next-comment (hel-comment-producer-next-comment producer))
        (far-edge (if (< 0 dir) :end :begin))
        result)
    (while-let (((not result))
                (start (funcall next-comment pos dir))
                (run (hel--comment-run-from producer start))
                ((/= pos (plist-get run far-edge))))
      (if (if (< 0 dir)
              (< pos (plist-get run :begin))
            (<= (plist-get run :end) pos))
          (setq result run)
        (setq pos (plist-get run far-edge))))
    result))

(defun hel-treesit--comment-at (capture pos)
  "Return the comment of CAPTURE that holds the character at POS, or nil."
  (-when-let* ((node (-some->> (hel-treesit--thing-matches
                                capture pos (min (1+ pos) (point-max)))
                       (-mapcat (lambda (match) (plist-get match :nodes)))
                       (-filter (lambda (node)
                                  (and (<= (treesit-node-start node) pos)
                                       (< pos (treesit-node-end node)))))
                       (-min-by (-on #'> #'hel-treesit--node-length))))
               ((begin . end) (hel-trim-whitespace-from-range
                               (treesit-node-start node)
                               (treesit-node-end node))))
    (list :begin begin
          :end end
          :parent (treesit-node-parent node))))

(defun hel-treesit--next-comment (capture pos dir)
  "Return the start of the next comment of CAPTURE from POS in direction DIR.
Forward, the comment starts at or after POS. Backward, it ends at or
before POS. Return nil if there is none."
  (if (< dir 0)
      (hel-treesit--search-in-windows
       (lambda (begin end)
         (-some->>  (hel-treesit--thing-matches capture begin end)
           (-mapcat (lambda (match)
                      (plist-get match :nodes)))
           (-filter (lambda (node)
                      (< begin (treesit-node-end node) (1+ pos))))
           (-max-by (-on #'> #'treesit-node-end))
           (treesit-node-start)))
       pos (point-min))
    ;; else
    (hel-treesit--search-in-windows
     (lambda (begin end)
       (-some->> (hel-treesit--thing-matches capture begin end)
         (-mapcat (lambda (match)
                    (plist-get match :nodes)))
         (-map #'treesit-node-start)
         (-filter (lambda (start)
                    (and (<= pos start) (< start end))))
         (-min)))
     pos (point-max))))

(defun hel-treesit-comment-source (capture)
  "Return the source of the comment runs that CAPTURE finds in the queries.
The source applies where `hel-treesit-text-objects-p' returns non-nil."
  (lambda ()
    (when (hel-treesit-text-objects-p)
      (hel-comment-producer-create
       :comment-at (-partial #'hel-treesit--comment-at capture)
       :next-comment (-partial #'hel-treesit--next-comment capture)))))

(defun hel--syntax-comment-at (pos)
  "Return the comment that holds the character at POS, from the syntax table."
  (save-excursion
    (cl-loop for start in (list (let ((ppss (syntax-ppss pos)))
                                  (and (nth 4 ppss) (nth 8 ppss)))
                                pos
                                (1- pos))
             thereis
             (when-let* ((start)
                         ((<= (point-min) start))
                         ((not (memq (char-after start) '(nil ?\s ?\t ?\n ?\r))))
                         ((not (nth 3 (syntax-ppss start))))
                         ((progn (goto-char start)
                                 (forward-comment 1)))
                         (range (hel-trim-whitespace-from-range start (point)))
                         ((= start (car range)))
                         ((<= start pos))
                         ((< pos (cdr range))))
               (list :begin start
                     :end (cdr range)
                     :parent (nth 1 (syntax-ppss start)))))))

(defun hel--syntax-next-comment (pos dir)
  "Return the start of the next comment from POS in direction DIR, or nil.
Forward, the comment starts at or after POS. Backward, it starts before
POS."
  (comment-normalize-vars t)
  (save-excursion
    (goto-char pos)
    (if (< 0 dir)
        (comment-search-forward (point-max) t)
      (when-let* ((start (comment-search-backward (point-min) t))
                  ((< start pos)))
        start))))

(defun hel-syntax-comment-source ()
  "Return the producer of the comment runs that the syntax table finds."
  (hel-comment-producer-create :comment-at #'hel--syntax-comment-at
                               :next-comment #'hel--syntax-next-comment))

;;; Algorithms over runs

(defun hel--comment-line-p ()
  "Return the comment start if the line at point holds only a comment."
  (save-excursion
    (back-to-indentation)
    (unless (eolp)
      (let* ((line-start (line-beginning-position))
             (ppss (syntax-ppss))
             (start (if (nth 4 ppss) (nth 8 ppss) (point))))
        (goto-char start)
        (when (and (forward-comment 1)
                   (or (and (bolp) (< line-start (point)))
                       (progn
                         (skip-chars-forward " \t")
                         (eolp))))
          start)))))

(defun hel--header-start (pos)
  "Return the start of the header comment of the thing that starts at POS.
The header comment is the run of comment lines directly above the thing.
Return POS when the thing has no header comment."
  (save-excursion
    (goto-char pos)
    (if (not (hel--only-whitespace-p (line-beginning-position) pos))
        pos
      (let ((start pos))
        (while (and (progn
                      (beginning-of-line)
                      (not (bobp)))
                    (progn
                      (forward-line -1)
                      (when-let* ((comment (hel--comment-line-p)))
                        (setq start comment)))))
        start))))

(defun hel--thing-start (thing instance)
  "Return the start of INSTANCE, a THING, with its header comment."
  (if (hel-thing-header-p thing)
      (hel--header-start (plist-get instance :begin))
    (plist-get instance :begin)))

(cl-defgeneric hel-thing-around (thing producer first last)
  "Return the around range of the THINGs from FIRST to LAST.
The range holds the header comment of FIRST, the things, and the gap
after LAST, or the gap before FIRST when no gap follows LAST.")

(cl-defmethod hel-thing-around (thing producer first last)
  "Return the around range: the gap follows LAST, or else precedes FIRST.
The gap reaches the next THING, or its header comment, or
the previous thing. An empty gap does not count."
  (let* ((header-p (hel-thing-header-p thing))
         (begin (hel--thing-start thing first))
         (end   (plist-get last :end))
         (gap-end   (hel-thing-skip-gap thing producer end 1))
         (gap-begin (hel-thing-skip-gap thing producer begin -1))
         ;; The header comment of the next thing follows the gap.
         (limit (if header-p
                    (save-excursion
                      (goto-char gap-end)
                      (forward-comment (buffer-size))
                      (point))
                  gap-end)))
    (or (when-let* ((next (hel-thing-sibling thing producer last 1 limit))
                    (next-begin (hel--thing-start thing next))
                    ((< end next-begin))
                    ((<= next-begin gap-end)))
          (cons begin next-begin))
        (when-let* ((previous (hel-thing-sibling thing producer first -1 gap-begin))
                    (previous-end (plist-get previous :end))
                    ((< previous-end begin))
                    ((<= gap-begin previous-end)))
          (cons previous-end end))
        (cons begin end))))

(cl-defmethod hel-thing-around ((thing (eql 'comment)) producer first last)
  "Return the around range of the comment runs from FIRST to LAST.
A comment after code has the spaces and tabs before it as its gap when no
gap follows it, so that `mac' and `d' leave no blanks at the end of the
line of code."
  (-let [(range &as begin . end) (cl-call-next-method)]
    (if (and (equal range (cons (plist-get first :begin)
                                (plist-get last :end)))
             (not (save-excursion
                    (goto-char begin)
                    (bolp))))
        (cons (hel-thing-skip-gap thing producer begin -1)
              end)
      range)))

(cl-defgeneric hel-thing-at (thing producer begin end)
  "Return the THING at BEGIN..END, or nil.
PRODUCER finds the things. An empty BEGIN..END is a bare point.")

(cl-defmethod hel-thing-at (thing producer begin end)
  "Return the thing whose range or around range holds BEGIN..END.
Of nested things, the innermost one wins. Of two neighbours, the one
that contains BEGIN..END wins, and the one after a bare point wins
a tie. When no thing contains BEGIN..END, the selection lies in a gap or
on a header comment: the thing before it wins, and the thing after it
when nothing comes before."
  (cl-flet* ((contains-p (range)
               (<= (car range) begin end (cdr range)))
             (inside-p (inner outer)
               (and (not (equal inner outer))
                    (<= (car outer) (car inner) (cdr inner) (cdr outer)))))
    (let* ((candidates
            (->> (hel-things-at thing producer begin end)
                 ;; Keep one thing of each range.
                 (-group-by #'hel--thing-range)
                 (-map #'cadr)
                 (-filter (lambda (instance)
                            (or (contains-p (hel--thing-range instance))
                                (contains-p (hel-thing-around thing producer
                                                              instance instance)))))))
           (innermost
            (-remove (lambda (instance)
                       (-any? (lambda (other)
                                (inside-p (hel--thing-range other)
                                          (hel--thing-range instance)))
                              candidates))
                     candidates)))
      (or (-some->> innermost
            (-filter (lambda (instance)
                       (contains-p (hel--thing-range instance))))
            (-max-by (-on #'> (lambda (x) (plist-get x :begin)))))
          (-some->> innermost
            (-filter (lambda (x) (<= (plist-get x :end) begin)))
            (-max-by (-on #'> (lambda (x) (plist-get x :end)))))
          (-some->> innermost
            (-min-by (-on #'> (lambda (x) (plist-get x :begin)))))))))

(defun hel--extend-things (thing producer first last count)
  "Add COUNT THINGs to the run from FIRST to LAST.
A positive COUNT adds them after LAST, a negative one before FIRST. When
fewer things exist, add the ones that exist. Return (FIRST . LAST)."
  (let ((dir (hel-sign count)))
    (cl-loop repeat (abs count)
             for next = (hel-thing-sibling thing producer
                                           (if (< 0 dir) last first)
                                           dir)
             while next
             do (if (< 0 dir)
                    (setq last next)
                  (setq first next))))
  (cons first last))

(defun hel--thing-under (thing producer pos dir)
  "Return the innermost THING under the cursor at POS, or nil.
Forward, the cursor is the character after POS, and backward the
character before it."
  (let ((char (if (< 0 dir) pos (1- pos))))
    (when (and (<= (point-min) char) (< char (point-max)))
      (-some->> (hel-things-at thing producer char (1+ char))
        (-filter (lambda (instance)
                   (and (<= (plist-get instance :begin) char)
                        (< char (plist-get instance :end)))))
        (-min-by (-on #'> (lambda (instance)
                            (- (plist-get instance :end)
                               (plist-get instance :begin)))))))))

(cl-defgeneric hel-thing-edge (thing producer pos dir count edge whole)
  "Return the range from POS to the EDGE of the COUNT-th THING in DIR.
PRODUCER finds the things. EDGE is
`:begin' or `:end'. With WHOLE non-nil, a range that does not start
inside its first thing holds that whole thing.

Return (START END REST), where REST is the part of COUNT that found no
thing, or nil when no thing exists in DIR.")

(cl-defmethod hel-thing-edge (thing producer pos dir count edge whole)
  "Return the range from POS to the EDGE of the COUNT-th THING in DIR.
The first thing is the innermost one under the cursor at POS when its
EDGE lies ahead of POS in DIR, and the next thing in DIR otherwise.
Each next thing comes from the far edge of the one before it. The range
starts at POS, or at the near edge of the first thing when WHOLE is
non-nil and POS lies outside that thing."
  (let* ((forward (< 0 dir))
         (ahead-p (lambda (instance)
                    (let ((target (plist-get instance edge)))
                      (if forward (< pos target) (< target pos)))))
         (here (when-let* ((instance (hel--thing-under thing producer pos dir))
                           ((funcall ahead-p instance)))
                 instance))
         (next (lambda (instance)
                 (hel-thing-next thing producer
                                 (if forward
                                     (1- (plist-get instance :end))
                                   (plist-get instance :begin))
                                 dir)))
         (first (or here
                    (when-let* ((instance (hel-thing-next thing producer pos dir)))
                      ;; The next thing may have its EDGE at POS, as the
                      ;; paragraph that `{' starts from does: then take
                      ;; the thing after it.
                      (if (funcall ahead-p instance)
                          instance
                        (funcall next instance)))))
         (last first)
         (rest (1- count)))
    (when first
      (while-let (((< 0 rest))
                  (instance (funcall next last)))
        (setq last instance)
        (cl-decf rest))
      (list (if (and whole (not here))
                (plist-get first (if forward :begin :end))
              pos)
            (plist-get last edge)
            rest))))

(defun hel--backward-word-end (motion count)
  "Move point back to the end of the COUNT-th previous word of MOTION.
Return the part of COUNT that found no word end. A line start and the
blanks before a word are steps of their own."
  (while (and (< 0 count)
              (let ((old (point)))
                (cond ((and (bolp) (not (bobp)))
                       (skip-chars-backward "\r\n"))
                      ((hel-whitespace-p (preceding-char))
                       (hel-skip-whitespaces -1))
                      (t
                       (funcall motion -1)
                       (hel-skip-whitespaces -1)))
                (/= old (point))))
    (cl-decf count))
  count)

(cl-defmethod hel-thing-edge ((_thing (eql 'word)) (producer hel-motion-producer)
                              pos dir count edge _whole)
  "Return the range from POS over COUNT word steps to EDGE.
A word does not cross a line end: the line end and the indentation of
a line are steps of their own. The range starts at POS after the line
ends that a forward motion, or a backward motion to a start, crosses
first."
  (hel--word-edge (hel-motion-producer-motion producer) pos dir count edge))

(cl-defmethod hel-thing-edge ((_thing (eql 'WORD)) (producer hel-motion-producer)
                              pos dir count edge _whole)
  "Return the range from POS over COUNT WORD steps to EDGE.
WORDs follow the rules of words."
  (hel--word-edge (hel-motion-producer-motion producer) pos dir count edge))

(defun hel--word-edge (motion pos dir count edge)
  "Return the range from POS over COUNT steps of MOTION in DIR to EDGE.
See the method of `hel-thing-edge' for words."
  (save-excursion
    (goto-char pos)
    (if (and (< dir 0) (eq edge :end))
        (let ((rest (hel--backward-word-end motion count)))
          (list pos (point) rest))
      (if (< 0 dir)
          (skip-chars-forward "\r\n")
        (skip-chars-backward "\r\n"))
      (let ((start (point))
            rest)
        (pcase (list dir edge)
          ('(1 :begin)
           ;; The blanks before the next word are the first step.
           (when (hel-whitespace-p (following-char))
             (cl-decf count))
           (setq rest (funcall motion count))
           (hel-skip-whitespaces 1))
          ('(1 :end)
           (setq rest (funcall motion count)))
          (_
           (setq rest (funcall motion (- count)))))
        (list start (point) (abs rest))))))

;;; Algorithms over trees

(defun hel--node-at-selection (thing producer)
  "Return the node of THING under the selection, or nil without one.
The node covers the region without the whitespace at its edges."
  (-when-let ((begin . end) (hel--selection))
    (hel-node-at thing producer begin end)))

(defun hel--point-touching (thing producer pos dir)
  "Return the smallest node of THING that touches POS, or nil.
Try the side DIR first, and then the other side."
  (or (hel-node-touching thing producer pos dir)
      (hel-node-touching thing producer pos (- dir))))

(defun hel--point-node (thing producer dir)
  "Return the node that a tree command starts from at a bare point, or nil.
This is the token with point inside it, or else the smallest node that
touches point, on the side DIR first."
  (or (hel-node-token thing producer (point))
      (hel--point-touching thing producer (point) dir)))

(defun hel--point-sibling-node (thing producer dir)
  "Return the node that a sibling command starts from at a bare point.
This is the token with point inside it, or else the child of the
enclosing node on the side DIR of point, so that the next step goes on
from sibling to sibling."
  (or (hel-node-token thing producer (point))
      (hel-node-side-child thing producer (point) dir)))

(defun hel--start-node (thing producer direction)
  "Return the node that a command without siblings starts from, or nil.
With a selection, this is the node under it. Without selection, it is
the node at point toward the DIRECTION if point is at the boundary
between two nodes."
  (if (use-region-p)
      (hel--node-at-selection thing producer)
    (or (hel--point-node thing producer direction)
        (hel-node-enclosing thing producer (point)))))

(defun hel--parent-targets (thing producer node dir)
  "Return the targets of the parent commands from NODE in direction DIR.
These are the ends of NODE and of its ancestors, or their starts when DIR
is negative, the nearest first. Every target lies ahead of point in DIR,
and no two are equal."
  (let (targets)
    (while node
      (let ((target (funcall (if (< dir 0) #'car #'cdr)
                             (hel-node-range thing producer node))))
        (when (and (if (< dir 0)
                       (< target (point))
                     (< (point) target))
                   (not (eql target (-first-item targets))))
          (push target targets)))
      (setq node (hel-node-parent thing producer node)))
    (nreverse targets)))

(defun hel--single-child-chain-end (thing producer node)
  "Return NODE, or its descendant through only children of the same range."
  (let ((range (hel-node-range thing producer node))
        children)
    (while (and (setq children (hel-node-children thing producer node))
                (null (cdr children))
                (equal range (hel-node-range thing producer (car children))))
      (setq node (car children)))
    node))

(defun hel--all-children (thing producer node)
  "Return the ranges of the children of NODE.
A node whose only child has its range counts as that child."
  (->> (hel--single-child-chain-end thing producer node)
       (hel-node-children thing producer)
       (-map (lambda (child)
               (hel-node-range thing producer child)))))

(defun hel--all-siblings (thing producer node)
  "Return the ranges of NODE and of all its siblings.
When NODE is an only child, use the nearest parent with more than one
child."
  (cl-loop for parent = (hel-node-parent thing producer node)
           then (hel-node-parent thing producer parent)
           while parent
           for children = (hel-node-children thing producer parent)
           when (cdr children)
           return (-map (-partial #'hel-node-range thing producer)
                        children)))

(defun hel--smallest-enclosing-region (regions begin end)
  "Return the smallest of REGIONS that contains BEGIN..END and is larger."
  (-some->> regions
    (-non-nil)
    (-filter (-lambda ((region &as r-begin . r-end))
               (and (<= r-begin begin end r-end)
                    (not (equal (cons begin end)
                                region)))))
    (-min-by (-on #'> (-lambda ((begin . end)) (- end begin))))))

;;; Drivers

(defun hel--mark-text-object (thing kind count)
  "Select COUNT THINGs at the selection, the range of KIND.
KIND is `:inner' for the things alone, and `:around' for the things
with their header comment and gap. When the selection is the range that
the previous call for THING and KIND selected, add COUNT more things."
  (if-let* ((producer (hel--producer thing)))
      (progn
        (hel--sync-selection-history)
        (-let* ((key (list thing kind))
                (before (hel--selection-before))
                (current (if (use-region-p)
                             (cons (region-beginning)
                                   (region-end))))
                ((_ run-range run-first run-last) hel--text-object-run)
                ((first . last)
                 (if (and current
                          (equal key (car hel--text-object-run))
                          (equal run-range current))
                     (hel--extend-things thing producer run-first run-last count)
                   (-when-let* (((begin . end) (or (hel--selection)
                                                   (cons (point) (point))))
                                (instance (hel-thing-at thing producer begin end)))
                     (hel--extend-things thing producer instance instance
                                         (- count (hel-sign count))))))
                (range (when first
                         (pcase kind
                           (:inner (cons (plist-get first :begin)
                                         (plist-get last :end)))
                           (_ (hel-thing-around thing producer first last))))))
          (when (and range
                     (not (equal range current)))
            (hel-push-point)
            (hel-select range -1)
            (hel--record-selection before)
            (setq hel--text-object-run (list key range first last))
            (hel-reveal-point-when-on-top))))
    (hel--no-producer-message thing)))

(defun hel--mark-to-thing-edge (thing count edge whole)
  "Select from point to the EDGE of the COUNT-th THING.
A negative COUNT goes backward. EDGE and WHOLE are as in `hel-thing-edge'.
With the selection extended, extend it. When no thing exists in the direction,
the selection stays as it is."
  (if-let* ((producer (hel--producer thing)))
      (-when-let ((start end) (hel-thing-edge thing producer (point)
                                              (hel-sign count) (abs count)
                                              edge whole))
        (when (hel-thing-jump-p thing)
          (hel-push-point))
        (hel-select-motion start end)
        (hel-reveal-point-when-on-top))
    ;; else
    (hel--no-producer-message thing)))

(defun hel--expand-1 (thing producer)
  "Expand the selection one step over the nodes of THING.
Point goes to the start: when the selection outgrows the window, the start
shows what the selection is. Without a selection, the first step selects
the node that touches point, when point lies inside no token. Return non-nil
if the selection changed."
  (hel--sync-selection-history)
  (-let* ((before (hel--selection-before))
          ((begin end) before))
    (-when-let ((new-begin . new-end)
                (or (and (not (use-region-p))
                         (not (hel-node-token thing producer (point)))
                         (-some->> (hel--point-touching thing producer (point) 1)
                           (hel-node-range thing producer)))
                    (-> (hel-node-expand-steps thing producer begin end)
                        (hel--smallest-enclosing-region begin end))))
      (hel-select (cons new-begin new-end) -1)
      (hel--record-selection before)
      t)))

(defun hel--select-first-child (thing producer)
  "Select the first child of the node under the selection.
Keep the direction of the selection. Without a selection, select the
first child of the node after point, forward. Return non-nil if the
selection changed."
  (let ((selected (use-region-p)))
    (when-let* ((node (if selected
                          (hel--node-at-selection thing producer)
                        (hel-node-touching thing producer (point) 1)))
                (child (-first-item (hel--all-children thing producer node)))
                ((not (and selected
                           (equal child (cons (region-beginning) (region-end)))))))
      (hel-select child (if selected (hel-region-direction) 1))
      t)))

(defun hel--shrink-1 (thing producer)
  "Shrink the selection one step. Return non-nil if it changed.
Return to the selection before the last expand. When the selection is not
the last one in the history, select the first child of its node."
  (-let [(before after run) (car hel-selection-history)]
    (if (and hel-selection-history
             (equal after (hel-region)))
        (progn
          (pop hel-selection-history)
          (setq hel--text-object-run run)
          (hel--restore-selection before)
          t)
      (setq hel-selection-history nil)
      (hel--select-first-child thing producer))))

(defconst hel--expand-commands
  '(hel-expand-selection
    hel-shrink-selection
    hel-select-node-start
    hel-select-node-end)
  "The commands of one series of expand and shrink steps.
The first command of a series pushes the old point to the mark ring.")

(defun hel--expand (thing count)
  "Expand the selection COUNT times over the nodes of THING.
A negative COUNT shrinks it."
  (when-let* ((producer (hel--producer thing)))
    (if (< count 0)
        (dotimes (_ (- count))
          (hel--shrink-1 thing producer))
      (let ((origin (point))
            changed)
        (dotimes (_ count)
          (when (hel--expand-1 thing producer)
            (setq changed t)))
        (when (and changed
                   (not (memq last-command hel--expand-commands)))
          (hel-push-point origin))))
    (hel-reveal-point-when-on-top)))

(defun hel--moving-node (thing producer)
  "Return the node of THING at the end of the selection that point is on.
Of the nodes that end there, this is the largest one inside the selection."
  (-let* (((begin . end) (hel--selection))
          (forward (< 0 (hel-region-direction)))
          (char (if forward (1- end) begin))
          (node (hel-node-at thing producer char (1+ char))))
    (while-let ((node)
                (parent (hel-node-parent thing producer node))
                ((-let [(parent-begin . parent-end)
                        (hel-node-range thing producer parent)]
                   (<= begin parent-begin parent-end end))))
      (setq node parent))
    node))

(defun hel--extend-to-sibling (thing producer dir)
  "Move point of the extended selection over one sibling in direction DIR.
Toward the side of point, the selection takes the next sibling. Away from
it, the selection gives back its last node, and when only one node is left,
it keeps that node and turns around. Return non-nil if the selection
changed."
  (if (not (use-region-p))
      (when-let* ((node (hel--point-sibling-node thing producer dir))
                  (range (hel-node-range thing producer node)))
        (goto-char (if (< 0 dir) (cdr range) (car range)))
        t)
    (when-let* ((node (hel--moving-node thing producer))
                (range (hel-node-range thing producer node))
                (sibling (hel-node-sibling thing producer node dir))
                (target (hel-node-range thing producer sibling)))
      (cond ((= dir (hel-region-direction))
             (goto-char (if (< 0 dir) (cdr target) (car target))))
            ;; The sibling still overlaps the selection: give back NODE.
            ((if (< 0 dir)
                 (< (car target) (region-end))
               (< (region-beginning) (cdr target)))
             (goto-char (if (< 0 dir) (car target) (cdr target))))
            ;; NODE is the last one: keep it, and go on past the other end.
            (t
             (hel-select (if (< 0 dir)
                             (cons (car range) (cdr target))
                           (cons (car target) (cdr range)))
                         dir)))
      t)))

(defun hel--select-sibling (thing count)
  "Select the COUNT-th sibling of the node of THING under the selection.
A negative COUNT goes backward. At the last sibling, keep the selection.
Keep the direction of the selection. Without a selection, first select the
node at point on the side of the motion, in the direction of the motion.
With the selection extended, extend it."
  (when-let* ((producer (hel--producer thing)))
    (if hel--extend-selection
        (let ((origin (point)))
          (when (/= count
                    (hel-motion-loop (dir count)
                      (hel--extend-to-sibling thing producer dir)))
            (hel-push-point origin)
            (hel-reveal-point-when-on-top)))
    (let* ((origin (point))
           (dir (hel-sign count))
           (no-selection? (not (use-region-p)))
           (direction (if no-selection? dir (hel-region-direction)))
           (node (if no-selection?
                     (hel--point-sibling-node thing producer dir)
                   (hel--node-at-selection thing producer)))
           moved?)
      (when node
        (cl-flet ((select (node)
                    (hel-select (hel-node-range thing producer node) direction)))
          (when no-selection?
            (select node)
            (setq moved? t)
            (cl-callf - count dir))
          (when (/= count
                    (hel-motion-loop (dir count)
                      (when-let* ((sibling (hel-node-sibling thing producer node dir)))
                        (setq node sibling)
                        (select node))))
            (setq moved? t)))
        (when moved?
          (hel-push-point origin)
          (hel-reveal-point-when-on-top)))))))

(defun hel--select-to-parent-edge (thing count)
  "Select from point to the end of the node of THING at the selection.
When point is already there, go to the end of the nearest ancestor that
ends elsewhere. COUNT goes that many levels; a negative COUNT goes to the
starts. With the selection extended, extend it."
  (when-let* ((producer (hel--producer thing)))
    (let* ((dir (hel-sign count))
           (origin (point))
           (targets (when-let* ((node (hel--start-node thing producer dir)))
                      (hel--parent-targets thing producer node dir)))
           (target (or (nth (1- (abs count)) targets)
                       (-last-item targets))))
      (when target
        (hel-select-motion origin target)
        (hel-push-point origin)
        (hel-reveal-point-when-on-top)))))

(defun hel--select-regions-for-each-cursor (fn)
  "Put a cursor on each region that FN returns, for every cursor.
FN takes no arguments and runs at each cursor with its selection."
  (hel-with-each-cursor
    (hel-extend-selection -1)
    (when-let* ((regions (funcall fn)))
      (hel-select-regions regions (if (use-region-p)
                                      (hel-region-direction)
                                    1)))))

(defun hel--select-all-siblings (thing)
  "Put a cursor on the node of THING at each cursor and on its siblings.
At a bare point that touches no node, use the children of the node around
point."
  (hel--select-regions-for-each-cursor
   (lambda ()
     (when-let* ((producer (hel--producer thing)))
       (if-let* ((node (if (use-region-p)
                           (hel--node-at-selection thing producer)
                         (hel--point-node thing producer 1))))
           (hel--all-siblings thing producer node)
         (-some->> (hel-node-enclosing thing producer (point))
           (hel--all-children thing producer)))))))

(defun hel--select-all-children (thing)
  "Put a cursor on each child of the node of THING at each cursor."
  (hel--select-regions-for-each-cursor
   (lambda ()
     (when-let* ((producer (hel--producer thing))
                 (node (hel--start-node thing producer 1)))
       (hel--all-children thing producer node)))))

;;; Things

(put 'node 'hel-thing-sources
     (list #'hel-treesit-node-source
           #'hel-lisp-node-source
           #'hel-syntax-node-source))

(put 'word 'hel-thing-sources
     (list (hel-motion-source #'hel--forward-word)))

(put 'WORD 'hel-thing-sources
     (list (hel-motion-source #'hel--forward-WORD)))

(put 'sentence 'hel-thing-sources
     (list (hel-motion-source #'hel--forward-sentence)))

(put 'paragraph 'hel-thing-sources
     (list (hel-motion-source #'hel--forward-paragraph)))

(put 'function 'hel-thing-sources
     (list (hel-treesit-source "function")
           (hel-motion-source #'hel--forward-function)))

(put 'class 'hel-thing-sources
     (list (hel-treesit-source "class")))

(put 'argument 'hel-thing-sources
     (list (hel-treesit-source "parameter")))

(put 'comment 'hel-thing-sources
     (list (hel-treesit-comment-source "comment")
           #'hel-syntax-comment-source))

;;; .
(provide 'hel-structural-editing)
;;; hel-structural-editing.el ends here
