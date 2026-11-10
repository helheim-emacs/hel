;;; hel-text-objects.el --- Text objects and motions -*- lexical-binding: t -*-
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
;; The thing protocol, the text objects and the motions over things. The
;; commands in `hel-commands.el' call them, and every one of these commands
;; works in every buffer. The tree level, which walks a syntax tree, builds
;; on this file in `hel-structural-editing.el'.
;;
;; Every command runs an algorithm over one kind of thing such as `word',
;; `function' or `comment'. The property `hel-thing-sources' of a thing holds
;; a list of /sources/. A source is a function of no arguments that returns
;; a /producer/ at the cursor, or nil when it does not apply there. The first
;; source that returns a producer wins.
;;
;; Every operation of the protocol takes the thing and the producer, and
;; dispatches on the argument that its answer depends on:
;;
;; - How the things are found depends on the producer: a motion such as
;;   `hel--forward-word', a tree-sitter query, or a finder of comments.
;; - Which rules hold for a kind of thing depends on the thing: a word
;;   gap stays on its line, a paragraph covers whole lines, a function
;;   owns the comment above it. A rule that is a fixed fact, such as the
;;   last two, is a symbol property of the thing.
;;
;; The producers in this file give things one after another
;; (`hel-thing-candidates', `hel-thing-next'). The text object keys and the
;; motions need nothing more. A tree producer knows the parent and the
;; children of a node instead, and `hel-structural-editing.el' defines it.
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
(require 'newcomment)
(require 'treesit)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-multiple-cursors-core)
(require 'hel-core)

;;; Motions

(cl-defun hel--forward-word (&optional (count 1))
  "Move point COUNT words forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.

A word is:
- a run of characters matching `[[:word:]]';
- a run of characters that are neither word characters nor whitespace."
  (hel-motion-loop (dir count)
    (hel-skip-chars "\r\n" dir)
    (hel-skip-blanks dir)
    (or (hel-line-boundary-p dir)
        (/= 0 (hel-skip-chars (concat "^[:word:]" hel-whitespace-chars) dir))
        (let ((word-separating-categories hel-cjk-word-separating-categories)
              (word-combining-categories  hel-cjk-word-combining-categories))
          (forward-word dir)))))

(cl-defun hel--forward-WORD (&optional (count 1))
  "Move point COUNT WORDs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.

A WORD is a run of characters between whitespace."
  (hel-motion-loop (dir count)
    (hel-skip-chars "\r\n" dir)
    (hel-skip-blanks dir)
    (unless (hel-line-boundary-p dir)
      (hel-skip-chars (concat "^" hel-whitespace-chars) dir))))

(cl-defun hel--forward-sentence (&optional (count 1))
  "Move point COUNT sentences forward (backward if COUNT is negative).
Return the part of COUNT that is left to move, with the sign of COUNT.

The function `forward-sentence-function' defines what a sentence is."
  (hel-motion-loop (dir count)
    (ignore-errors (forward-sentence dir))))

(cl-defun hel--forward-paragraph (&optional (count 1))
  "Move point COUNT paragraphs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT."
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (hel-motion-loop (dir count)
      (cond ((natnump dir)
             (forward-paragraph))
            ((not (bobp))
             (start-of-paragraph-text)
             (beginning-of-line))))))

(cl-defun hel--forward-function (&optional (count 1))
  "Move point COUNT functions forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT."
  (hel-motion-loop (dir count)
    (if (< dir 0)
        (beginning-of-defun)
      (end-of-defun))))

(cl-defun hel--forward-sexp (&optional (count 1))
  "Move point COUNT sexps forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT."
  ;; Make binding only during this motion instead of global one, because some
  ;; major-modes rely on the jump to the buffer end.
  (let ((forward-sexp-function
         (if (memq forward-sexp-function '(forward-sexp-default-function nil))
             #'hel-forward-sexp-only
           ;; Do not rebind in modes that use their own version of
           ;; `forward-sexp-function' (`c-mode', `python-mode').
           forward-sexp-function)))
    (hel-motion-loop (dir count)
      (ignore-errors
        (forward-sexp dir)))))

(cl-defun hel-forward-sexp-only (&optional (count 1))
  "Move forward across COUNT balanced expressions (sexps).
Hel binds `forward-sexp-function' to this function during its own sexp
motions.  Unlike the default `forward-sexp' behavior, it does not move to
the end of the buffer when there is no sexp ahead.

Signal `scan-error' when there is no sexp to move over. The error
arguments follow the format used by `forward-sexp'."
  (if-let* ((pos (scan-sexps (point) count)))
      (progn
        (goto-char pos)
        (if (< count 0) (backward-prefix-chars)))
    (signal 'scan-error (if (< count 0)
                            (list "No previous sexp" (point-min) (point))
                          (list "No next sexp" (point) (point-max))))))

;;; Selection history

(defun hel--sync-selection-history ()
  "Forget `hel-selection-history' if the selection has changed.
If only the direction of the selection differs, remember the new direction."
  (-let [(range dir) (hel-selection)]
    (if-let* ((newest (car hel-selection-history))
              ((equal (car newest) range)))
        (setf (nth 1 newest) dir)
      (setq hel-selection-history nil))))

(defun hel--record-selection (before &optional run)
  "Remember the active region and RUN in `hel-selection-history'.
BEFORE is the selection before the command, and it starts an empty history.
RUN has the form described there."
  (unless hel-selection-history
    (push before hel-selection-history))
  (push (append (hel-selection) (list run))
        hel-selection-history))

(defun hel--restore-selection (entry)
  "Restore the selection of ENTRY of `hel-selection-history'."
  (-let [(range dir) entry]
    (if dir
        (hel-select range dir)
      (deactivate-mark)
      (goto-char (car range)))))

(defun hel--selection-before ()
  "Return the selection before a command, in the form of `hel-selection'.
Without a region, return an empty range at point and nil."
  (or (hel-selection)
      (list (cons (point) (point)) nil)))

;;; The protocol
;;;; Things and sources

(defun hel-producer (thing)
  "Return the producer of THING at the cursor, or nil.
The producer comes from the first source in the property
`hel-thing-sources' of THING that returns one."
  (-some #'funcall (get thing 'hel-thing-sources)))

(defun hel--no-producer-message (thing)
  "Report that no source of THING applies at the cursor."
  (message "No %s here" thing))

;;;; The thing instance
;;
;; A producer returns each thing that it finds as a `hel-thing-instance':
;; the bounds of the thing and the parent that the producer knows. The
;; operations that find things return this struct, whatever producer finds
;; them, and every algorithm in this file reads it. The tree level in
;; `hel-structural-editing.el' works on nodes instead.

(cl-defstruct (hel-thing-instance (:constructor hel-thing-instance--create)
                                  (:copier nil))
  "A thing that a producer found."
  (begin  nil :documentation "The start of the thing.")
  (end    nil :documentation "The end of the thing.")
  (parent nil :documentation "\
The parent that the producer knows: a tree-sitter node, a position, or nil."))

(defun hel-thing-instance-create (thing range &optional parent)
  "Return an instance of THING that lies on RANGE.
A THING that covers whole lines takes these lines with the last line
end, if it is the only text on them. PARENT is the parent that the
producer knows."
  (-let [(begin . end) (if (and (hel-thing-linewise-p thing)
                                (hel-only-text-on-lines-p range))
                           (hel-line-bounds range)
                         range)]
    (hel-thing-instance--create :begin begin
                                :end end
                                :parent parent)))

(defun hel-thing-instance-bounds (self)
  "Return the (BEGIN . END) bounds of `hel-thing-instance'."
  (cons (hel-thing-instance-begin self)
        (hel-thing-instance-end self)))

(defun hel-thing-instance-edge (self edge)
  "Return the EDGE position of `hel-thing-instance'.
EDGE is `:begin' or `:end'."
  (cl-ecase edge
    (:begin (hel-thing-instance-begin self))
    (:end   (hel-thing-instance-end self))))

;;;; The operations

(cl-defgeneric hel-thing-candidates (thing producer pos)
  "Return the THINGs that a text object command can select from POS.
These are the things that hold POS, and the neighbours whose gap or
header comment may hold it. PRODUCER finds the things.")

(cl-defgeneric hel-thing-next (thing producer pos dir)
  "Return the nearest THING from POS in direction DIR, or nil.
Forward, the thing starts after the character at POS. Backward, it ends
at or before POS. Of two things that start, or end, at the same place,
return the larger one. PRODUCER finds the things.")

(cl-defgeneric hel-thing-sibling (thing producer instance dir &optional limit)
  "Return the sibling of INSTANCE in direction DIR, or nil.
The sibling is the nearest THING in DIR that has the same parent as
INSTANCE, see `hel-thing-parent'. With LIMIT, the sibling starts at
or before LIMIT forward, and ends at or after LIMIT backward.

By default, the sibling is the next THING after INSTANCE, and the result
is nil when that thing has another parent.")

(cl-defgeneric hel-thing-owning (thing producer pos)
  "Return the THING that a text object command selects from POS, or nil.
POS can be in the thing, on its boundary, on its header comment, or in the
gap next to it.

Of nested things, the innermost one wins. Of two neighbours, the one that
holds POS wins, and otherwise the one before POS. Where two things meet at
POS, the thing after it wins.

PRODUCER finds the things.")

(cl-defgeneric hel-thing-around (thing producer first last)
  "Return what an around text object selects for the THINGs from FIRST to LAST.
This is the header comment of FIRST, the things, and the gap after LAST.
When no gap follows LAST, it takes the gap before FIRST instead. PRODUCER
finds the things.

By default, the gap after LAST ends at the next THING. For a thing with
a header comment, it ends at the first comment before the next THING, if
there is one. The gap before FIRST reaches the previous thing. An empty
gap counts as no gap.")

(cl-defgeneric hel-thing-edge (thing producer pos dir count edge whole)
  "Return the range from POS to the EDGE of the COUNT-th THING in DIR.
With WHOLE non-nil and POS outside the first thing, the range starts at
the near edge of that thing, so it holds the whole thing. With fewer
than COUNT things, the range goes to the last of them; with EDGE `:begin'
and WHOLE nil, it goes to the buffer edge in DIR. Return nil when the
range has nowhere to go.

EDGE is `:begin' or `:end'. PRODUCER finds the things.

By default, the first thing is the innermost one under the cursor at POS
when its EDGE lies ahead of POS in DIR, and the next thing in DIR
otherwise. Each next thing comes from the far edge of the one before it.
The range starts at POS, or at the near edge of the first thing when WHOLE
is non-nil and POS lies outside that thing. It ends where
`hel-thing-motion-edge' says for the last thing.")

(cl-defgeneric hel-thing-motion-edge (thing instance edge)
  "Return the position where a motion to the EDGE of INSTANCE stops.
INSTANCE is a THING, and EDGE is `:begin' or `:end'.

By default, this is that edge of INSTANCE.")

(cl-defgeneric hel-thing-after-gap (thing producer pos dir)
  "Return the position where the gap from POS in direction DIR ends.
A gap is the text between two THINGs, such as whitespace. PRODUCER finds
the things.

By default, the gap is the whitespace from POS.")

(cl-defgeneric hel-thing-parent (thing producer instance)
  "Return the parent of INSTANCE of THING that PRODUCER found.
A text object with a count selects a run of things, and the run stays
inside the parent. Two things with equal parents are siblings.")

;;;; Rules about a kind of thing

(defun hel-thing-header-comment-p (thing)
  "Return non-nil if the comment lines above a THING belong to it.
The property `hel-thing-header-comment' of THING holds the answer."
  (get thing 'hel-thing-header-comment))

(defun hel-thing-linewise-p (thing)
  "Return non-nil if a THING covers whole lines.
Such a thing runs from the start of its first line to the end of its
last line, with the line end, when it is the only text on them.
The property `hel-thing-linewise' of THING holds the answer."
  (get thing 'hel-thing-linewise))

(defun hel-thing-jump-p (thing)
  "Return non-nil if a motion over THINGs is a jump.
A jump pushes the old point to the mark ring. The property
`hel-thing-jump' of THING holds the answer, and a THING without this
property jumps."
  (if-let* ((cell (plist-member (symbol-plist thing) 'hel-thing-jump)))
      (cadr cell)
    t))

(defun hel-thing-selection-direction (thing)
  "Return the direction of a selection that holds one THING.
With 1, point goes to the end of the selection, and with -1 to the start.
The property `hel-thing-selection-direction' of THING holds the answer, and
a THING without this property gives 1."
  (or (get thing 'hel-thing-selection-direction)
      1))

;;; Algorithms

(cl-defmethod hel-thing-sibling (thing producer instance dir &optional limit)
  (when-let* ((next (hel--thing-past thing producer instance dir))
              ((equal (hel-thing-parent thing producer instance)
                      (hel-thing-parent thing producer next)))
              ((or (not limit)
                   (if (< dir 0)
                       (<= limit (hel-thing-instance-end next))
                     (<= (hel-thing-instance-begin next) limit)))))
    next))

(defun hel--thing-past (thing producer instance dir)
  "Return the nearest THING past INSTANCE in direction DIR, or nil."
  (hel-thing-next thing producer
                  (if (< 0 dir)
                      (1- (hel-thing-instance-end instance))
                    (hel-thing-instance-begin instance))
                  dir))

(cl-defmethod hel-thing-after-gap (_thing _producer pos dir)
  (hel-after-whitespace pos dir))

(cl-defmethod hel-thing-parent (_thing _producer instance)
  (hel-thing-instance-parent instance))

(defun hel--header-comment-begin (pos)
  "Return the start of the header comment of the thing that starts at POS.
The header comment is the comment lines directly above the thing.
Return POS when the thing has no header comment."
  (or (when-let* (((<= pos (save-excursion
                             (goto-char pos)
                             (back-to-indentation)
                             (point))))
                  (producer (hel-producer 'comment))
                  (above (hel--comment-piece-on-next-line producer pos -1)))
        (car (hel--farthest-comment-piece producer above -1)))
      pos))

(defun hel--after-comments (pos)
  "Return the end of the comments and the whitespace from POS forward."
  (when-let* ((producer (hel-producer 'comment)))
    (while-let ((piece (hel-comment-piece-under producer pos))
                (end (cdr piece))
                ((< pos end)))
      (setq pos (hel-after-whitespace end 1))))
  pos)

(defun hel--thing-begin (thing instance)
  "Return the start of INSTANCE, a THING, with its header comment."
  (if (hel-thing-header-comment-p thing)
      (hel--header-comment-begin (hel-thing-instance-begin instance))
    (hel-thing-instance-begin instance)))

(cl-defmethod hel-thing-around (thing producer first last)
  (let* ((header-comment? (hel-thing-header-comment-p thing))
         (begin (hel--thing-begin thing first))
         (end   (hel-thing-instance-end last))
         (gap-end   (hel-thing-after-gap thing producer end 1))
         (gap-begin (hel-thing-after-gap thing producer begin -1))
         ;; The comments before the next thing follow the gap.
         (limit (if header-comment?
                    (hel--after-comments gap-end)
                  gap-end)))
    (or (when-let* ((next (hel-thing-sibling thing producer last 1 limit))
                    (next-begin (hel--thing-begin thing next))
                    (stop (if (<= next-begin gap-end)
                              next-begin
                            ;; A comment comes first. The gap leaves the
                            ;; indentation of the comment in place.
                            (let ((blanks (hel-after-blanks gap-end -1)))
                              (if (save-excursion (goto-char blanks) (bolp))
                                  blanks
                                gap-end))))
                    ((< end stop)))
          (cons begin stop))
        (when-let* ((previous (hel-thing-sibling thing producer first -1 gap-begin))
                    (previous-end (hel-thing-instance-end previous))
                    ((< previous-end begin))
                    ((<= gap-begin previous-end)))
          (cons previous-end end))
        (cons begin end))))

(cl-defmethod hel-thing-owning (thing producer pos)
  (cl-flet* ((contains-p (outer)
               (<= (car outer) pos (cdr outer)))
             (inside-p (inner outer)
               (and (not (equal inner outer))
                    (<= (car outer) (car inner) (cdr inner) (cdr outer)))))
    (let* ((candidates
            (->> (hel-thing-candidates thing producer pos)
                 ;; Keep one thing of each range.
                 (-group-by #'hel-thing-instance-bounds)
                 (-map #'cadr)
                 (-filter (lambda (instance)
                            (or (contains-p (hel-thing-instance-bounds instance))
                                (contains-p (hel-thing-around thing producer
                                                              instance instance)))))))
           (innermost
            (-remove (lambda (instance)
                       (-any? (lambda (other)
                                (inside-p (hel-thing-instance-bounds other)
                                          (hel-thing-instance-bounds instance)))
                              candidates))
                     candidates)))
      (or (-some->> innermost
            (-filter (lambda (instance)
                       (contains-p (hel-thing-instance-bounds instance))))
            (-max-by (-on #'> #'hel-thing-instance-begin)))
          (-some->> innermost
            (-filter (lambda (x) (<= (hel-thing-instance-end x) pos)))
            (-max-by (-on #'> #'hel-thing-instance-end)))
          (-some->> innermost
            (-min-by (-on #'> #'hel-thing-instance-begin)))))))

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
      (-some->> (hel-thing-candidates thing producer char)
        (-filter (lambda (instance)
                   (and (<= (hel-thing-instance-begin instance) char)
                        (< char (hel-thing-instance-end instance)))))
        (-min-by (-on #'> (-compose #'hel-range-length
                                    #'hel-thing-instance-bounds)))))))

(cl-defmethod hel-thing-edge (thing producer pos dir count edge whole)
  (let* ((forward? (< 0 dir))
         (ahead-p (lambda (instance)
                    (let ((target (hel-thing-motion-edge thing instance edge)))
                      (if forward? (< pos target) (< target pos)))))
         (here (when-let* ((instance (hel--thing-under thing producer pos dir))
                           ((funcall ahead-p instance)))
                 instance))
         (first (or here
                    (when-let* ((instance (hel-thing-next thing producer pos dir)))
                      ;; The next thing may have its EDGE at POS, as the
                      ;; paragraph that `{' starts from does: then take
                      ;; the thing after it.
                      (if (funcall ahead-p instance)
                          instance
                        (hel--thing-past thing producer instance dir)))))
         (last first)
         (found (if first 1 0))
         (buffer-edge (if forward? (point-max) (point-min))))
    (when first
      (cl-loop repeat (1- count)
               for instance = (hel--thing-past thing producer last dir)
               while instance
               do (setq last instance
                        found (1+ found))))
    (cond ((and (eq edge :begin) (not whole) (< found count))
           (unless (= pos buffer-edge)
             (hel-range pos buffer-edge)))
          (first
           (hel-range (if (and whole (not here))
                          (hel-thing-instance-edge first
                                                    (if forward? :begin :end))
                        pos)
                      (hel-thing-motion-edge thing last edge))))))

(cl-defmethod hel-thing-motion-edge (_thing instance edge)
  (hel-thing-instance-edge instance edge))

;;; Producers

(defun hel-treesit--node-bounds (node)
  "Return the (BEGIN . END) bounds of NODE."
  (cons (treesit-node-start node)
        (treesit-node-end node)))

(defun hel-treesit--node-length (node)
  "Return the length of NODE."
  (hel-range-length (hel-treesit--node-bounds node)))

;;;; The tree-sitter query producer

(cl-defstruct (hel-treesit-query-producer
               (:constructor hel-treesit-query-producer-create)
               (:copier nil))
  "The producer of the things that a text object query finds.
The parent of a thing is the parent node of its match."
  (capture nil :documentation "The capture name without its suffix, such as
\"function\"."))

(defun hel-treesit-active-p ()
  "Return non-nil if the current buffer has a tree-sitter parser."
  (and (treesit-available-p)
       (treesit-parser-list)
       t))

(defun hel-treesit-source (capture)
  "Return the source of the things of CAPTURE in the text object queries.
The source applies where `hel-treesit-text-objects-p' returns non-nil."
  (lambda ()
    (when (hel-treesit-text-objects-p)
      (hel-treesit-query-producer-create :capture capture))))

(cl-defmethod hel-thing-candidates (thing (producer hel-treesit-query-producer) pos)
  "Return the things that hold POS, and the things next to it.
A thing is next to POS when only whitespace and separators lie between
them. The thing after the comments behind POS counts too, because these
comments may be its header comment."
  (let ((capture (hel-treesit-query-producer-capture producer))
        (from (-> (hel-treesit--after-gap pos -1)
                  (1-)
                  (max (point-min))))
        (to   (-> (hel-treesit--after-gap pos 1)
                  (hel--after-comments)
                  (1+)
                  (min (point-max)))))
    (hel-treesit--capture-things thing capture from to)))

(cl-defmethod hel-thing-next
  (thing (producer hel-treesit-query-producer) pos dir)
  (let ((forward? (< 0 dir)))
    (when (if forward?
              (< pos (point-max))
            (< (point-min) pos))
      (hel-treesit--nearest-in-growing-ranges
       (-partial #'hel-treesit--capture-things
                 thing (hel-treesit-query-producer-capture producer))
       (if forward? (1+ pos) pos)
       (if forward? (point-max) (point-min))
       dir
       :key #'hel-thing-instance-bounds))))

(cl-defmethod hel-thing-sibling ( thing (producer hel-treesit-query-producer)
                                  instance dir &optional limit)
  "Skip the things that lie deeper than INSTANCE.
Things nest, so the nearest thing may lie deeper: the method of a class
comes between two top-level functions. The search stays inside the parent
node."
  (let* ((capture (hel-treesit-query-producer-capture producer))
         (parent (hel-thing-parent thing producer instance))
         (forward? (< 0 dir))
         (bound (if forward?
                    (hel-thing-instance-end instance)
                  (hel--header-comment-begin (hel-thing-instance-begin instance))))
         (far (cond (limit
                     (if forward?
                         (min (1+ limit) (point-max))
                       (max (1- limit) (point-min))))
                    ((treesit-node-p parent)
                     (if forward?
                         (treesit-node-end parent)
                       (treesit-node-start parent)))
                    (forward? (point-max))
                    (t (point-min)))))
    (hel-treesit--nearest-in-growing-ranges
     (lambda (begin end)
       (->> (hel-treesit--capture-things thing capture begin end)
            (-filter (lambda (instance)
                       (treesit-node-eq
                        (hel-thing-parent thing producer instance)
                        parent)))))
     bound far dir :key #'hel-thing-instance-bounds)))

(cl-defmethod hel-thing-after-gap
  (_thing (_producer hel-treesit-query-producer) pos dir)
  "Return the end of the whitespace and the separator nodes from POS in DIR.
A separator lies between two things and belongs to neither, as the comma
between two arguments."
  (hel-treesit--after-gap pos dir))

(defvar hel-treesit-queries-directory
  (->> (or load-file-name
           (bound-and-true-p byte-compile-current-file)
           buffer-file-name)
       (file-name-directory)
       (expand-file-name "queries/"))
  "The directory that holds the text object queries.
It holds a directory for each language, named after the language of its
tree-sitter parser. That directory holds the file `textobjects.scm'.")

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
argument of `treesit-query-capture', which exists only in Emacs 31 and later."
  (let ((cursor-pos (if (and (use-region-p)
                             (< (mark) (point)))
                        (1- (point))
                      (point))))
   (and (hel-treesit-active-p)
        hel-treesit--grouped-capture-p
        (hel-treesit--query (treesit-language-at cursor-pos))
        t)))

(defun hel-treesit--query (language)
  "Return the compiled text object query of LANGUAGE, or nil.
`hel-treesit--queries' keeps the result of each language. A query file that
cannot be split into patterns gives no query, and a message names the
language."
  (pcase (gethash language hel-treesit--queries 'unknown)
    ('unknown
     (let ((query (condition-case err
                      (hel-treesit--compile-query language)
                    (error
                     (message "Hel: no text objects for %s: %s"
                              language (error-message-string err))
                     nil))))
       (puthash language query hel-treesit--queries)))
    (query query)))

(defun hel-treesit--compile-query (language)
  "Compile the text object query of LANGUAGE and return it, or nil.
The query leaves out the patterns that do not compile, so that a node type
or a field that the installed grammar lacks does not break the whole query."
  (when-let* ((patterns (-some->> (hel-treesit--query-text language)
                          (hel-treesit--query-patterns)
                          (-filter (lambda (pattern)
                                     (ignore-errors
                                       (treesit-query-compile
                                        language pattern t)))))))
    (treesit-query-compile language (string-join patterns "\n") t)))

(defun hel-treesit--query-text (language)
  "Return the text of the query of LANGUAGE or nil.
The query is the file \"textobjects.scm\" of LANGUAGE, after the files
that it inherits. LANGUAGE is a symbol or, for an inherited file,
a string."
  (when-let* ((file (expand-file-name (format "%s/textobjects.scm" language)
                                      hel-treesit-queries-directory))
              ((file-readable-p file)))
    (let* ((text (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
           (inherited (when (string-match "^;+ *inherits: *\\(.*\\)$" text)
                        (split-string (match-string 1 text) "," t "[ \t]+"))))
      (concat (mapconcat (lambda (parent)
                           (or (hel-treesit--query-text parent) ""))
                         inherited "\n")
              "\n" text))))

(defun hel-treesit--query-patterns (query-text)
  "Split the QUERY-TEXT into its top-level patterns, a list of strings.
A pattern keeps the quantifiers and the captures that follow it."
  (with-temp-buffer
    (set-syntax-table hel-treesit--query-syntax-table)
    (insert query-text)
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

(defun hel-treesit--match (match around inside)
  "Return the plist of MATCH, or nil if it has no capture of the thing.
MATCH is a list of (CAPTURE . NODE) pairs. AROUND and INSIDE are the
capture names of the thing. The plist holds:

  :nodes     the nodes of those captures: all of them in a match with an
             `_extended' capture, which captures exactly the bounds of the
             thing, and the named ones in other matches;
  :node      the largest of these nodes;
  :extended  the node of the `_extended' capture."
  (cl-flet ((nodes (name)
              (-keep (-lambda ((capture . node))
                       (if (eq capture name) node))
                     match)))
    (when-let* ((nodes (or (nodes around)
                           (nodes inside))))
      (let* ((extended (car (nodes '_extended)))
             (kept (or (and (not extended)
                            (-filter (lambda (node)
                                       (treesit-node-check node 'named))
                                     nodes))
                       nodes)))
        (list :nodes kept
              :node (-max-by (-on #'> #'hel-treesit--node-length)
                             kept)
              :extended extended)))))

(defun hel-treesit--thing-matches (capture begin end)
  "Return the matches of CAPTURE that have nodes in BEGIN..END.
CAPTURE is a capture name of the queries without its suffix, such as
\"function\". Each match is a plist from `hel-treesit--match'.

A match with an `_extended' capture replaces the match whose node is its
`_extended' node. That match may have an `_extended' capture of its own,
so the replacements chain: an `export' statement replaces a `const'
declaration, which replaces the arrow function in it.
A match never replaces itself: in a pattern of siblings, such as a struct
and the `;' after it, the node of the match is its own `_extended' node."
  (when-let* ((node (treesit-node-on begin end))
              (parser (treesit-node-parser node))
              (query (hel-treesit--query (treesit-parser-language parser))))
    (let* ((around (intern (concat capture ".around")))
           (inside (intern (concat capture ".inside")))
           (root (treesit-parser-root-node parser))
           ;; The sixth argument — GROUPED — exists only in Emacs 31 and later.
           ;; `hel-treesit-text-objects-p' keeps this call from running on an
           ;; older Emacs, whose byte compiler would reject the argument count.
           (matches (->> (with-suppressed-warnings ((callargs treesit-query-capture))
                           (treesit-query-capture root query begin end nil t))
                         (-keep (lambda (match)
                                  (hel-treesit--match match around inside))))))
      (->> matches
           (-remove (lambda (match)
                      (->> matches
                           (-any? (lambda (other)
                                    (and (not (eq other match))
                                         (plist-get other :extended)
                                         (treesit-node-eq
                                          (plist-get other :extended)
                                          (plist-get match :node))))))))))))

(defun hel-treesit--capture-things (thing capture begin end)
  "Return the instances of THING that CAPTURE finds with nodes in BEGIN..END.
CAPTURE is the capture name in the query, which can differ from THING:
the thing `argument' reads the capture \"parameter\"."
  (->> (hel-treesit--thing-matches capture begin end)
       (-map (-partial #'hel-treesit--match-thing thing))))

(defun hel-treesit--match-thing (thing match)
  "Return the thing produced by MATCH from `hel-treesit--thing-matches'.
The thing spans the kept nodes of the match, so it leaves out the
anonymous nodes at the edges of a match without an `_extended' capture,
such as the trailing comma of an argument."
  (let ((nodes (plist-get match :nodes)))
    (hel-thing-instance-create thing
                               (cons (-min (-map #'treesit-node-start nodes))
                                     (-max (-map #'treesit-node-end nodes)))
                               (treesit-node-parent (plist-get match :node)))))

(defvar hel-treesit--search-range-size 4096
  "The size in characters of the first range of a text object search.")

(defun hel-treesit--search-in-growing-ranges (fn pos limit)
  "Run FN on ranges that start at POS and double in size, up to LIMIT.
Return the first non-nil value of FN, or nil. FN takes the BEGIN and
END of a range. A small range comes first, because a query over it is
fast and what the caller looks for usually lies near POS."
  (let ((size hel-treesit--search-range-size)
        result)
    (while (let ((far (if (<= pos limit)
                          (min limit (+ pos size))
                        (max limit (- pos size)))))
             (setq result (funcall fn (min pos far)
                                   (max pos far)))
             (setq size (* 2 size))
             (and (not result)
                  (/= far limit))))
    result))

(cl-defun hel-treesit--nearest-in-growing-ranges ( find-all pos limit dir
                                                   &key (key #'identity))
  "Return what lies nearest to POS in direction DIR, or nil.
FIND-ALL is the search of the caller: a function that takes the BEGIN
and END of a range and returns all it finds there, such as thing
instances or ranges. It runs first on a small range at POS, and the
range grows toward LIMIT until something lies in it. Forward, the
result starts at or after POS. Backward, it ends at or before POS.
KEY returns the (BEGIN . END) of a result, and it is `identity' by
default."
  (hel-treesit--search-in-growing-ranges
   (lambda (begin end)
     (let ((items
            (->> (funcall find-all begin end)
                 (-filter (lambda (item)
                            (-let [(item-begin . item-end) (funcall key item)]
                              (if (< 0 dir)
                                  (and (<= pos item-begin) (< item-begin end))
                                (and (< begin item-end) (<= item-end pos)))))))))
       (hel--nearest items dir :key key)))
   pos limit))

(cl-defun hel--nearest (items direction &key (key #'identity))
  "Return the first of ITEMS in DIRECTION, or nil.
KEY maps ITEMS to (BEGIN . END) ranges; by default it is `identity'.
DIRECTION is 1 or -1. When two ranges share the nearest edge, return
the item with the larger range."
  (cond ((null items) nil)
        ((< 0 direction)
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

(defun hel-treesit--after-gap (pos dir)
  "Return the end of the whitespace and separators from POS in DIR."
  (save-excursion
    (goto-char pos)
    (while (let ((old (point)))
             (goto-char (hel-after-whitespace (point) dir))
             (when-let* ((end (hel-treesit--separator-end (point) dir)))
               (goto-char end))
             (/= old (point))))
    (point)))

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

(defun hel--motion-thing (thing range)
  "Return the instance of THING that a motion passed over, or nil.
A motion can stop in the whitespace around a thing, so the whitespace at
the edges of RANGE is not part of the result. A RANGE of only whitespace
gives nil."
  (when-let* ((trimmed (hel-trim-whitespace-from-range range)))
    (hel-thing-instance-create thing trimmed)))

(defun hel--motion-at (thing producer pos)
  "Return the THING that holds POS, or nil.
To find the start of the thing, the motion of PRODUCER goes to its end and
back. The first backward move alone can stop elsewhere: from a header
comment, `beginning-of-defun' goes back to the function before it."
  (let ((motion (hel-motion-producer-motion producer)))
    (when-let* ((instance (save-excursion
                            (goto-char (min (1+ pos) (point-max)))
                            (and (zerop (funcall motion -1))
                                 (zerop (funcall motion 1))
                                 (let ((end (point)))
                                   (funcall motion -1)
                                   (hel--motion-thing thing (cons (point) end))))))
                ((<= (hel-thing-instance-begin instance) pos))
                ((<= pos (hel-thing-instance-end instance))))
      instance)))

(cl-defmethod hel-thing-candidates (thing (producer hel-motion-producer) pos)
  (-non-nil (list (hel--motion-at thing producer pos)
                  (hel-thing-next thing producer pos -1)
                  (hel-thing-next thing producer pos 1))))

(cl-defmethod hel-thing-next (thing (producer hel-motion-producer) pos dir)
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
                 (instance (hel--motion-thing thing (hel-range stop other))))
            (when (and instance
                       (if (< 0 dir)
                           (< pos (hel-thing-instance-begin instance))
                         (<= (hel-thing-instance-end instance) pos)))
              (setq result instance))))
        result))))

;;; Drivers

(defun hel--text-object-position ()
  "Return the position where a text object command looks for its object.
With a selection, this is the character under the cursor, without the
whitespace at the edges of the selection. Without one, this is point."
  (-if-let ((begin . end) (hel-trimmed-selection-bounds))
      (if (< 0 (hel-region-direction))
          (1- end)
        begin)
    (point)))

(defun hel--mark-text-object (thing kind count)
  "Select COUNT THINGs at the cursor, inner or around as KIND says.
When the previous call for THING and KIND made this selection, add COUNT
more things. A negative COUNT adds things backward.

Point stands where the selection changed. When the command selects one
thing, the selection takes the direction of `hel-thing-selection-direction'.
When it adds things, on a repeat or with a COUNT above 1 or below -1, point
goes to the edge where they were added.

KIND is:
- `:inner' for what `mi' selects;
- `:around' for what `ma' selects."
  (if-let* ((producer (hel-producer thing)))
      (progn
        (hel--sync-selection-history)
        (-let* ((key (list thing kind))
                (dir (hel-sign count))
                (before (hel--selection-before))
                ((_ _ (run-key run-first run-last))
                 (car hel-selection-history))
                (extend? (equal key run-key))
                ((first . last)
                 (if extend?
                     (hel--extend-things thing producer run-first run-last count)
                   (when-let* ((instance (hel-thing-owning
                                          thing producer
                                          (hel--text-object-position))))
                     (hel--extend-things thing producer instance instance
                                         (- count dir)))))
                (range (when first
                         (pcase kind
                           (:inner (cons (hel-thing-instance-begin first)
                                         (hel-thing-instance-end last)))
                           (_ (hel-thing-around thing producer first last))))))
          (when (and range
                     (not (equal range (car before))))
            (hel-push-point)
            (hel-select range (if (or extend? (/= 1 (abs count)))
                                  dir
                                (hel-thing-selection-direction thing)))
            (hel--record-selection before (list key first last))
            (hel-reveal-point-when-on-top))))
    (hel--no-producer-message thing)))

(defun hel--mark-to-thing-edge (thing count edge whole)
  "Select from point to the EDGE of the COUNT-th THING.
A negative COUNT goes backward. EDGE and WHOLE are as in `hel-thing-edge'.
With the selection extended, extend it. When no thing exists in the direction,
the selection stays as it is."
  (if-let* ((producer (hel-producer thing)))
      (let ((dir (hel-sign count)))
        (-when-let ((begin . end) (hel-thing-edge thing producer (point)
                                                  dir (abs count) edge whole))
          (when (hel-thing-jump-p thing) (hel-push-point))
          (when (< dir 0) (cl-rotatef begin end))
          (hel-select-motion begin end)
          (hel-reveal-point-when-on-top)))
    ;; else
    (hel--no-producer-message thing)))

;;; Things
;;;; Words

(put 'word 'hel-thing-jump nil)
(put 'word 'hel-thing-sources (list (hel-motion-source #'hel--forward-word)))

(put 'WORD 'hel-thing-jump nil)
(put 'WORD 'hel-thing-sources (list (hel-motion-source #'hel--forward-WORD)))

(cl-defmethod hel-thing-after-gap ((_thing (eql 'word)) _producer pos dir)
  "Return the end of the blanks from POS in DIR.
A word gap stays on its line."
  (hel-after-blanks pos dir))

(cl-defmethod hel-thing-after-gap ((_thing (eql 'WORD)) _producer pos dir)
  "Return the end of the blanks from POS in DIR.
A WORD gap stays on its line."
  (hel-after-blanks pos dir))

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
        (progn
          (hel--backward-word-end motion count)
          (hel-range pos (point)))
      ;; else
      (hel-skip-chars "\r\n" dir)
      (let ((start (point)))
        (pcase (list dir edge)
          ('(1 :begin)
           ;; The blanks before the next word are the first step.
           (when (hel-blank-p (following-char))
             (cl-decf count))
           (funcall motion count)
           (hel-skip-blanks 1))
          ('(1 :end)
           (funcall motion count))
          (_
           (funcall motion (- count))))
        (hel-range start (point))))))

(defun hel--backward-word-end (motion count)
  "Move point back to the end of the COUNT-th previous word of MOTION.
A line start and the blanks before a word are steps of their own."
  (hel-motion-loop (dir count)
    (cond ((and (bolp) (not (bobp)))
           (skip-chars-backward "\r\n"))
          ((hel-blank-p (preceding-char))
           (hel-skip-blanks -1))
          (t
           (funcall motion -1)
           (hel-skip-blanks -1)))))

;;;; Sentences

(put 'sentence 'hel-thing-sources
     (list (hel-motion-source #'hel--forward-sentence)))

(cl-defmethod hel-thing-parent ((_thing (eql 'sentence)) _producer sentence)
  "Return the start of the paragraph the SENTENCE belongs to."
  (save-excursion
    (goto-char (min (1+ (hel-thing-instance-begin sentence))
                    (point-max)))
    (hel--forward-paragraph -1)
    (point)))

;;;; Paragraphs

(put 'paragraph 'hel-thing-linewise t)
(put 'paragraph 'hel-thing-selection-direction -1)
(put 'paragraph 'hel-thing-sources (list (hel-motion-source
                                          #'hel--forward-paragraph)))

;;;; Comments

(put 'comment 'hel-thing-selection-direction -1)
(put 'comment 'hel-thing-sources (list #'hel-treesit-comment-source
                                       #'hel-syntax-comment-source))

(cl-defstruct (hel-comment-producer (:constructor nil) (:copier nil))
  "The producer of comments.
Its models find the pieces of comments, and the producer joins the pieces
into comments.")

(cl-defstruct (hel-syntax-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-syntax-comment-source)
               (:copier nil))
  "The producer of the comments that the syntax table finds.")

(cl-defstruct (hel-treesit-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-treesit-comment-producer-create)
               (:copier nil))
  "The producer of the comments that a tree-sitter query finds.")

(defun hel-treesit-comment-source ()
  "Return the source of the comments found with the tree-sitter queries."
  (when (hel-treesit-text-objects-p)
    (hel-treesit-comment-producer-create)))

(cl-defgeneric hel-comment-piece-under (producer pos)
  "Return the bounds of the comment piece that holds the character at POS.
A piece is what the parser or the syntax table calls one comment: a line
of a `//' comment, or a whole `/* */' comment. The bounds leave out the
whitespace at the edges of the piece. Return nil when POS is in no piece.
PRODUCER finds the pieces.")

(cl-defgeneric hel-comment-piece-next (producer pos dir)
  "Return the bounds of the next comment piece from POS in DIR, or nil.
Forward, the piece starts at or after POS. Backward, it starts before POS,
so it can be the piece that holds POS. PRODUCER finds the pieces.")

(cl-defgeneric hel-comment-piece-parent (_producer _pos)
  "Return the parent of the comment whose first piece starts at POS.
Two comments with equal parents form one run, so a comment in the body of
a function and a comment after the function stay apart. PRODUCER finds
the pieces.

By default, comments have no parent, and parents keep no comments apart."
  nil)

(defun hel-bounds-of-comment-at (pos)
  "Return the bounds of the comment at POS, or nil.
POS can be inside the comment or on its edge, and also on the blanks
around it or on the line end after it. The bounds are what `mic' selects:
the whole lines of the comment with the line end of the last one, or only
the comment when it follows code."
  (-some-> (hel-producer 'comment)
    (hel--bounds-of-comment-at pos)))

(defun hel--bounds-of-comment-at (producer pos)
  "Return the bounds of the comment at POS, or nil.
POS can also be on the blanks around the comment or on the line end after
it. PRODUCER finds the pieces of the comments."
  (when-let*
      ((piece (or (hel-comment-piece-under producer pos)
                  (let ((text-begin (hel-after-blanks pos 1))
                        (text-end   (hel-after-blanks pos -1)))
                    (save-excursion
                      (cond ((progn (goto-char text-end)
                                    (bolp))
                             (hel-comment-piece-under producer text-begin))
                            ((progn (goto-char text-begin)
                                    (eolp))
                             (hel-comment-piece-under producer (1- text-end)))))))))
    (hel--comment-from-piece producer piece)))

(defun hel--comment-from-piece (producer piece)
  "Return the bounds of the comment that holds comment PIECE.
A comment is a sequence of pieces, each of which fills its line, with no
blank line between them. It covers whole lines, as a paragraph does, with
the line end of its last line. A piece after code is a comment of its own,
and it covers only the piece. PIECE is the bounds of a piece, and PRODUCER
finds the pieces."
  (let* ((whole-lines? (hel-only-text-on-lines-p piece))
         (first (if whole-lines?
                    (hel--farthest-comment-piece producer piece -1)
                  piece))
         (last  (if whole-lines?
                    (hel--farthest-comment-piece producer piece 1)
                  piece)))
    (if whole-lines?
        (hel-line-bounds (cons (car first) (cdr last)))
      piece)))

(defun hel--comment-thing (producer comment)
  "Return COMMENT as a thing, with the parent that PRODUCER knows.
COMMENT is the bounds of a comment. Its first piece starts at its first
character that is not a blank."
  (-let [(begin . end) comment]
    (hel-thing-instance--create
     :begin begin
     :end end
     :parent (hel-comment-piece-parent producer (hel-after-blanks begin 1)))))

(cl-defmethod hel-thing-candidates (thing (producer hel-comment-producer) pos)
  (-non-nil (list (-some->> (hel--bounds-of-comment-at producer pos)
                    (hel--comment-thing producer))
                  (hel-thing-next thing producer pos -1)
                  (hel-thing-next thing producer pos 1))))

(cl-defmethod hel-thing-next (_thing (producer hel-comment-producer) pos dir)
  (let (result)
    (while-let (((not result))
                (piece (hel-comment-piece-next producer pos dir))
                (comment (hel--comment-from-piece producer piece))
                (far-edge (if (< 0 dir) (cdr comment) (car comment)))
                ((/= pos far-edge)))
      (if (if (< 0 dir)
              (< pos (car comment))
            (<= (cdr comment) pos))
          (setq result comment)
        (setq pos far-edge)))
    (when result
      (hel--comment-thing producer result))))

(cl-defmethod hel-comment-piece-under ((_producer hel-treesit-comment-producer)
                                      pos)
  (-some-> (hel-treesit--comment-node-under pos)
    (hel-treesit--node-bounds)
    (hel-trim-whitespace-from-range)))

(cl-defmethod hel-comment-piece-parent ((_producer hel-treesit-comment-producer)
                                       pos)
  (-some-> (hel-treesit--comment-node-under pos)
    (treesit-node-parent)))

(cl-defmethod hel-comment-piece-next ((producer hel-treesit-comment-producer)
                                     pos dir)
  (-some->> (hel-treesit--nearest-in-growing-ranges
             (lambda (begin end)
               (->> (hel-treesit--comment-nodes begin end)
                    (-map #'hel-treesit--node-bounds)))
             pos
             (if (< 0 dir) (point-max) (point-min))
             dir)
    (car)
    (hel-comment-piece-under producer)))

(defun hel-treesit--comment-node-under (pos)
  "Return the smallest comment node that holds the character at POS, or nil."
  (-some->> (hel-treesit--comment-nodes pos (min (1+ pos) (point-max)))
    (-filter (lambda (node)
               (and (<= (treesit-node-start node) pos)
                    (< pos (treesit-node-end node)))))
    (-min-by (-on #'> #'hel-treesit--node-length))))

(defun hel-treesit--comment-nodes (begin end)
  "Return the comment nodes that the text object query finds in BEGIN..END."
  (->> (hel-treesit--thing-matches "comment" begin end)
       (-mapcat (lambda (match) (plist-get match :nodes)))))

(cl-defmethod hel-comment-piece-under ((_producer hel-syntax-comment-producer)
                                      pos)
  "Find the piece with `forward-comment' from one of three starts.
The start is the comment start that `syntax-ppss' reports, POS itself on
the first delimiter character, or the character before POS on the second
one. The end of the buffer closes a comment that has no end of its own."
  (save-excursion
    (cl-loop
     for begin in (list (if-let* ((ppss (syntax-ppss pos))
                                  ((nth 4 ppss)))
                            (nth 8 ppss))
                        pos
                        (1- pos))
     thereis
     (when-let* ((begin)
                 ((<= (point-min) begin pos))
                 ((not (memq (char-after begin) '(nil ?\s ?\t ?\n ?\r))))
                 ((not (nth 3 (syntax-ppss begin))))
                 ;; A comment that the buffer end closes moves point to the
                 ;; end, but `forward-comment' returns nil.
                 ((progn (goto-char begin)
                         (or (forward-comment 1)
                             (eobp))))
                 (end (cdr (hel-trim-whitespace-from-range (cons begin (point)))))
                 ((< pos end)))
       (cons begin end)))))

(cl-defmethod hel-comment-piece-parent ((_producer hel-syntax-comment-producer)
                                       pos)
  "Return the start of the innermost bracket pair around POS, or nil."
  (save-excursion
    (nth 1 (syntax-ppss pos))))

(cl-defmethod hel-comment-piece-next ((producer hel-syntax-comment-producer)
                                     pos dir)
  (comment-normalize-vars t)
  (save-excursion
    (goto-char pos)
    (if (< 0 dir)
        (when-let* ((start (comment-search-forward (point-max) t)))
          (hel-comment-piece-under producer start))
      ;; else
      (when-let* ((start (comment-search-backward (point-min) t))
                  ((< start pos)))
        (hel-comment-piece-under producer start)))))

(cl-defmethod hel-thing-after-gap ((_thing (eql 'comment)) _producer pos dir)
  "Return the end of the gap next to a comment, from POS in DIR.
Next to a comment of whole lines, the gap is the blank lines. Next to a
comment that follows code, the gap is the blanks on that line, so the line
of code never joins the next line."
  (if (save-excursion (goto-char pos) (bolp))
      (hel-after-whitespace pos dir)
    (hel-after-blanks pos dir)))

(cl-defmethod hel-thing-motion-edge ((_thing (eql 'comment)) instance edge)
  "A motion to the start of a comment stops after its indentation."
  (let ((pos (hel-thing-instance-edge instance edge)))
    (if (eq edge :begin)
        (hel-after-blanks pos 1)
      pos)))

(cl-defmethod hel-thing-around ((thing (eql 'comment)) producer first last)
  "Return the around range of the comments from FIRST to LAST.
A comment after code has the spaces and tabs before it as its gap when no
gap follows it, so that `mac' and `d' leave no blanks at the end of the
line of code."
  (-let [(range &as begin . end) (cl-call-next-method)]
    (if (and (equal range (cons (hel-thing-instance-begin first)
                                (hel-thing-instance-end last)))
             (not (save-excursion
                    (goto-char begin)
                    (bolp))))
        (cons (hel-thing-after-gap thing producer begin -1)
              end)
      range)))

(defun hel--farthest-comment-piece (producer piece dir)
  "Return the last piece of the comment lines from PIECE in direction DIR.
Go from line to line while the next line holds a piece that fills it.
Return PIECE when the next line holds none. PIECE and the result are the
bounds of pieces, and PRODUCER finds the pieces."
  (while-let ((next (hel--comment-piece-on-next-line
                     producer (if (< dir 0) (car piece) (cdr piece)) dir)))
    (setq piece next))
  piece)

(defun hel--comment-piece-on-next-line (producer pos dir)
  "Return the bounds of the piece that fills the line next to POS in DIR.
Return nil when that line holds no such piece.
The next line is the line after the line of POS, or the line before it
when DIR is negative. PRODUCER finds the pieces."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (and (not (if (< dir 0) (bobp) (eobp)))
               (zerop (forward-line dir))
               ;; From a last line without a newline, `forward-line' reports
               ;; success and stays on that line.
               (bolp)
               (progn (back-to-indentation)
                      (not (eolp))))
      (let ((pos (if (< 0 dir)
                     (point)
                   (end-of-line)
                   (hel-skip-blanks -1)
                   (1- (point)))))
        (when-let* ((other (hel-comment-piece-under producer pos))
                    ((hel-only-text-on-lines-p other)))
          other)))))

;;;; Functions

(put 'function 'hel-thing-header-comment t)
(put 'function 'hel-thing-linewise t)
(put 'function 'hel-thing-selection-direction -1)
(put 'function 'hel-thing-sources (list
                                   (hel-treesit-source "function")
                                   (hel-motion-source #'hel--forward-function)))

(cl-defmethod hel-thing-parent ((_thing (eql 'function))
                                    (_producer hel-motion-producer) instance)
  "Return the start of the bracket pair around INSTANCE.
A function that a motion finds has no parent node. Its run stays inside
the bracket pair around it, as a sexp does."
  (->> (save-excursion (syntax-ppss (hel-thing-instance-begin instance)))
       (nth 1)))

;;;; Classes

(put 'class 'hel-thing-header-comment t)
(put 'class 'hel-thing-linewise t)
(put 'class 'hel-thing-selection-direction -1)
(put 'class 'hel-thing-sources (list (hel-treesit-source "class")))

;;;; Arguments

(put 'argument 'hel-thing-header-comment t)
(put 'argument 'hel-thing-sources (list (hel-treesit-source "parameter")))

;;; .
(provide 'hel-text-objects)
;;; hel-text-objects.el ends here
