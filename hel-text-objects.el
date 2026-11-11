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
;;   `hel-word-motion', a tree-sitter query, or a finder of comments.
;; - Which rules hold for a kind of thing depends on the thing: a word
;;   gap stays on its line, a paragraph covers whole lines, a function
;;   owns the comment above it. A rule that is a fixed fact, such as the
;;   last two, is a symbol property of the thing.
;;
;; The producers in this file give things one after another
;; (`hel-thing-at', `hel-thing-next'). The text object keys and the
;; motions need nothing more. A tree producer knows the parent and the
;; children of a node instead, and `hel-structural-editing.el' defines it.
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

(cl-defun hel-sexp-motion (&optional (count 1))
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
Hel binds `forward-sexp-function' to this function during its own sexp motions.
Unlike the default `forward-sexp' behavior, it does not move to the end of the
buffer when there is no sexp ahead.

Signal `scan-error' when there is no sexp to move over. The error arguments
follow the format used by `forward-sexp'."
  (if-let* ((pos (scan-sexps (point) count)))
      (progn
        (goto-char pos)
        (if (< count 0) (backward-prefix-chars)))
    (signal 'scan-error (if (< count 0)
                            (list "No previous sexp" (point-min) (point))
                          (list "No next sexp" (point) (point-max))))))

;;; The protocol
;;;; The thing instance
;;
;; A producer returns each thing that it finds as a `hel-thing-instance':
;; the bounds of the thing and the data that the producer keeps about it.

(cl-defstruct (hel-thing-instance
               (:constructor hel-thing-instance-create ( range &key data
                                                         &aux (begin (car range))
                                                              (end (cdr range))))
               (:copier nil))
  "A thing that a producer found.
`hel-thing-instance-create' takes the (BEGIN . END) bounds RANGE of the thing
and key `:data', which can holds arbitrary data that producer stores in it.
Two instances with equal bounds are the same thing: DATA is not a part of
the value."
  (begin  nil :documentation "The start of the thing.")
  (end    nil :documentation "The end of the thing.")
  (data   nil :documentation "\
What the producer that found the thing keeps about it.
Only the methods of that producer read it."))

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

;;;; The rules of a thing

(defun hel-thing-owns-header-comment-p (thing)
  "Return non-nil if the comment lines above a THING belong to it.
The property `hel-thing-owns-header-comment' of THING holds the answer."
  (get thing 'hel-thing-owns-header-comment))

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

(defun hel-thing-maybe-expand-bounds-to-full-lines (thing bounds)
  "Return BOUNDS of the THING expanded to full lines when THING is linewise.
A linewise THING has the property `hel-thing-linewise'. BOUNDS expands
only when THING holds the only text on its lines, and then runs from the
start of the first line to the end of the last line, with the line end.
BOUNDS is a (BEGIN . END) pair."
  (if (and (get thing 'hel-thing-linewise)
           (hel-only-text-on-lines-p bounds))
      (hel-line-bounds bounds)
    bounds))

(defun hel-thing-maybe-trim-punctuation (thing bounds)
  "Return BOUNDS of the THING without the punctuation at its edges.
The property `hel-thing-trim-punctuation' of THING is a string of the
punctuation characters to trim, and the BOUNDS of a THING without this
property stay as they are. A bracket or a quote at an edge stays when its
pair is inside BOUNDS, see `hel-trim-punctuation-from-range'.
BOUNDS is a (BEGIN . END) pair."
  (if-let* ((chars (get thing 'hel-thing-trim-punctuation)))
      (hel-trim-punctuation-from-range bounds chars)
    bounds))

(defun hel-thing-apply-rules (thing bounds)
  "Return the BOUNDS of a THING as the user sees them.
A producer finds where a THING is in the text, and the rules of THING
then fix its edges: the punctuation at the edges goes, see
`hel-thing-maybe-trim-punctuation', and a linewise THING takes its whole
lines, see `hel-thing-maybe-expand-bounds-to-full-lines'. Every producer
passes the bounds that it found through this function, before it compares
them with a position. BOUNDS is a (BEGIN . END) pair."
  (->> bounds
       (hel-thing-maybe-trim-punctuation thing)
       (hel-thing-maybe-expand-bounds-to-full-lines thing)))

(cl-defgeneric hel-thing-after-gap (thing producer pos dir)
  "Return the position after the gap between THINGs from POS in DIR, or nil.
A gap is the text between two THINGs, such as whitespace. Return nil
when no gap separates THINGs here: the gap is empty, or it ends at
a border. A border closes a run of THINGs, and no THING of the run
lies past it. See `hel--reached-border-p'. PRODUCER finds the things.

By default, the gap is the whitespace from POS. When THING has the
property `hel-thing-line-end-is-border', the gap is the blanks from POS,
and a line end is a border too.")

(cl-defmethod hel-thing-after-gap (thing _producer pos dir)
  (let* ((line-end-is-border? (get thing 'hel-thing-line-end-is-border))
         (end (if line-end-is-border?
                  (hel-after-blanks pos dir)
                (hel-after-whitespace pos dir))))
    (unless (or (= end pos)
                (hel--reached-border-p end dir line-end-is-border?))
      end)))

(defun hel--reached-border-p (pos dir &optional line-end-is-border?)
  "Return non-nil if POS has reached a border in direction DIR.
A border is an edge of the buffer, a bracket, or a line end when
LINE-END-IS-BORDER? is non-nil. Forward, the border is the text after
POS, and backward the text before it."
  (save-excursion
    (goto-char pos)
    (if (< dir 0)
        (or (bobp)
            (eq ?\( (char-syntax (char-before)))
            (and line-end-is-border? (bolp)))
      (or (eobp)
          (eq ?\) (char-syntax (char-after)))
          (and line-end-is-border? (eolp))))))

(cl-defgeneric hel-thing-parent (thing producer instance)
  "Return the parent of INSTANCE of THING that PRODUCER found.
A text object with a count, such as `2 maf', selects several things in
a row, and only things with the same parent. Two things with the same
parent are siblings. Things with a nil parent are siblings of
each other.")

(cl-defmethod hel-thing-parent (_thing _producer _instance)
  "By default, the parent is nil, so all THINGs are siblings."
  nil)

;;;; The finding operations

(cl-defgeneric hel-thing-at (thing producer pos)
  "Return the THING under the cursor at POS, or nil.
This is the THING that holds the character at POS. When no THING holds
it, it is the THING that ends at POS. Of nested THINGs, return the
innermost one. PRODUCER finds the things.")

(cl-defgeneric hel-thing-next (thing producer pos dir)
  "Return the nearest THING from POS in direction DIR, or nil.
Forward, the thing starts after the character at POS. Backward, it ends
at or before POS. Of two things that start, or end, at the same place,
return the larger one. PRODUCER finds the things.")

(cl-defgeneric hel-thing-candidates (thing producer pos)
  "Return the THINGs that a text object command can select from POS.
These are the things that hold POS, and the neighbours whose gap or
header comment may hold it. PRODUCER finds the things.

By default, the candidates are the THING at POS, see `hel-thing-at', and
the nearest THING on each side of POS, see `hel-thing-next'."
  (-non-nil (list (hel-thing-at thing producer pos)
                  (hel-thing-next thing producer pos -1)
                  (hel-thing-next thing producer pos 1))))

;;; Algorithms

(cl-defgeneric hel-thing-sibling (thing producer instance dir)
  "Return the sibling of INSTANCE in direction DIR, or nil.
The sibling is the nearest THING in DIR that has the same parent as
INSTANCE, see `hel-thing-parent'.

By default, the sibling is the next THING after INSTANCE, and the result
is nil when that thing has another parent."
  (when-let* ((next (hel--thing-past thing producer instance dir))
              ((equal (hel-thing-parent thing producer instance)
                      (hel-thing-parent thing producer next))))
    next))

(cl-defgeneric hel-thing-owning (thing producer pos)
  "Return the THING that owns the position POS, or nil.
A thing owns its body, its header comment and the gap next to it.

Of nested things, the innermost one wins. When POS is on the boundary
between two things, the one after wins. PRODUCER finds the things."
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
            (->> candidates
                 (-remove (lambda (instance)
                            (-any? (lambda (other)
                                     (inside-p (hel-thing-instance-bounds other)
                                               (hel-thing-instance-bounds instance)))
                                   candidates))))))
      (or (-some->> innermost
            (-filter (lambda (instance)
                       (contains-p (hel-thing-instance-bounds instance))))
            (-max-by (-on #'> #'hel-thing-instance-begin)))
          (-some->> innermost
            (-filter (lambda (instance)
                       (<= (hel-thing-instance-end instance) pos)))
            (-max-by (-on #'> #'hel-thing-instance-end)))
          (-some->> innermost
            (-min-by (-on #'> #'hel-thing-instance-begin)))))))

(cl-defgeneric hel-thing-around (thing producer first last)
  "Return what an around text object selects for the THINGs from FIRST to LAST.
This is the header comment of FIRST, the things, and the gap after LAST.
When no gap follows LAST, it takes the gap before FIRST instead. So
deleting the around range leaves no separator and no blank line before
a border, such as the closing bracket. PRODUCER finds the things.

A gap is the text that `hel-thing-after-gap' skips. When the things fill
their lines, the gap is whole lines too, so it leaves the indentation of
the text after it in place."
  (let ((begin (hel--thing-begin thing first))
        (end (hel-thing-instance-end last)))
    (cl-flet ((after-gap (pos dir)
                ;; From a line start, the gap ends at a line start too,
                ;; unless it ends at the edge of the buffer.
                (when-let* ((edge (hel-thing-after-gap thing producer pos dir)))
                  (save-excursion
                    (goto-char pos)
                    (when (and (bolp)
                               (/= edge (if (< 0 dir) (point-max) (point-min))))
                      (goto-char edge)
                      (setq edge (line-beginning-position (if (< 0 dir) 1 2)))))
                  (unless (= edge pos)
                    edge))))
      (if-let* ((gap-end (after-gap end 1)))
          (cons begin gap-end)
        (cons (or (after-gap begin -1) begin)
              end)))))

(cl-defgeneric hel-thing-motion-range (thing producer pos count edge whole?)
  "Return the range from POS to the EDGE of the COUNT-th THING.
A negative COUNT goes backward. With WHOLE non-nil and POS outside the first
thing, the range starts at the near edge of that thing, so it holds the whole
thing. With fewer than COUNT things, the range goes to the last of them; with
EDGE `:begin' and WHOLE nil, it goes to the buffer edge. Return nil when no
thing lies in that direction, or when POS is already at the buffer edge.

EDGE is `:begin' or `:end'. PRODUCER finds the things.

By default, the first thing is the innermost one under the cursor at POS when
its EDGE lies ahead of POS, and the next thing ahead otherwise. The search for
each next thing starts at the far edge of the previous one. The range starts at
POS, or at the near edge of the first thing when WHOLE is non-nil and POS lies
outside that thing. It ends at the EDGE of the last thing."
  (let* ((dir (hel-sign count))
         (count (abs count))
         (ahead-p (lambda (instance)
                    (let ((target (hel-thing-instance-edge instance edge)))
                      (if (< dir 0)
                          (< target pos)
                        (< pos target)))))
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
         (buffer-edge (if (< dir 0) (point-min) (point-max))))
    (when first
      (cl-loop repeat (1- count)
               for instance = (hel--thing-past thing producer last dir)
               while instance
               do (setq last instance
                        found (1+ found))))
    (cond ((and (eq edge :begin)
                (not whole?)
                (< found count))
           (unless (= pos buffer-edge)
             (hel-range pos buffer-edge)))
          (first
           (hel-range (if (and whole? (not here))
                          (hel-thing-instance-edge first (if (< 0 dir) :begin :end))
                        pos)
                      (hel-thing-instance-edge last edge))))))

(defun hel--thing-past (thing producer instance dir)
  "Return the nearest THING past INSTANCE in direction DIR, or nil."
  (hel-thing-next thing producer
                  (if (< 0 dir)
                      (1- (hel-thing-instance-end instance))
                    (hel-thing-instance-begin instance))
                  dir))

(defun hel--extend-things (thing producer first last count)
  "Add COUNT THINGs to the selected things from FIRST to LAST.
A positive COUNT adds them after LAST, a negative one before FIRST.
When fewer things exist, add the ones that exist. Return (FIRST . LAST)."
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
Forward, the cursor is the character after POS, and backward the character
before it."
  (let ((char (if (< 0 dir) pos (1- pos))))
    (when (and (<= (point-min) char) (< char (point-max)))
      (when-let* ((instance (hel-thing-at thing producer char))
                  ((< char (hel-thing-instance-end instance))))
        instance))))

(defvar hel--search-range-size 4096
  "The size in characters of the first range of a text object search.")

(cl-defun hel--find-nearest
    (seeker pos dir &key (limit (if (< dir 0) (point-min) (point-max)))
                         (key #'identity))
  "Return the item nearest to POS in direction DIR, or nil.
Forward, the item starts at or after POS. Backward, it ends at or before
POS. Of two items that start, or end, at the same place, return the larger
one. The item lies before LIMIT, which is the edge of the buffer by default.

A tree-sitter query, or a scan of the bracket pairs, cannot find the next
item in a direction: it lists all the items in a range. So this function
runs SEEKER on a window that starts at POS and doubles toward LIMIT, until
the window holds an item. The nearest item in the window is final, because
every item outside the window starts farther from POS.

SEEKER takes the BEGIN and END of a window and returns every item whose
near edge lies in the window. KEY maps an item to its (BEGIN . END) bounds."
  (let ((size hel--search-range-size)
        (far pos)
        result)
    (cl-flet* ((near-edge (item)
                 (-let [(begin . end) (funcall key item)]
                   (if (< 0 dir) begin end)))
               (rank (item)
                 (-let [(begin . end) (funcall key item)]
                   (cons (* dir (near-edge item))
                         (- begin end))))
               (in-window-p (item)
                 ;; POS <= edge < FAR forward, FAR < edge <= POS backward.
                 (let ((edge (near-edge item)))
                   (and (<= 0 (* dir (- edge pos)))
                        (< 0 (* dir (- far edge))))))
               (farther-p (a b)
                 ;; The rank is the signed near edge, then the negative
                 ;; length: the smaller rank is nearer, or larger.
                 (-let (((a-edge . a-size) (rank a))
                        ((b-edge . b-size) (rank b)))
                   (or (> a-edge b-edge)
                       (and (= a-edge b-edge) (> a-size b-size))))))
      (while (and (not result) (/= far limit))
        (setq far (if (< 0 dir)
                      (min limit (+ pos size))
                    (max limit (- pos size))))
        (setq size (* 2 size))
        ;; `-min-by' fails on an empty list, so use `-some->>' to prevent it.
        (setq result (-some->> (funcall seeker (min pos far) (max pos far))
                       (-filter #'in-window-p)
                       (-min-by #'farther-p)))))
    result))

(defun hel--after-whitespace-and-separators (pos dir after-separator)
  "Return the end of the gap of whitespace and separators from POS in DIR.
The gap is the whitespace and the separators, such as commas, in any
order. Return nil when the gap is empty or ends at a border, see
`hel--reached-border-p'. AFTER-SEPARATOR takes a position and returns
the far edge in DIR of the separator next to it, or the position itself
when no separator is there."
  (let ((end pos)
        old)
    (while (not (eql old end))
      (setq old end
            end (funcall after-separator (hel-after-whitespace end dir))))
    (unless (or (= end pos)
                (hel--reached-border-p end dir))
      end)))

;;;; Header comments

(defun hel--header-comment-begin (pos)
  "Return the start of the header comment of the thing that starts at POS.
The header comment is the comment lines directly above the thing. It
starts at the start of its first line when the thing starts at a line
start, and at the comment text otherwise, after the indentation. Return
POS when the thing has no header comment."
  (or (save-excursion
        (goto-char pos)
        (when-let* ((line-begin (line-beginning-position))
                    ((< (point-min) line-begin))
                    ;; The thing is the first text on its line.
                    ((save-excursion (hel-skip-blanks -1) (bolp)))
                    (comment (hel-bounds-of-comment-at (1- line-begin)))
                    ((= (cdr comment) line-begin)))
          (if (= pos line-begin)
              (car comment)
            (goto-char (car comment))
            (back-to-indentation)
            (point))))
      pos))

(defun hel--after-comments (pos)
  "Return the end of the comments and the whitespace from POS forward."
  (while-let ((comment (hel-bounds-of-comment-at pos))
              ((< pos (cdr comment))))
    (setq pos (hel-after-whitespace (cdr comment) 1)))
  pos)

(defun hel--thing-begin (thing instance)
  "Return the start of the INSTANCE of THING, with its header comment."
  (if (hel-thing-owns-header-comment-p thing)
      (hel--header-comment-begin (hel-thing-instance-begin instance))
    (hel-thing-instance-begin instance)))

;;; Selecting things

(defun hel--sync-selection-history ()
  "Forget `hel-selection-history' if the selection has changed.
If only the direction of the selection differs, remember the new direction."
  (-let [(range dir) (hel-selection)]
    (if-let* ((newest (car hel-selection-history))
              ((equal (-first-item newest) range)))
        (setf (-second-item newest) dir)
      (setq hel-selection-history nil))))

(defun hel--record-selection (before &optional run)
  "Remember the active region and RUN in `hel-selection-history'.
BEFORE is the selection before the command, as `hel-selection' returns it,
or point when there was none. When the history is empty, BEFORE becomes its
first entry. RUN has the form described in `hel-selection-history'."
  (unless hel-selection-history
    (push before hel-selection-history))
  (push (append (hel-selection) (list run))
        hel-selection-history))

(defun hel--restore-selection (entry)
  "Restore the selection of ENTRY of `hel-selection-history'."
  (if (numberp entry)
      (progn
        (deactivate-mark)
        (goto-char entry))
    ;; else
    (-let [(range dir) entry]
      (hel-select range dir))))

(defun hel-mark-text-object (thing kind count)
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
  (if-let* ((producer (hel-thing-producer thing)))
      (progn
        (hel--sync-selection-history)
        (-let* ((dir (hel-sign count))
                (before (or (hel-selection) (point)))
                (key (list thing kind))
                ((_ _ (run-key run-first run-last)) (car hel-selection-history))
                (extend? (equal key run-key))
                ((first . last)
                 (if extend?
                     (hel--extend-things thing producer run-first run-last count)
                   (when-let* ((instance (->> (hel-cursor-position)
                                              (hel-thing-owning thing producer))))
                     (hel--extend-things thing producer instance instance
                                         (- count dir))))))
          (when-let* ((first)
                      (range (pcase kind
                               (:inner (cons (hel-thing-instance-begin first)
                                             (hel-thing-instance-end last)))
                               (_ (hel-thing-around thing producer first last))))
                      ((not (equal range (car-safe before)))))
            (hel-push-point)
            (hel-select range (if (or extend? (/= 1 (abs count)))
                                  dir
                                (hel-thing-selection-direction thing)))
            (hel--record-selection before (list key first last))
            (hel-reveal-point-when-on-top))))
    (hel--no-thing-producer-message thing)))

(defun hel-mark-to-thing-edge (thing count edge whole?)
  "Select from point to the EDGE of the COUNT-th THING.
A negative COUNT goes backward. EDGE and WHOLE? are as in
`hel-thing-motion-range'. When extending the selection is on, extend the
selection instead of replacing it. When no thing exists in the direction,
the selection stays as it is."
  (if-let* ((producer (hel-thing-producer thing)))
      (-when-let ((begin . end) (hel-thing-motion-range
                                 thing producer (point) count edge whole?))
        (when (hel-thing-jump-p thing) (hel-push-point))
        (when (< count 0) (cl-rotatef begin end))
        (hel-motion-select begin end)
        (hel-reveal-point-when-on-top))
    ;; else
    (hel--no-thing-producer-message thing)))

;;; Producers

(defun hel-thing-producer (thing)
  "Return the producer of THING at the cursor, or nil.
The producer comes from the first source in the property `hel-thing-sources'
of THING that returns one."
  (-some #'funcall (get thing 'hel-thing-sources)))

(defun hel--no-thing-producer-message (thing)
  "Report that no source of THING applies at the cursor."
  (message "No %s here" thing))

;;;; The tree-sitter query producer

(cl-defstruct (hel-treesit-query-producer
               (:constructor hel-treesit-query-producer--create)
               (:copier nil))
  "The producer of the things that the text object queries find.
A query captures a thing under the name of the thing, such as `function'.
The parent of a thing is the parent node of its match.")

(defun hel-treesit-source ()
  "Return the producer of text objects based on tree-sitter queries, or nil."
  (if (hel-treesit-text-objects-p)
      (hel-treesit-query-producer--create)))

(defun hel-treesit-text-objects-p ()
  "Return non-nil if the tree-sitter text objects work at the cursor."
  (let ((cursor-pos (if (and (use-region-p)
                             (< (mark) (point)))
                        (1- (point))
                      (point))))
    (and (hel-treesit-active-p)
         ;; We need the `treesit-query-capture' GROUPED argument which was
         ;; added in Emacs 31.
         (<= 31 emacs-major-version)
         (hel-treesit--query (treesit-language-at cursor-pos))
         t)))

(cl-defmethod hel-thing-at (thing (_producer hel-treesit-query-producer) pos)
  "Capture the things in the whitespace around POS.
A linewise thing ends after its line end, while its node may end before
trailing blanks, so the query covers the whitespace on both sides."
  (let ((things (hel-treesit--capture-things
                 thing
                 (max (point-min) (1- (hel-after-whitespace pos -1)))
                 (min (point-max) (1+ (hel-after-whitespace pos 1))))))
    (cl-flet ((innermost (pred)
                (-some->> things
                  (-filter pred)
                  (-min-by (-on #'> (-compose #'hel-range-length
                                              #'hel-thing-instance-bounds))))))
      ;; Two things can touch at POS. The one that holds the character
      ;; under the cursor wins, whatever the size of the other one.
      (or (innermost (lambda (instance)
                       (and (<= (hel-thing-instance-begin instance) pos)
                            (< pos (hel-thing-instance-end instance)))))
          (innermost (lambda (instance)
                       (= pos (hel-thing-instance-end instance))))))))

(cl-defmethod hel-thing-candidates (thing (producer hel-treesit-query-producer) pos)
  "Return the things that hold POS, and the things next to it.
A thing is next to POS when only whitespace and separators lie between
them. The thing after the comments behind POS counts too, because these
comments may be its header comment."
  (let ((from (-> (or (hel-thing-after-gap thing producer pos -1) pos)
                  (1-)
                  (max (point-min))))
        (to   (-> (or (hel-thing-after-gap thing producer pos 1) pos)
                  (hel--after-comments)
                  (1+)
                  (min (point-max)))))
    (hel-treesit--capture-things thing from to)))

(cl-defmethod hel-thing-next (thing (_producer hel-treesit-query-producer) pos dir)
  (when (if (< dir 0)
            (< (point-min) pos)
          (< pos (point-max)))
    (hel--find-nearest (-partial #'hel-treesit--capture-things thing)
                               (if (< dir 0) pos (1+ pos))
                               dir :key #'hel-thing-instance-bounds)))

(cl-defmethod hel-thing-parent (_thing (_producer hel-treesit-query-producer)
                                instance)
  "Return the nearest node above all the nodes of INSTANCE.
This is the common ancestor of their parents: the parent of a single
node, the parent of siblings, and the parent of the outer node when the
nodes nest."
  (->> (hel-thing-instance-data instance)
       (-map #'treesit-node-parent)
       (-reduce #'hel-treesit--common-ancestor)))

(cl-defmethod hel-thing-sibling ( thing (producer hel-treesit-query-producer)
                                  instance dir)
  "Skip the things that lie deeper than INSTANCE.
Things nest, so the nearest thing may lie deeper: the method of a class comes
between two top-level functions. The search stays inside the parent node."
  (let* ((parent (hel-thing-parent thing producer instance))
         (parent-bound (when (treesit-node-p parent)
                         (if (< dir 0)
                             (treesit-node-start parent)
                           (treesit-node-end parent))))
         (thing-bound (if (< dir 0)
                          (-> (hel-thing-instance-begin instance)
                              (hel--header-comment-begin))
                        (hel-thing-instance-end instance))))
    (hel--find-nearest
     (lambda (begin end)
       (->> (hel-treesit--capture-things thing begin end)
            (-filter (lambda (instance)
                       (treesit-node-eq (hel-thing-parent thing producer instance)
                                        parent)))))
     thing-bound dir :limit parent-bound
     :key #'hel-thing-instance-bounds)))

(cl-defmethod hel-thing-after-gap ( _thing (_producer hel-treesit-query-producer)
                                    pos dir)
  "Return the end of the whitespace and the separator nodes from POS in DIR.
A separator is an anonymous node of punctuation, such as the comma between
two arguments. It lies between two things and belongs to neither."
  (cl-flet ((after-separator (pos)
              (if-let* ((char (if (< 0 dir) pos (1- pos)))
                        ((<= (point-min) char))
                        ((< char (point-max)))
                        (node (treesit-node-at char))
                        ((not (treesit-node-check node 'named)))
                        (begin (treesit-node-start node))
                        (end   (treesit-node-end node))
                        ((<= begin char))
                        ((< char end))
                        ((save-excursion
                           (goto-char begin)
                           (skip-syntax-forward "." end)
                           (= end (point)))))
                  (if (< dir 0) begin end)
                pos)))
    (hel--after-whitespace-and-separators pos dir #'after-separator)))

(defun hel-treesit--capture-things (thing begin end)
  "Return the THINGs that the query finds in BEGIN..END.
A query captures THING under its name, such as `function'. The thing is
all the nodes that one match captures for it, such as a struct and the
`;' after it, and it spans them all.

A larger thing can hold a smaller one with the syntax around it: a Python
function with its decorators holds the function without them. Its pattern
marks the nodes of the smaller thing with the capture `_shadowed', and
every other match that has one of these nodes among its own is dropped.
A pattern may mark any number of nodes. Shadowing can chain: an `export'
statement shadows a `const' declaration, which shadows the arrow function
in it. A match never drops itself, so a pattern may also mark its own
node, as the pattern of a struct and the `;' after it does.

Each instance keeps the list of its nodes as its data."
  (when-let* ((node (treesit-node-on begin end))
              (parser (treesit-node-parser node))
              (query (hel-treesit--query (treesit-parser-language parser))))
    (cl-flet ((nodes (name match)
                (-keep (-lambda ((capture . node))
                         (if (eq capture name) node))
                       match)))
      (let (;; Each `_shadowed' node maps to the instances whose match
            ;; marks it: the instances that shadow the things of the node.
            (shadowing (make-hash-table :test 'hel-treesit-node))
            instances)
        ;; The sixth argument — GROUPED — exists only since Emacs 31.
        ;; `hel-treesit-text-objects-p' keeps this call from running on an older
        ;; Emacs, but the byte compiler still rise the argument count error.
        (dolist (match (with-suppressed-warnings ((callargs treesit-query-capture))
                         (treesit-query-capture parser query begin end nil t)))
          (when-let* ((nodes (nodes thing match))
                      (bounds (->> (hel-treesit-nodes-hull nodes)
                                   (hel-thing-apply-rules thing)))
                      (instance (hel-thing-instance-create bounds :data nodes)))
            (dolist (shadowed (nodes '_shadowed match))
              (push instance (gethash shadowed shadowing)))
            (push instance instances)))
        (->> instances
             (-remove (lambda (instance)
                        (->> (hel-thing-instance-data instance)
                             (-any? (lambda (node)
                                      (->> (gethash node shadowing)
                                           (-any? (lambda (other)
                                                    (not (eq other instance))))))))))
             (nreverse))))))

(defun hel-treesit--common-ancestor (a b)
  "Return the nearest node that is A or above A, and also B or above B.
Return nil when A and B have no node in common, or when one of them is
nil: nil stands above the root. The result does not depend on the order
of A and B, and a list of nodes reduces to one common ancestor in any
order."
  (when (and a b)
    (let ((ancestors (make-hash-table :test 'hel-treesit-node)))
      (cl-loop for node = a then (treesit-node-parent node)
               while node
               do (puthash node t ancestors))
      (cl-loop for node = b then (treesit-node-parent node)
               while node
               when (gethash node ancestors)
               return node))))

;;;;; Queries

(defvar hel-treesit-queries-directory
  (->> (or load-file-name
           (bound-and-true-p byte-compile-current-file)
           buffer-file-name)
       (file-name-directory)
       (expand-file-name "queries/"))
  "The directory that holds the text object queries.
It holds a directory for each language, named after the language of its
tree-sitter parser. That directory holds the file `textobjects.scm'.")

(defconst hel-treesit--query-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (dolist (char '(?@ ?. ?_ ?- ?! ?? ?# ?+ ?* ?:))
      (modify-syntax-entry char "_" table))
    table)
  "The syntax table of tree-sitter query files.
`hel-treesit--query-patterns' splits a query file into its top-level patterns
with `forward-comment' and `forward-sexp', and this table tells them how a query
is written: `;' starts a comment that ends at the end of the line; a string is
in double quotes, with `\\' as the escape character; and the characters of
a capture, a predicate, a field or a quantifier are symbol characters, so that
`@function.inner' or `#match?' is one sexp. A parenthesis in a comment or in
a regex then does not count.

The syntax table of Scheme is close, but it has rules that a query does not:
`#|' starts a block comment, `#;' comments out the next sexp, and the quote
characters are prefixes.")

(defun hel-treesit--query-text (language)
  "Return the text of the query of LANGUAGE or nil.
The query is the file \"textobjects.scm\" of LANGUAGE, after the files that
it inherits. LANGUAGE is a symbol or, for an inherited file, a string."
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

(defvar hel-treesit--queries (make-hash-table :test #'eq)
  "The compiled text object queries, by language.
A language without a query file or without valid patterns maps to nil.")

(defun hel-treesit--query (language)
  "Return the compiled text object query of LANGUAGE, or nil.
`hel-treesit--queries' keeps the result of each language. A query file that
cannot be split into patterns gives no query, and a message names the language."
  (pcase (gethash language hel-treesit--queries 'unknown)
    ('unknown
     (let ((query (condition-case err
                      ;; Each pattern compiles on its own first, so that
                      ;; a node type or a field that the installed grammar
                      ;; lacks drops one pattern and not the whole query.
                      (when-let* ((patterns
                                   (-some->> (hel-treesit--query-text language)
                                     (hel-treesit--query-patterns)
                                     (-filter (lambda (pattern)
                                                (ignore-errors
                                                  (treesit-query-compile
                                                   language pattern t)))))))
                        (treesit-query-compile
                         language (string-join patterns "\n") t))
                    (error
                     (message "Hel: no text objects for %s: %s"
                              language (error-message-string err))
                     nil))))
       (puthash language query hel-treesit--queries)))
    (query query)))

;;;; The motion producer

(cl-defstruct (hel-motion-producer (:constructor hel-motion-producer--create)
                                   (:copier nil))
  "The producer of the things that a motion moves over.
A thing is the text between two stops of the motion, without the whitespace
at its edges."
  (function nil :documentation "\
A function of COUNT that moves point over COUNT things, backward when
COUNT is negative, and returns the part of COUNT that it could not move,
as `hel-word-motion' does."))

(cl-defstruct (hel-word-motion-producer
               (:include hel-motion-producer)
               (:constructor hel-word-motion-producer--create)
               (:copier nil))
  "The producer of the word things.
A motion to the edge of a word does not cross a line end: the line end and
the indentation of a line are steps of their own.")

(defun hel-motion-source (motion)
  "Return a source of the things that MOTION moves over.
A source is a function that returns a producer."
  (let ((producer (hel-motion-producer--create :function motion)))
    (lambda () producer)))

(defun hel-word-motion-source (motion)
  "Return a source of the words that MOTION moves over.
A source is a function that returns a producer."
  (let ((producer (hel-word-motion-producer--create :function motion)))
    (lambda () producer)))

(cl-defmethod hel-thing-at (thing (producer hel-motion-producer) pos)
  "To find the start of the thing, the motion of PRODUCER goes to its end
and back. The first backward move alone can stop elsewhere: from
a header comment, `beginning-of-defun' goes back to the function
before it."
  (when-let* ((motion (hel-motion-producer-function producer))
              (bounds (save-excursion
                        (goto-char (min (1+ pos) (point-max)))
                        (and (zerop (funcall motion -1))
                             (zerop (funcall motion 1))
                             (let ((end (point)))
                               (funcall motion -1)
                               (-some->> (hel-range (point) end)
                                 (hel-trim-whitespace-from-range)
                                 (hel-thing-apply-rules thing))))))
              ((<= (car bounds) pos (cdr bounds))))
    (hel-thing-instance-create bounds)))

(cl-defmethod hel-thing-next (thing (producer hel-motion-producer) pos dir)
  (let ((motion (hel-motion-producer-function producer)))
    (save-excursion
      (goto-char pos)
      (let (result)
        (while-let (((not result))
                    (old (point))
                    ((zerop (funcall motion dir)))
                    ((/= old (point))))
          (when-let* ((stop (point))
                      (other (save-excursion
                               (funcall motion (- dir))
                               (point)))
                      (bounds (-some->> (hel-range stop other)
                                (hel-trim-whitespace-from-range)
                                (hel-thing-apply-rules thing)))
                      ((if (< 0 dir)
                           (< pos (car bounds))
                         (<= (cdr bounds) pos))))
            (setq result bounds)))
        (when result
          (hel-thing-instance-create result))))))

;;; Things
;;;; Words

(put 'word 'hel-thing-jump nil)
(put 'word 'hel-thing-line-end-is-border t)
(put 'word 'hel-thing-sources (list (hel-word-motion-source #'hel-word-motion)))

(put 'WORD 'hel-thing-jump nil)
(put 'WORD 'hel-thing-line-end-is-border t)
(put 'WORD 'hel-thing-trim-punctuation ",.;:!?()[]{}<>\"'`")
(put 'WORD 'hel-thing-sources (list (hel-word-motion-source #'hel-WORD-motion)))

(cl-defun hel-word-motion (&optional (count 1))
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

(cl-defun hel-WORD-motion (&optional (count 1))
  "Move point COUNT WORDs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.

A WORD is a run of characters between whitespace."
  (hel-motion-loop (dir count)
    (hel-skip-chars "\r\n" dir)
    (hel-skip-blanks dir)
    (unless (hel-line-boundary-p dir)
      (hel-skip-chars (concat "^" hel-whitespace-chars) dir))))

(cl-defmethod hel-thing-motion-range (_thing (producer hel-word-motion-producer)
                                      pos count edge _whole)
  "Step over the line end and the indentation of a line one at a time."
  (save-excursion
    (goto-char pos)
    (let ((word-motion (hel-motion-producer-function producer))
          start end)
      (cond ((and (< count 0) (eq edge :end))
             (setq start (point)
                   end   (progn
                           (hel-motion-loop (dir count)
                             (cond ((and (bolp) (not (bobp)))
                                    (skip-chars-backward "\r\n"))
                                   ((hel-blank-p (preceding-char))
                                    (hel-skip-blanks dir))
                                   (t
                                    (funcall word-motion dir)
                                    (hel-skip-blanks dir))))
                           (point))))
            ((and (< count 0) (eq edge :begin))
             (setq start (progn
                           (skip-chars-backward "\r\n")
                           (point))
                   end   (progn
                           (funcall word-motion count)
                           (point))))
            ((and (< 0 count) (eq edge :begin))
             (setq start (progn
                           (skip-chars-forward "\r\n")
                           (point))
                   end   (progn
                           ;; The blanks before the next word are the first step.
                           (when (hel-blank-p (following-char))
                             (cl-decf count))
                           (funcall word-motion count)
                           (hel-skip-blanks 1)
                           (point))))
            (t ;; (and (< 0 count) (eq edge :end))
             (setq start (progn
                           (skip-chars-forward "\r\n")
                           (point))
                   end   (progn
                           (funcall word-motion count)
                           (point)))))
      (hel-range start end))))

;;;; Sentences

(put 'sentence 'hel-thing-line-end-is-border t)
(put 'sentence 'hel-thing-sources (list (hel-motion-source
                                         #'hel-sentence-motion)))

(cl-defun hel-sentence-motion (&optional (count 1))
  "Move point COUNT sentences forward (backward if COUNT is negative).
Return the part of COUNT that is left to move, with the sign of COUNT.

The function `forward-sentence-function' defines what a sentence is."
  (hel-motion-loop (dir count)
    (ignore-errors (forward-sentence dir))))

(cl-defmethod hel-thing-parent ((_thing (eql 'sentence)) _producer sentence)
  "Return the start of the paragraph that SENTENCE belongs to."
  (save-excursion
    (goto-char (min (1+ (hel-thing-instance-begin sentence))
                    (point-max)))
    (hel-paragraph-motion -1)
    (point)))

;;;; Paragraphs

(put 'paragraph 'hel-thing-linewise t)
(put 'paragraph 'hel-thing-selection-direction -1)
(put 'paragraph 'hel-thing-sources (list (hel-motion-source
                                          #'hel-paragraph-motion)))

(cl-defun hel-paragraph-motion (&optional (count 1))
  "Move point COUNT paragraphs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT."
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (hel-motion-loop (dir count)
      (cond ((< 0 dir)
             (forward-paragraph))
            ((not (bobp))
             (unless (bolp) (end-of-line))
             (start-of-paragraph-text)
             (beginning-of-line))))))

;;;; Comments

(put 'comment 'hel-thing-linewise t)
(put 'comment 'hel-thing-selection-direction -1)
(put 'comment 'hel-thing-sources (list #'hel-treesit-comment-source
                                       #'hel-syntax-comment-source))

(cl-defstruct (hel-comment-producer (:constructor nil)
                                    (:copier nil))
  "The producer of comments.
A parser or the syntax table finds only the pieces of a comment, such as
each line of a `//' comment. The two kinds of this producer, one for each,
find the pieces, and the code they share joins the pieces into comments.")

(cl-defstruct (hel-syntax-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-syntax-comment-source)
               (:copier nil))
  "The producer of the comments that the syntax table finds.")

(cl-defstruct (hel-treesit-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-treesit-comment-producer--create)
               (:copier nil))
  "The producer of the comments that a tree-sitter query finds.")

(defun hel-treesit-comment-source ()
  "Return the source of the comments found with the tree-sitter queries."
  (when (hel-treesit-text-objects-p)
    (hel-treesit-comment-producer--create)))

(cl-defgeneric hel--comment-piece-under (producer pos)
  "Return the bounds of the comment piece that holds the character at POS.
A comment consists of pieces. A piece is what the parser or the syntax
table sees as one comment: one line of a `//' comment, or a whole `/* */'
comment. The bounds leave out the whitespace at the edges of the piece.
Return nil when POS is in no piece. PRODUCER finds the pieces.")

(cl-defgeneric hel--comment-piece-next (producer pos dir)
  "Return the bounds of the next comment piece from POS in DIR, or nil.
Forward, the piece starts at or after POS. Backward, it ends at or before POS.
PRODUCER finds the pieces.")

(defun hel-bounds-of-comment-at (pos)
  "Return the bounds of the comment at POS, or nil.
POS can be inside the comment or on its edge, and also on the blanks
around it or on the line end after it. The bounds are what `mic' selects:
the whole lines of the comment with the line end of the last one, or only
the comment when it follows code."
  (when-let* ((producer (hel-thing-producer 'comment))
              (range (hel--comment-run-at producer pos)))
    (hel-thing-apply-rules 'comment range)))

(defun hel--comment-run-at (producer pos)
  "Return the bounds of the run of comment pieces at POS, or nil.
A comment is a run of pieces, each of which fills its line, with no blank
line between them. The bounds run from the start of the first piece to the
end of the last one. A piece after code is a run of its own. POS can also
be on the blanks around the run or on the line end after it. PRODUCER
finds the pieces."
  (when-let*
      ((piece (or (hel--comment-piece-under producer pos)
                  (let ((text-begin (hel-after-blanks pos 1))
                        (text-end   (hel-after-blanks pos -1)))
                    (save-excursion
                      (cond ((progn (goto-char text-end) (bolp))
                             (hel--comment-piece-under producer text-begin))
                            ((progn (goto-char text-begin) (eolp))
                             (hel--comment-piece-under producer (1- text-end)))))))))
    (if (hel-only-text-on-lines-p piece)
        (cons (hel--after-comment-lines producer (car piece) -1)
              (hel--after-comment-lines producer (cdr piece) 1))
      piece)))

(cl-defmethod hel-thing-at ((_thing (eql 'comment))
                            (producer hel-comment-producer) pos)
  "Look for the comment at POS, then for the comment before POS.
The second one can end at POS."
  (->> (list pos
             (max (1- pos) (point-min)))
       (-some (lambda (from)
                (when-let* ((bounds (-some->> (hel--comment-run-at producer from)
                                              (hel-thing-apply-rules 'comment)))
                            ((<= (car bounds) pos (cdr bounds))))
                  (hel-thing-instance-create bounds))))))

(cl-defmethod hel-thing-sibling ((_thing (eql 'comment)) _producer _instance _dir)
  "Return nil: a comment has no siblings."
  nil)

(cl-defmethod hel-thing-next ((_thing (eql 'comment))
                              (producer hel-comment-producer) pos dir)
  (let (result)
    (while-let (((not result))
                (comment (-some->> (hel--comment-piece-next producer pos dir)
                                   (car)
                                   (hel--comment-run-at producer)
                                   (hel-thing-apply-rules 'comment)))
                (far-edge (if (< dir 0) (car comment) (cdr comment)))
                ((/= pos far-edge)))
      (if (if (< 0 dir)
              (< pos (car comment))
            (<= (cdr comment) pos))
          (setq result comment)
        (setq pos far-edge)))
    (when result
      (hel-thing-instance-create result))))

(cl-defmethod hel--comment-piece-under ((_producer hel-treesit-comment-producer)
                                        pos)
  ;; The smallest comment node that holds the character at POS.
  (-some->> (hel-treesit--comment-nodes pos (min (1+ pos) (point-max)))
    (-filter (lambda (node)
               (and (<= (treesit-node-start node) pos)
                    (< pos (treesit-node-end node)))))
    (-min-by (-on #'> #'hel-treesit-node-length))
    (hel-treesit-node-bounds)
    (hel-trim-whitespace-from-range)))

(cl-defmethod hel--comment-piece-next ((_producer hel-treesit-comment-producer)
                                       pos dir)
  (when-let* ((node (hel--find-nearest
                     #'hel-treesit--comment-nodes
                     pos dir :key #'hel-treesit-node-bounds)))
    (-> node
        (hel-treesit-node-bounds)
        (hel-trim-whitespace-from-range))))

(defun hel-treesit--comment-nodes (begin end)
  "Return the comment nodes that the text object query finds in BEGIN..END."
  (when-let* ((node (treesit-node-on begin end))
              (parser (treesit-node-parser node))
              (query (hel-treesit--query (treesit-parser-language parser))))
    (->> (treesit-query-capture parser query begin end)
         (-keep (-lambda ((capture . node))
                  (if (eq capture 'comment) node))))))

(cl-defmethod hel--comment-piece-under ((_producer hel-syntax-comment-producer)
                                        pos)
  "Find the piece with `forward-comment', which needs the comment start.
Try three starts: the comment start that `syntax-ppss' reports when POS
is inside a comment; POS itself, when POS is on the first character of the
comment delimiter, such as the first `/' of `//'; and the position before
POS, when POS is on its second character. A comment that has no end of
its own ends at the end of the buffer."
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

(cl-defmethod hel--comment-piece-next ((producer hel-syntax-comment-producer)
                                       pos dir)
  (comment-normalize-vars t)
  (save-excursion
    (goto-char pos)
    (if (< 0 dir)
        (when-let* ((start (comment-search-forward (point-max) t)))
          (hel--comment-piece-under producer start))
      ;; else
      ;; A piece that holds POS does not end before it: search on from
      ;; its start.
      (cl-loop for start = (comment-search-backward (point-min) t)
               while start
               for piece = (hel--comment-piece-under producer start)
               when (and piece (<= (cdr piece) pos))
               return piece
               do (goto-char start)))))

(cl-defmethod hel-thing-after-gap ((_thing (eql 'comment)) _producer pos dir)
  "Next to a comment of whole lines, the gap is the blank lines.
Next to a comment that follows code, the gap is the blanks on that line,
so the line of code never joins the next line. An opening bracket before
such a comment is no border: the gap is the blanks between the code and
the comment, as in `{  // comment', and `mac' takes them."
  (let ((end (if (save-excursion (goto-char pos) (bolp))
                 (hel-after-whitespace pos dir)
               (hel-after-blanks pos dir))))
    (unless (or (= end pos)
                (and (hel--reached-border-p end dir)
                     (or (< 0 dir)
                         (save-excursion (goto-char end) (eolp)))))
      end)))

(defun hel--after-comment-lines (producer pos dir)
  "Return the far edge of the comment lines next to the line of POS in DIR.
A comment line is a line that one comment piece fills, apart from the blanks.
Walk from the line of POS over the next lines in direction DIR while they are
comment lines, and return the start of the last piece backward, or its end
forward. Return POS when the next line is not a comment line. PRODUCER finds
the pieces."
  (save-excursion
    (let ((edge pos))
      (while-let (((progn (goto-char edge)
                          (beginning-of-line)
                          (not (if (< dir 0) (bobp) (eobp)))))
                  ((zerop (forward-line dir)))
                  ;; From a last line without a newline, `forward-line'
                  ;; reports success and stays on that line.
                  ((bolp))
                  ((progn (back-to-indentation)
                          (not (eolp))))
                  (text (if (< 0 dir)
                            (point)
                          (end-of-line)
                          (hel-skip-blanks -1)
                          (1- (point))))
                  (piece (hel--comment-piece-under producer text))
                  ((hel-only-text-on-lines-p piece)))
        (setq edge (if (< dir 0) (car piece) (cdr piece))))
      edge)))

;;;; Functions

(put 'function 'hel-thing-linewise t)
(put 'function 'hel-thing-owns-header-comment t)
(put 'function 'hel-thing-selection-direction -1)
(put 'function 'hel-thing-sources (list #'hel-treesit-source
                                        (hel-motion-source #'hel-function-motion)))

(cl-defun hel-function-motion (&optional (count 1))
  "Move point COUNT functions forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT."
  (hel-motion-loop (dir count)
    (if (< dir 0)
        (beginning-of-defun)
      (end-of-defun))))

(cl-defmethod hel-thing-parent ((_thing (eql 'function))
                                (_producer hel-motion-producer) instance)
  "Return the start of the bracket pair around INSTANCE.
A function that a motion finds has no parent node. So `2 maf' selects only
functions inside the same bracket pair, as it does for sexps."
  (->> (save-excursion (syntax-ppss (hel-thing-instance-begin instance)))
       (nth 1)))

;;;; Classes

(put 'class 'hel-thing-linewise t)
(put 'class 'hel-thing-owns-header-comment t)
(put 'class 'hel-thing-selection-direction -1)
(put 'class 'hel-thing-sources (list #'hel-treesit-source))

;;;; Arguments

(put 'argument 'hel-thing-owns-header-comment t)
(put 'argument 'hel-thing-sources (list #'hel-treesit-source
                                        #'hel-syntax-argument-source))

(cl-defstruct (hel-syntax-argument-producer
               (:constructor hel-syntax-argument-producer--create)
               (:copier nil))
  "The producer of the arguments that the syntax table finds.
An argument is a piece of the text between parentheses. A comma at the
level of the parentheses separates two arguments, and a comma in nested
brackets, in a string or in a comment does not. The parent of an
argument is the start of its parentheses.")

(defun hel-syntax-argument-source ()
  "Return the producer of the arguments that the syntax table finds, or nil.
In Lisp, whitespace and not a comma separates the elements of a list, so
in a buffer derived from `lisp-data-mode' this source returns nil."
  (unless (derived-mode-p 'lisp-data-mode)
    (hel-syntax-argument-producer--create)))

(cl-defmethod hel-thing-at (thing (_producer hel-syntax-argument-producer) pos)
  "Look in each pair of parentheses around POS, the innermost first.
The arguments of inner parentheses lie inside an argument of the outer
ones, so the first argument found is the innermost."
  (let ((runs (-map (-partial #'hel--paren-arguments thing)
                    (hel--parens-around pos))))
    (cl-flet ((find (pred)
                (-some (lambda (run) (-first pred run)) runs)))
      (or (find (lambda (instance)
                  (and (<= (hel-thing-instance-begin instance) pos)
                       (< pos (hel-thing-instance-end instance)))))
          (find (lambda (instance)
                  (= pos (hel-thing-instance-end instance))))))))

(cl-defmethod hel-thing-next (thing (_producer hel-syntax-argument-producer)
                                    pos dir)
  (when (if (< dir 0)
            (< (point-min) pos)
          (< pos (point-max)))
    (hel--find-nearest (-partial #'hel--syntax-arguments-in thing)
                       (if (< dir 0) pos (1+ pos))
                       dir :key #'hel-thing-instance-bounds)))

(cl-defmethod hel-thing-parent (_thing (_producer hel-syntax-argument-producer)
                                instance)
  "Return the start of the parentheses around INSTANCE."
  (hel-thing-instance-data instance))

(cl-defmethod hel-thing-after-gap (_thing (_producer hel-syntax-argument-producer)
                                   pos dir)
  "Return the end of the whitespace and the commas from POS in DIR."
  (hel--after-whitespace-and-separators
   pos dir (lambda (pos)
             (if (eq ?, (if (< 0 dir) (char-after pos) (char-before pos)))
                 (+ pos dir)
               pos))))

(defun hel--syntax-arguments-in (thing begin end)
  "Return the THINGs in every pair of parentheses that overlaps BEGIN..END.
These are the parentheses around BEGIN and the ones that open between
BEGIN and END. Every THING whose start or end lies in BEGIN..END is
among the result."
  (->> (append (hel--parens-around begin)
               (hel--parens-in begin end))
       (-mapcat (-partial #'hel--paren-arguments thing))))

(defun hel--parens-around (pos)
  "Return the starts of the parentheses around the character at POS.
The innermost come first. Brackets of other shapes do not count."
  (->> (save-excursion (syntax-ppss pos))
       (nth 9)
       (reverse)
       (-filter (lambda (open) (eq ?\( (char-after open))))))

(defun hel--parens-in (begin end)
  "Return the starts of the parentheses that open between BEGIN and END.
A parenthesis in a string or in a comment does not count."
  (save-excursion
    (goto-char begin)
    (let (opens)
      (while (search-forward "(" end t)
        (let ((open (1- (point))))
          (when (and (= 4 (syntax-class (syntax-after open)))
                     (not (nth 8 (save-excursion (syntax-ppss open)))))
            (push open opens))))
      (nreverse opens))))

(defun hel--paren-arguments (thing open)
  "Return the THINGs between the parentheses that open at OPEN.
A THING is the text between two commas at the level of the parentheses,
or between a comma and a parenthesis, without the whitespace and the
comments at its edges. An empty piece, such as the one after a trailing
comma, is no THING. Each instance keeps OPEN as its data. The time is
linear in the size of the parentheses."
  (save-excursion
    (when-let* ((close (ignore-errors (1- (scan-lists open 1 0)))))
      (let ((begin (1+ open))
            instances)
        (cl-flet ((add (begin end)
                    (save-excursion
                      (let ((text-begin (progn (goto-char begin)
                                               (forward-comment (buffer-size))
                                               (point)))
                            (text-end (progn (goto-char end)
                                             (forward-comment (- (buffer-size)))
                                             (point))))
                        (when (< text-begin text-end)
                          (push (hel-thing-instance-create
                                 (hel-thing-apply-rules
                                  thing (cons text-begin text-end))
                                 :data open)
                                instances))))))
          (goto-char begin)
          (while (search-forward "," close t)
            (let* ((comma (1- (point)))
                   (ppss (save-excursion (syntax-ppss comma))))
              (when (and (eql open (nth 1 ppss))
                         (not (nth 8 ppss)))
                (add begin comma)
                (setq begin (point)))))
          (add begin close))
        (nreverse instances)))))

;;;; Entries
;;
;; An entry is one element of a data structure: an element of a list or
;; an array, a key and its value in a dictionary, a field of a struct.

(put 'entry 'hel-thing-owns-header-comment t)
(put 'entry 'hel-thing-sources (list #'hel-treesit-source))

;;; .
(provide 'hel-text-objects)
;;; hel-text-objects.el ends here
