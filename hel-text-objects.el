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
;; a list of /sources/. A source is a function of a position that returns
;; a /producer/ for a search from that position, or nil when it does not
;; apply there. The first source that returns a producer wins. Each command
;; passes the position that it searches from, so the source is chosen where
;; the search runs.
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
(require 'hel-cursors)
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
               (:constructor hel-thing-instance-create
                             (range &key data &aux (begin (car range))
                                                   (end   (cdr range))))
               (:copier nil))
  "A thing that a producer found: its bounds and what the producer keeps
about it. `hel-thing-instance-create' takes the (BEGIN . END) bounds RANGE
of the thing and the key `:data'. Two instances with equal bounds are the
same thing: DATA is not a part of the value."
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
  "Return the position of the `hel-thing-instance' at EDGE.
EDGE is `:begin' or `:end'."
  (cl-ecase edge
    (:begin (hel-thing-instance-begin self))
    (:end   (hel-thing-instance-end self))))

;;;; The rules of a thing

(defun hel-thing-owns-header-comment-p (thing)
  "Return non-nil if the comment lines above a THING belong to it."
  (get thing 'hel-thing-owns-header-comment))

(defun hel-thing-jump-p (thing)
  "Return non-nil if a motion over THING is a jump.
A jump first pushes point to the mark ring, so the user can return to the
place where the motion started. A motion over a THING with the property
`hel-thing-no-jump' is not a jump."
  (not (get thing 'hel-thing-no-jump)))

(defun hel-thing-selection-direction (thing)
  "Return the direction, 1 or -1, of a selection that holds one THING.
The property `hel-thing-selection-direction' of THING sets it. Without
the property the direction is 1, forward."
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

(cl-defgeneric hel-thing-apply-rules (thing bounds)
  "Return the bounds of a THING as the user sees them.
A producer finds where a THING is in the text, and the rules of THING
then tune its edges. BOUNDS is a (BEGIN . END) pair.

A method tunes the edges by one rule for its kind of THING, then calls
the next method so that the other rules apply too."
  (hel-thing-maybe-expand-bounds-to-full-lines thing bounds))

(cl-defgeneric hel-thing-parent (thing producer instance)
  "Return the parent of INSTANCE, a THING that PRODUCER found.
Two things with `equal' parents are siblings, also when both parents are nil.
A text object with a count, such as `2 maf', selects only siblings: it stops at
the last sibling and never takes a thing with another parent. The parent can be
any value, such as a tree-sitter node or the start of the parentheses around
an argument.")

(cl-defmethod hel-thing-parent (_thing _producer _instance)
  "By default, the parent is nil, so all the THINGs in the buffer are siblings."
  nil)

(cl-defgeneric hel-thing-after-gap (thing producer pos dir)
  "Return the far edge of the gap after THING in direction DIR.
The gap separates THING from the next thing of any kind: the comma and the
blanks after a function argument, or the blank lines between a comment and
a function. When no gap follows THING, return POS. Called at its own result, the
function returns that result again. POS is the edge of THING on the side DIR.
PRODUCER finds the things.

By default, the gap is the whitespace.")

(cl-defmethod hel-thing-after-gap (_thing _producer pos dir)
  (hel-after-whitespace pos dir))

(cl-defgeneric hel-thing-gap-end-allowed-p (thing producer pos dir)
  "Return non-nil if a gap after THING may end at POS in direction DIR.
`ma' takes the gap after the THINGs only when its end is allowed, and
otherwise the gap before them, see `hel-thing-around'. A gap may not end
at a closing bracket, so deleting the last argument of a call leaves no
comma before the bracket. POS is the far edge of a gap, see
`hel-thing-after-gap'. PRODUCER finds the things.

By default, a gap may end anywhere but at an edge of the buffer and at
a bracket: forward, before a closing bracket, and backward, after an
opening bracket.")

(cl-defmethod hel-thing-gap-end-allowed-p (_thing _producer pos dir)
  (save-excursion
    (goto-char pos)
    (not (if (< dir 0)
             (or (bobp)
                 (eq ?\( (char-syntax (char-before))))
           (or (eobp)
               (eq ?\) (char-syntax (char-after))))))))

;;;; The finding operations

(cl-defgeneric hel-thing-at (thing producer pos)
  "Return the THING that holds the character at POS, or nil.
Of nested THINGs, return the innermost one. PRODUCER finds the things.")

(cl-defgeneric hel-thing-next (thing producer pos dir)
  "Return the nearest THING from POS in direction DIR, or nil.
Forward, the thing starts after the character at POS. Backward, it ends
at or before POS. Of two things that start, or end, at the same place,
return the larger one. PRODUCER finds the things.")

(cl-defgeneric hel-thing-candidates (thing producer pos)
  "Return the THINGs that a text object command can select at POS.
They include every THING whose body holds POS, and the nearest THINGs
whose gaps hold POS, see `hel-thing-nearest'. PRODUCER finds the things.

By default, the candidates are the THING at POS, see `hel-thing-at',
and the nearest THING on each side of POS, see `hel-thing-next'."
  (-non-nil (list (hel-thing-at thing producer pos)
                  (hel-thing-next thing producer pos -1)
                  (hel-thing-next thing producer pos 1))))

;;;; Algorithms

(cl-defgeneric hel-thing-sibling (thing producer instance dir)
  "Return the sibling of INSTANCE of THING in direction DIR, or nil.
The sibling is the nearest THING in DIR that has the same parent as
INSTANCE, defined by `hel-thing-parent'."
  (when-let* ((next (hel--thing-past thing producer instance dir))
              ((equal (hel-thing-parent thing producer instance)
                      (hel-thing-parent thing producer next))))
    next))

(cl-defgeneric hel-thing-nearest (thing producer pos)
  "Return the THING at POS, or the nearest THING across a gap, or nil.
A THING takes its body, its header comment and the gap before it, so on
a gap or a header comment the THING after it wins. The last THING of its
parent takes the gap after it too, because no THING follows to take that
gap. Of nested THINGs, the innermost one wins.

The candidates come from `hel-thing-candidates', and the gaps from
`hel-thing-after-gap'. PRODUCER finds the things."
  (cl-flet* ((body-holds-p (instance)
               (and (<= (hel-thing-instance-begin instance) pos)
                    (< pos (hel-thing-instance-end instance))))
             (gap-before-holds-p (instance)
               (let* ((begin (hel-thing-instance-begin instance))
                      (header-begin (if (hel-thing-owns-header-comment-p thing)
                                        (hel--header-comment-begin begin)
                                      begin)))
                 (and (<= (hel-thing-after-gap thing producer header-begin -1)
                          pos)
                      (< pos begin))))
             (gap-after-holds-p (instance)
               (let ((end (hel-thing-instance-end instance)))
                 (<= end pos (hel-thing-after-gap thing producer end 1))))
             (holds-p (instance)
               (or (body-holds-p instance)
                   (gap-before-holds-p instance)
                   (gap-after-holds-p instance))))
    (let* ((candidates (->> (hel-thing-candidates thing producer pos)
                            ;; Keep one thing of each range.
                            (-group-by #'hel-thing-instance-bounds)
                            (-map #'-second-item)
                            (-filter #'holds-p)))
           (innermost
            (->> candidates
                 (-remove (lambda (instance)
                            (->> candidates
                                 (-any? (lambda (other)
                                          (hel-range-strictly-inside-p
                                           (hel-thing-instance-bounds other)
                                           (hel-thing-instance-bounds instance))))))))))
      (or (-first #'body-holds-p innermost)
          (-first #'gap-before-holds-p innermost)
          (-first #'gap-after-holds-p innermost)))))

(cl-defgeneric hel-thing-around (thing producer first last)
  "Return the range that `ma' selects of the run of THINGs from FIRST to LAST.
The range hold the header comment of FIRST, the THINGs, and the gap after LAST.
When no gap follows LAST, the gap before FIRST included into range instead.
So deleting them leaves no separator and no blank line before a closing bracket.

`hel-thing-owns-header-comment-p' answers if THING can have header comment,
`hel-thing-after-gap' finds a gap, and `hel-thing-gap-end-allowed-p' decides
where a gap may end. PRODUCER finds the things."
  (let ((begin (if (hel-thing-owns-header-comment-p thing)
                   (-> (hel-thing-instance-begin first)
                       (hel--header-comment-begin))
                 (hel-thing-instance-begin first)))
        (end (hel-thing-instance-end last)))
    (cl-flet ((after-gap (pos dir)
                (let ((edge (hel-thing-after-gap thing producer pos dir)))
                  (when (and (/= edge pos)
                             (hel-thing-gap-end-allowed-p thing producer edge dir))
                    ;; From a line start, the gap ends at a line start too,
                    ;; unless it ends at the edge of the buffer.
                    (save-excursion
                      (goto-char pos)
                      (when (and (bolp)
                                 (/= edge (if (< dir 0) (point-min) (point-max))))
                        (goto-char edge)
                        (setq edge (line-beginning-position (if (< dir 0) 2 1)))))
                    (unless (= edge pos)
                      edge)))))
      (if-let* ((gap-end (after-gap end 1)))
          (cons begin gap-end)
        (cons (or (after-gap begin -1) begin)
              end)))))

(cl-defgeneric hel-thing-motion-range (thing producer pos count edge skip-gap?)
  "Return the range of a motion from POS over COUNT THINGs.
The motion runs from POS to the EDGE of the COUNT-th THING, and a negative
COUNT moves backward.

When SKIP-GAP? is non-nil and POS lies between things, the motion starts at
the first THING and leaves out the gap before it.

When fewer than COUNT things exist, the motion ends at the last of them.
A motion that keeps the gaps and goes to the starts of things goes on to
the edge of the buffer instead, because the gap after the last thing runs
up to there.

Return nil when no THING lies in that direction. The motion to the edge
of the buffer returns nil only when POS is already there. EDGE is
`:begin' or `:end'. PRODUCER finds the things."
  (let* ((dir (hel-sign count))
         (count (abs count))
         (ahead-p (lambda (instance)
                    (let ((target (hel-thing-instance-edge instance edge)))
                      (if (< dir 0)
                          (< target pos)
                        (< pos target)))))
         (here (when-let* ((char (if (< dir 0) (1- pos) pos))
                           ((<= (point-min) char))
                           ((< char (point-max)))
                           (instance (hel-thing-at thing producer char))
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
                (not skip-gap?)
                (< found count))
           (unless (= pos buffer-edge)
             (hel-range pos buffer-edge)))
          (first
           (hel-range (if (and skip-gap? (not here))
                          (hel-thing-instance-edge first (if (< dir 0) :end :begin))
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
  (or last (setq last first))
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
        (setq result (-some->> (funcall seeker (min pos far) (max pos far))
                       (-filter #'in-window-p)
                       (-min-by #'farther-p)))))
    result))

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

;;; Selecting things

(defun hel--sync-selection-history ()
  "Forget `hel-selection-history' if the selection has changed.
If only the direction of the selection differs, remember the new direction."
  (let ((selection (hel-selection)))
    (if-let* ((newest (car hel-selection-history))
              ((equal (hel-selection-bounds (car newest))
                      (hel-selection-bounds selection))))
        (setcar newest selection)
      (setq hel-selection-history nil))))

(defun hel--selection-entry (&optional run)
  "Return the selection as an entry of `hel-selection-history'.
The entry holds the selection and RUN."
  (list (hel-selection) run))

(defun hel--record-selection (before &optional run)
  "Remember the selection and RUN in `hel-selection-history'.
BEFORE is the entry of the selection before the command, as
`hel--selection-entry' returns it. When the history is empty, BEFORE
becomes its first entry. RUN has the form described in
`hel-selection-history'."
  (unless hel-selection-history
    (push before hel-selection-history))
  (push (hel--selection-entry run)
        hel-selection-history))

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
  (let* ((head (-some-> (hel-trimmed-selection) (hel-selection-head)))
         (producer (hel-thing-producer thing head)))
    (hel--sync-selection-history)
    (-let* ((dir (hel-sign count))
            (before (hel--selection-entry))
            (key (list thing kind))
            ((_ (run-key run-first run-last)) (car hel-selection-history))
            (extend? (equal key run-key))
            ((first . last)
             (if extend?
                 (hel--extend-things thing producer run-first run-last count)
               (when-let* ((instance (hel-thing-nearest thing producer head)))
                 (hel--extend-things thing producer instance nil (- count dir))))))
      (when-let* ((first)
                  (range (pcase kind
                           (:inner
                            (cons (hel-thing-instance-begin first)
                                  (hel-thing-instance-end last)))
                           (:around
                            (hel-thing-around thing producer first last))))
                  ;; Skip an object that is selected already. At a bare
                  ;; cursor, a one-character object is still selected, so
                  ;; the history records it and a repeated key goes on
                  ;; from it.
                  ((or (hel-bare-cursor-p (car before))
                       (not (equal range
                                   (hel-selection-bounds (car before)))))))
        (hel-push-point)
        (hel-select range (if (or extend? (/= 1 (abs count)))
                              dir
                            (hel-thing-selection-direction thing)))
        (hel--record-selection before (list key first last))
        (hel-reveal-point-when-on-top)))))

(defun hel-mark-to-thing-edge (thing count edge skip-gap? extend)
  "Select from the cursor to the EDGE of the COUNT-th THING.
A negative COUNT goes backward. EDGE and SKIP-GAP? are as in
`hel-thing-motion-range'.

The motion starts at the head character of the selection, the
character under the cursor, which counts as passed, as in Helix: the
selection takes it in.  When the head character alone is a step of the
motion, the motion goes on from its other edge and leaves it out, so
`w' on the space before a word selects that word.

With EXTEND, extend the selection instead of replacing it: its anchor
stays, and the motion starts from its other end. A selection of one
character has its anchor behind the motion, so it keeps its character
and the motion starts past it. Go on to the next edge when the motion
would stop on the anchor of the selection, where the selection would be
empty. When no thing exists in the direction, the selection stays as
it is."
  (let* ((selection (hel-selection))
         (producer (hel-thing-producer thing (hel-selection-head selection))))
    (cl-flet ((reach (pos count)
                ;; The range in the order of the motion: (START . END).
                (-when-let ((begin . end)
                            (hel-thing-motion-range thing producer pos count
                                                    edge skip-gap?))
                  (if (< 0 count) (cons begin end) (cons end begin)))))
      (let ((anchor (and extend
                         (hel-selection-anchor selection (hel-sign count)))))
        (-when-let* (((start . end)
                      (if extend
                          (reach (-let [((begin . end)) selection]
                                   (if (= anchor begin) end begin))
                                 count)
                        ;; NEAR and FAR are the edges of the head character
                        ;; behind and ahead of the motion.
                        (-let* (((begin . end) (-> (hel-selection-head selection)
                                                   (hel-char-range)))
                                (near (if (< 0 count) begin end))
                                (far  (if (< 0 count) end begin))
                                (step (reach near (hel-sign count))))
                          (cond ((= (point) (if (< count 0)
                                                (point-min)
                                              (point-max)))
                                 ;; Nothing lies ahead of a cursor at the edge
                                 ;; of the buffer.
                                 nil)
                                ((and step (/= near far) (eql (cdr step) far))
                                 ;; At the edge of the buffer the motion from
                                 ;; FAR gives an empty range. Then a bare
                                 ;; cursor selects its character, and
                                 ;; a selection stays.
                                 (-let [(start . end) (reach far count)]
                                   (cond ((and start (/= start end))
                                          (cons start end))
                                         ((hel-bare-cursor-p selection) step))))
                                ((= 1 (abs count)) step)
                                (t (reach near count))))))
                     ((start . end) (if (eql end anchor)
                                        (reach end (hel-sign count))
                                      (cons start end))))
          (when (hel-thing-jump-p thing) (hel-push-point))
          (hel-move-to-pos start end extend)
          (hel-reveal-point-when-on-top))))))

;;; Producer

(defun hel-thing-producer (thing pos)
  "Return the producer of THING for a search from POS.
The producer comes from the first source in the property `hel-thing-sources' of
THING that returns one for POS. POS is the position that the search starts from,
so the source decides at the same place as the search: in a buffer with embedded
languages, in the same language.

Signal a user error when no source of THING applies at POS, or when POS is nil
because the command has no place to search from."
  (or (if pos
          (->> (get thing 'hel-thing-sources)
               (-some (lambda (source) (funcall source pos)))))
      (user-error "No %s here" thing)))

;;;; The tree-sitter query producer

(cl-defstruct (hel-treesit-query-producer
               (:constructor hel-treesit-query-producer--create)
               (:copier nil))
  "The producer of the things that the text object queries find.
A query captures a thing under the name of the thing, such as `function'.
The parent of a thing is the parent node of its match.")

(defun hel-treesit-source (pos)
  "Return the producer of the tree-sitter text objects at POS, or nil."
  (if (hel-treesit-text-objects-p pos)
      (hel-treesit-query-producer--create)))

(defun hel-treesit-text-objects-p (pos)
  "Return non-nil if the tree-sitter text objects work at POS.
They work when the buffer has a parser, Emacs is version 31 or later,
and a text object query exists for the language at POS."
  (and (hel-treesit-active-p)
       ;; We need the `treesit-query-capture' GROUPED argument which was
       ;; added in Emacs 31.
       (<= 31 emacs-major-version)
       (hel-treesit--query (treesit-language-at pos))
       t))

(cl-defmethod hel-thing-at (thing (_producer hel-treesit-query-producer) pos)
  "Capture the things in the whitespace around POS.
A linewise thing ends after its line end, while its node may end before
trailing blanks, so the query covers the whitespace on both sides."
  (let ((things (hel-treesit--capture-things
                 thing
                 (max (point-min) (1- (hel-after-whitespace pos -1)))
                 (min (point-max) (1+ (hel-after-whitespace pos 1))))))
    (-some->> things
      (-filter (lambda (instance)
                 (and (<= (hel-thing-instance-begin instance) pos)
                      (< pos (hel-thing-instance-end instance)))))
      (-min-by (-on #'> (-compose #'hel-range-length
                                  #'hel-thing-instance-bounds))))))

(cl-defmethod hel-thing-candidates (thing (producer hel-treesit-query-producer) pos)
  "Return the things that hold POS, and the things next to it.
A thing is next to POS when only whitespace and separators lie between
them. The thing after the comments behind POS counts too, because these
comments may be its header comment."
  (cl-flet ((after-comments (pos)
              ;; Return the end of the comments and the whitespace forward.
              (while-let ((comment (hel-bounds-of-comment-at pos))
                          ((< pos (cdr comment))))
                (setq pos (hel-after-whitespace (cdr comment) 1)))
              pos))
    (let ((from (max (-> (hel-thing-after-gap thing producer pos -1)
                         (1-))
                     (point-min)))
          (to   (min (-> (hel-thing-after-gap thing producer pos 1)
                         (after-comments)
                         (1+))
                     (point-max))))
      (hel-treesit--capture-things thing from to))))

(cl-defmethod hel-thing-next (thing (_producer hel-treesit-query-producer) pos dir)
  (when (if (< dir 0)
            (< (point-min) pos)
          (< pos (point-max)))
    (let ((start (if (< dir 0) pos (1+ pos))))
      (hel--find-nearest (-partial #'hel-treesit--capture-things thing)
                         start dir :key #'hel-thing-instance-bounds))))

(cl-defmethod hel-thing-parent ( _thing (_producer hel-treesit-query-producer)
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
two arguments. It lies between two things, and no query captures it."
  (cl-flet ((after-separator (pos dir)
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
    (let ((end pos)
          old)
      (while (not (eql old end))
        (setq old end
              end (-> end
                      (hel-after-whitespace dir)
                      (after-separator dir))))
      end)))

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
`hel-treesit--queries' keeps the result of each language. A pattern that
the installed grammar cannot compile is left out, so a node type that the
grammar lacks costs one pattern and not the whole query. A query file that
cannot be split into patterns gives no query, and a message names the
language."
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

(defun hel-motion-source (motion)
  "Return a source of the things that MOTION moves over.
A source is a function of a position that returns a producer. This one
returns the same producer at every position."
  (let ((producer (hel-motion-producer--create :function motion)))
    (lambda (_pos) producer)))

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
              ((<= (car bounds) pos))
              ((< pos (cdr bounds))))
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
;;;; Word

(defconst hel--bracket-pairs '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))
  "Opening and closing brackets that enclose a part of a text.")

(defconst hel--quote-pairs
  '((?\" . ?\") (?` . ?`) (?` . ?\')
    (?« . ?») (?„ . ?“) (?“ . ?”) (?‘ . ?’))
  "Opening and closing quotation marks.
A character may participate in more than one pair. For example, a backquote may
pair with another backquote in Markdown or with an apostrophe in Emacs Lisp and
TeX. The character “ closes the German quote opened by „ and opens the English
quote closed by ”.")

(defconst hel--punctuation-after-word '(?, ?. ?\; ?: ?! ??)
  "Punctuation marks that separate a word from the text following it.")

(defconst hel--punctuation-before-word '(?¿ ?¡)
  "Punctuation marks that separate a word from the text before it.")

(cl-defstruct (hel-word-rules (:constructor hel-word-rules-create)
                              (:copier nil))
  "The rules that split the text of a buffer into words.
Each field is a value that a major mode can set: see `hel-word-rules'."
  (brackets hel--bracket-pairs :documentation "\
The pairs of brackets that build the fences between words, as an alist
of the opening and closing characters. A character of these pairs at the
edge of a run of non-blank characters is a fence when its pair is not in
the run, see `hel-word-motion-producer'. The motions over words and
WORDs step over a fence and never select it, so in `(defun f (n 1))' `W'
selects `defun', `f', `n' and `1'. A pair with both halves in the run
stays in the body, as in `foo(bar)' and `(n)'.")
  (pairs hel--quote-pairs :documentation "\
The pairs of characters that `miW' leaves out at the edges of a WORD,
besides the brackets, as an alist of the opening and closing characters.
A character of these pairs at an edge goes when its pair is not in the
WORD, and stays when its pair is inside or at the other edge, as in
\"foo\". The pairs `<>' are not here by default, because they are
operators in most code, as in `->'.")
  (separators nil :documentation "\
The characters that separate words as a line end does, such as `|' in
an Org table. A separator is a fence, see `hel-word-motion-producer'."))

(defvar-local hel-word-rules (hel-word-rules-create)
  "The rules that split the text of the buffer into words.
A major mode sets its own rules in its hook: Org adds the markers of
verbatim, code and bold text to the brackets, `<>' to the pairs for its
timestamps, and the `|' of its tables to the separators.")

(cl-defstruct (hel-word-motion-producer
               (:include hel-motion-producer)
               (:constructor hel-word-motion-producer--create)
               (:copier nil))
  "The producer of the word things.
A word is a body, and a fence stands between two bodies. The body of
a `word' is a run of word characters or of punctuation, and the body of
a `WORD' is a run of non-blank characters.

A fence is a line end, a separator, or a bracket at the edge of a run of
non-blank characters that does not hold its pair, see `hel-word-rules'.
Fences in a row, with the blanks between them, count as one fence. The
indentation of a line is not a part of a fence. A motion to the edge of
a word never crosses a fence: the fence and the indentation are steps of
their own.")

(defun hel-word-motion-source (motion)
  "Return a source of the words that MOTION moves over.
MOTION stops at the fence between words, see `hel-word-motion-producer'.
A source is a function of a position that returns a producer. This one
returns the same producer at every position."
  (let ((producer (hel-word-motion-producer--create :function motion)))
    (lambda (_pos) producer)))

(put 'word 'hel-thing-no-jump t)
(put 'word 'hel-thing-sources (list (hel-word-motion-source #'hel-word-motion)))

(put 'WORD 'hel-thing-no-jump t)
(put 'WORD 'hel-thing-sources (list (hel-word-motion-source #'hel-WORD-motion)))

(cl-defun hel-word-motion (&optional (count 1))
  "Move point COUNT words forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.
The motion stops before a fence, see `hel-word-motion-producer'."
  (hel-motion-loop (dir count)
    (hel--word-step #'hel--word-skip-body dir)))

(cl-defun hel-WORD-motion (&optional (count 1))
  "Move point COUNT WORDs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.
The motion stops before a fence, see `hel-word-motion-producer'."
  (hel-motion-loop (dir count)
    (hel--word-step #'hel--WORD-skip-body dir)))

(defun hel--word-skip-body (dir)
  "Move point in DIR over the body of a word.
The body is a run of characters matching `[[:word:]]', or a run of
characters that are neither word characters nor whitespace."
  (or (/= 0 (hel-skip-chars (concat "^[:word:]" hel-whitespace-chars) dir))
      (let ((word-separating-categories hel-cjk-word-separating-categories)
            (word-combining-categories  hel-cjk-word-combining-categories))
        (forward-word dir))))

(defun hel--WORD-skip-body (dir)
  "Move point in DIR over the body of a WORD: a run of non-blank characters."
  (hel-skip-chars (concat "^" hel-whitespace-chars) dir))

(defun hel--word-step (skip-body dir)
  "Move point one word in DIR, stopping before a fence.
A fence and the blanks before it are each a step of their own, see
`hel-word-motion-producer'. SKIP-BODY is a function of DIR that moves
point over the body of one word."
  (let ((origin (point)))
    (hel--word-skip-fence dir)
    (cond ((and (= 0 (hel-skip-blanks dir))
                (if (< dir 0) (bobp) (eobp)))
           ;; Only a fence is left: no step.
           (goto-char origin))
          ((if (< dir 0) (bobp) (eobp))
           ;; Blanks at the edge of the buffer are a step, as in Helix.
           nil)
          ((or (hel-line-boundary-p dir)
               ;; A fence comes next: the blanks were a step of their own.
               (let ((next (if (< dir 0) (preceding-char) (following-char))))
                 (and (or (hel--word-bracket-p next)
                          (memq next (hel-word-rules-separators hel-word-rules)))
                      (save-excursion (/= 0 (hel--word-skip-fence dir)))))))
          (t
           (let ((pos (if (< dir 0) (1- (point)) (point))))
             (funcall skip-body dir)
             ;; The run may end with a fence: stop before it.
             (when (hel--word-bracket-p (if (< dir 0) (following-char) (preceding-char)))
               (-let [(_ body-begin body-end _) (hel--word-body-bounds pos)]
                 (goto-char (if (< dir 0)
                                (max (point) body-begin)
                              (min (point) body-end))))))))))

(cl-defmethod hel-thing-motion-range ( _thing (producer hel-word-motion-producer)
                                       pos count edge _skip-gap?)
  "Step over a fence and over the indentation of a line one at a time."
  (save-excursion
    (goto-char pos)
    (let ((word-motion (hel-motion-producer-function producer))
          (backward-to-end? (and (< count 0) (eq edge :end))))
      (if backward-to-end?
          ;; As in Helix, the newlines before POS are not a step.
          (skip-chars-backward "\r\n")
        (let ((dir (if (< count 0) -1 1)))
          (when (and (/= 0 (hel--word-skip-fence dir))
                     (eq edge (if (< dir 0) :begin :end)))
            ;; `e' and `b' take the blanks before the word into the step,
            ;; and the blanks after a bracket fence are those blanks.
            ;; A separator keeps the blanks on both sides: they pad
            ;; a table cell.
            (let ((after-fence (point)))
              (hel-skip-blanks (- dir))
              (when (memq (if (< dir 0) (following-char) (preceding-char))
                          (hel-word-rules-separators hel-word-rules))
                (goto-char after-fence))))))
      (let ((start (point)))
        (cond (backward-to-end?
               (hel-motion-loop (dir count)
                 (cond ((/= 0 (hel--word-skip-fence dir)))
                       ((hel-blank-p (preceding-char))
                        (hel-skip-blanks dir))
                       (t
                        (funcall word-motion dir)
                        (hel-skip-blanks dir)))))
              ((and (< 0 count) (eq edge :begin))
               ;; The blanks before the next word are the first step.
               (when (hel-blank-p (following-char))
                 (cl-decf count))
               (funcall word-motion count)
               (hel-skip-blanks 1))
              (t
               (funcall word-motion count)))
        (hel-range start (point))))))

(cl-defmethod hel-thing-nearest ((_thing (eql 'word))
                                 (_producer hel-word-motion-producer) pos)
  "On a fence at the edge of a word, return that word."
  (or (hel--word-behind-fence 'word pos)
      (cl-call-next-method)))

(cl-defmethod hel-thing-nearest ((_thing (eql 'WORD))
                                 (_producer hel-word-motion-producer) pos)
  "On a fence at the edge of a WORD, return that WORD."
  (or (hel--word-behind-fence 'WORD pos)
      (cl-call-next-method)))

(cl-defmethod hel-thing-after-gap ( _thing (_producer hel-word-motion-producer)
                                    pos dir)
  "The gap between words is the blanks."
  (hel-after-blanks pos dir))

(cl-defmethod hel-thing-gap-end-allowed-p
  (_thing (_producer hel-word-motion-producer) pos dir)
  "A gap may not end at a line end or at a separator either.
A separator is a character such as `|' in an Org table, see `hel-word-rules'."
  (and (cl-call-next-method)
       (not (save-excursion
              (goto-char pos)
              (if (< dir 0) (bolp) (eolp))))
       (not (memq (if (< dir 0) (char-before pos) (char-after pos))
                  (hel-word-rules-separators hel-word-rules)))))

(cl-defmethod hel-thing-apply-rules ((_thing (eql 'WORD)) bounds)
  "Leave out the punctuation around the WORD, see `hel--WORD-trim'."
  (->> (hel--WORD-trim bounds
                       (append (hel-word-rules-brackets hel-word-rules)
                               (hel-word-rules-pairs hel-word-rules))
                       t)
       (cl-call-next-method 'WORD)))

(defun hel--word-body-bounds (pos)
  "Return the run of non-blank characters at POS and the body inside it.
The body is the run without the fences at its edges, see
`hel-word-motion-producer'. The expression prefixes in front of an
opening fence, such as the quote in `\\='(a', belong to the fence.
The result is a list of four positions: the start of the run, the start
and the end of the body, and the end of the run. The character after POS
must belong to the run."
  (save-excursion
    (-let* ((not-whitespace (concat "^" hel-whitespace-chars))
            (brackets (hel-word-rules-brackets hel-word-rules))
            (begin (progn (goto-char pos)
                          (skip-chars-backward not-whitespace)
                          (point)))
            (end (progn (goto-char pos)
                        (skip-chars-forward not-whitespace)
                        (point)))
            (body (hel--WORD-trim (cons begin end) brackets))
            (after-prefixes (progn (goto-char begin)
                                   (skip-syntax-forward "'" end)
                                   (point)))
            ((body-begin . body-end)
             (or (and (= (car body) begin)
                      (< begin after-prefixes end)
                      (assq (char-after after-prefixes) brackets)
                      (let ((without-prefixes
                             (hel--WORD-trim (cons after-prefixes end) brackets)))
                        (and (< after-prefixes (car without-prefixes))
                             without-prefixes)))
                 body)))
      (list begin body-begin body-end end))))

(defun hel--word-behind-fence (word-thing pos)
  "Return the WORD-THING (`word' or `WORD') behind the fence at POS, or nil.
The fence is a bracket at the edge of a run of non-blank characters, see
`hel-word-motion-producer'."
  (when (hel--word-bracket-p (char-after pos))
    (-let [(_ body-begin body-end _) (hel--word-body-bounds pos)]
      (unless (and (<= body-begin pos) (< pos body-end))
        (hel-thing-instance-create (->> (cons body-begin body-end)
                                        (hel-thing-apply-rules word-thing)))))))

(defun hel--word-skip-fence (dir)
  "Move point in DIR over the fence between words.
Fences in a row, with the blanks between them, count as one fence, see
`hel-word-motion-producer'. The indentation of a line stays: it is
a step of its own. Return the distance traveled, with the sign of DIR."
  (cl-flet* ((skip-blanks ()
               ;; Skip the blanks in DIR unless they are the indentation:
               ;; only blanks lie between point and the line start.
               (unless (save-excursion (hel-skip-blanks -1) (bolp))
                 (hel-skip-blanks dir))))
    (let ((start (point))
          (separators (hel-word-rules-separators hel-word-rules)))
      (while (or (/= 0 (hel-skip-chars "\r\n" dir))
                 ;; Skip a separator with the blanks around it.
                 (and separators
                      (let ((origin (point)))
                        (skip-blanks)
                        (if (memq (if (< dir 0) (char-before) (char-after))
                                  separators)
                            (progn (forward-char dir)
                                   (skip-blanks)
                                   t)
                          (goto-char origin)
                          nil)))
                 ;; Move over the brackets at the edge of the next word, and
                 ;; return non-nil if point moved. Most steps meet no fence.
                 ;; Look past the whitespace first: when the character there
                 ;; is no bracket, no fence comes next.
                 (when (hel--word-bracket-p
                        (save-excursion
                          (if (< dir 0)
                              (progn (skip-syntax-backward " ")
                                     (char-before))
                            (skip-syntax-forward " ")
                            (char-after))))
                   (let ((origin (point)))
                     (skip-blanks)
                     (let ((pos (if (< dir 0) (1- (point)) (point))))
                       (if (and (hel--word-bracket-p (char-after pos))
                                (-let [(begin body-begin body-end end)
                                       (hel--word-body-bounds pos)]
                                  (cond ((< pos body-begin)
                                         (goto-char (if (< dir 0) begin body-begin)))
                                        ((<= body-end pos)
                                         (goto-char (if (< dir 0) body-end end))))))
                           (progn (skip-blanks) t)
                         ;; else
                         (goto-char origin)
                         nil))))))
      (- (point) start))))

(defun hel--word-bracket-p (char)
  "Return non-nil if CHAR is a bracket of the word rules.
The brackets are the characters of the pairs in the field `brackets' of
`hel-word-rules', and the expression prefixes, such as the Lisp quote:
a prefix belongs to the fence of the bracket after it. A bracket is
a fence only at the edge of a run of non-blank characters that does not
hold its pair, see `hel--word-body-bounds'."
  (and char
       (or (assq  char (hel-word-rules-brackets hel-word-rules))
           (rassq char (hel-word-rules-brackets hel-word-rules))
           (eq ?\' (char-syntax char)))))

(defun hel--WORD-trim (bounds pairs &optional punctuation?)
  "Return the bounds of the WORD in BOUNDS without the punctuation at its edges.
A character of PAIRS at an edge goes when its pair is not in the WORD. It
stays when its pair is inside or at the other edge, as in `foo(bar)',
`(foo)' and \"foo\". A run of a character that is its own pair counts
as one character, so `**foo**' stays and `**foo' loses its stars. An
apostrophe at the end stays without the backquote that it closes,
because it is a part of the word, as in a possessive.

With PUNCTUATION? non-nil, the punctuation that separates words goes too,
each mark from its own side: `,.;:!?' from the end, so `foo,' loses its
comma but `:foo' keeps its colon, and `¿¡' from the start. A mark at the
end stays when the WORD starts with it, as in `:tag:'.

When nothing would be left, return BOUNDS. BOUNDS is a (BEGIN . END)
pair, and PAIRS is an alist of the opening and closing characters."
  (-let* (((begin . end) bounds)
          (punctuation-after  (if punctuation? hel--punctuation-after-word))
          (punctuation-before (if punctuation? hel--punctuation-before-word)))
    (cl-labels ((pair-pos (pos open close dir)
                  ;; The position of the pair of the character at POS,
                  ;; which is OPEN forward and CLOSE backward, or nil.
                  (let ((depth 0)
                        (i (+ pos dir))
                        found)
                    (when (eq open close)
                      ;; Skip the rest of a run, such as `**'.
                      (while (and (<= begin i) (< i end)
                                  (eq (char-after i) open))
                        (cl-incf i dir)))
                    (while (and (not found) (<= begin i) (< i end))
                      (let ((char (char-after i)))
                        (cond ((eq char (if (< dir 0) open close))
                               (if (zerop depth)
                                   (setq found i)
                                 (cl-decf depth)))
                              ((eq char (if (< dir 0) close open))
                               (cl-incf depth))))
                      (cl-incf i dir))
                    found))
                (trim-begin ()
                  (let* ((char (char-after begin))
                         (closes (->> pairs
                                      (-keep (-lambda ((open . close))
                                               (and (eq open char) close))))))
                    (cond ((memq char punctuation-before)
                           (cl-incf begin))
                          ((not closes) nil)
                          ((->> closes (-any (lambda (close)
                                               (pair-pos begin char close 1))))
                           nil)
                          (t (cl-incf begin)))))
                (trim-end ()
                  (let* ((char (char-before end))
                         (opens (->> pairs
                                     (-keep (-lambda ((open . close))
                                              (and (eq close char) open))))))
                    (cond ((and (memq char punctuation-after)
                                (not (eq char (char-after begin))))
                           (cl-decf end))
                          ((not opens) nil)
                          ((->> opens (-any (lambda (open)
                                              (pair-pos (1- end) open char -1))))
                           nil)
                          ((eq char ?\') nil)
                          (t (cl-decf end))))))
      (while (and (< begin end)
                  (or (trim-begin)
                      (and (< begin end)
                           (trim-end)))))
      (if (< begin end)
          (cons begin end)
        bounds))))

;;;; Sentence

(put 'sentence 'hel-thing-sources (list (hel-motion-source #'hel-sentence-motion)))

(cl-defun hel-sentence-motion (&optional (count 1))
  "Move point COUNT sentences forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.

The variable `forward-sentence-function' defines what a sentence is."
  (save-restriction
    ;; Narrow to string or comment, since sentence can never extend beyond them.
    (when-let* ((ppss (syntax-ppss))
                (begin (nth 8 ppss))
                ;; Inside a string or a comment the motion constrained by them,
                ;; because a sentence never goes out from them.
                (bounds (if (nth 3 ppss) ; inside string
                            (when-let* ((end (ignore-errors
                                               (scan-sexps begin 1))))
                              (cons (1+ begin) (1- end)))
                          ;; else
                          (-when-let ((begin . end)
                                      (hel-bounds-of-comment-at (point)))
                            ;; Start after the comment starter, such as `;;'.
                            (save-excursion
                              (goto-char begin)
                              (comment-normalize-vars t)
                              (comment-search-forward end t)
                              (cons (point) end))))))
      (narrow-to-region (car bounds) (cdr bounds)))
    (hel-motion-loop (dir count)
      (ignore-errors (forward-sentence dir)))))

(cl-defmethod hel-thing-after-gap ((_thing (eql 'sentence)) _producer pos dir)
  "The gap between sentences is the blanks."
  (hel-after-blanks pos dir))

(cl-defmethod hel-thing-gap-end-allowed-p
  ((_thing (eql 'sentence)) _producer pos dir)
  "A gap may not end at a line end either."
  (and (cl-call-next-method)
       (not (save-excursion
              (goto-char pos)
              (if (< dir 0) (bolp) (eolp))))))

(cl-defmethod hel-thing-parent ((_thing (eql 'sentence)) _producer sentence)
  "Return the start of the paragraph that SENTENCE belongs to."
  (save-excursion
    (goto-char (min (1+ (hel-thing-instance-begin sentence))
                    (point-max)))
    (hel-paragraph-motion -1)
    (point)))

;;;; Paragraph

(put 'paragraph 'hel-thing-linewise t)
(put 'paragraph 'hel-thing-selection-direction -1)
(put 'paragraph 'hel-thing-sources (list (hel-motion-source
                                          #'hel-paragraph-motion)))

(cl-defun hel-paragraph-motion (&optional (count 1))
  "Move point COUNT paragraphs forward, or backward when COUNT is negative.
Return the part of COUNT that is left to move, with the sign of COUNT.
A paragraph is a run of lines between blank lines, as in Vim and Helix.
So the motion uses the default values of `paragraph-start' and
`paragraph-separate', because major modes such as Org and Markdown set
their own values, which define a paragraph in another way. Backward, the
motion stops at the start of the first line of a paragraph."
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (hel-motion-loop (dir count)
      (cond ((< 0 dir)
             (forward-paragraph))
            ((not (bobp))
             (unless (bolp) (end-of-line))
             (start-of-paragraph-text)
             (beginning-of-line))))))

;;;; Comment

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
               (:constructor hel-syntax-comment-producer--create)
               (:copier nil))
  "The producer of the comments that the syntax table finds.")

(defun hel-syntax-comment-source (_pos)
  "Return the producer of the comments that the syntax table finds."
  (hel-syntax-comment-producer--create))

(cl-defstruct (hel-treesit-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-treesit-comment-producer--create)
               (:copier nil))
  "The producer of the comments that a tree-sitter query finds.")

(defun hel-treesit-comment-source (pos)
  "Return the producer of the comments that the tree-sitter query finds.
Return nil when the tree-sitter text objects do not work at POS."
  (when (hel-treesit-text-objects-p pos)
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
  (when-let* ((producer (hel-thing-producer 'comment pos))
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
  (when-let* ((bounds (-some->> (hel--comment-run-at producer pos)
                        (hel-thing-apply-rules 'comment)))
              ((<= (car bounds) pos))
              ((< pos (cdr bounds))))
    (hel-thing-instance-create bounds)))

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
  "For a full-line comment, treat blank lines as the gap.
For an inline comment, limit the gap to horizontal whitespace on
the same line, keeping the comment attached to its code line."
  (if (save-excursion (goto-char pos) (bolp))
      (hel-after-whitespace pos dir)
    (hel-after-blanks pos dir)))

(cl-defmethod hel-thing-gap-end-allowed-p ((_thing (eql 'comment)) _producer pos dir)
  "Backward, a gap that ends inside a line may end anywhere.
Such a gap lies between code and an inline comment after it, as in
`{ // comment', and `mac' takes it even after an opening bracket. The
gap before a full-line comment ends at the end of the line before, and
there the default rules hold."
  (or (cl-call-next-method)
      (and (< dir 0)
           (not (save-excursion (goto-char pos) (eolp))))))

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

;;;; Function

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

;;;; Class

(put 'class 'hel-thing-linewise t)
(put 'class 'hel-thing-owns-header-comment t)
(put 'class 'hel-thing-selection-direction -1)
(put 'class 'hel-thing-sources (list #'hel-treesit-source))

;;;; Argument

(put 'argument 'hel-thing-no-jump t)
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

(defun hel-syntax-argument-source (_pos)
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
    (->> runs
         (-some (lambda (run)
                  (-first (lambda (instance)
                            (and (<= (hel-thing-instance-begin instance) pos)
                                 (< pos (hel-thing-instance-end instance))))
                          run))))))

(cl-defmethod hel-thing-next (thing (_producer hel-syntax-argument-producer)
                                    pos dir)
  (when (if (< dir 0)
            (< (point-min) pos)
          (< pos (point-max)))
    (let ((start (if (< dir 0) pos (1+ pos))))
      (hel--find-nearest (-partial #'hel--syntax-arguments-in thing)
                         start dir :key #'hel-thing-instance-bounds))))

(cl-defmethod hel-thing-parent ( _thing (_producer hel-syntax-argument-producer)
                                 instance)
  "Return the start of the parentheses around INSTANCE."
  (hel-thing-instance-data instance))

(cl-defmethod hel-thing-after-gap ( _thing (_producer hel-syntax-argument-producer)
                                    pos dir)
  "Return the end of the whitespace and the commas from POS in DIR."
  (cl-flet ((after-comma (pos dir)
              (let ((next-char (if (< dir 0) (char-before pos) (char-after pos))))
                (if (eq ?, next-char)
                    (+ pos dir)
                  pos))))
    (let ((end pos)
          old)
      (while (not (eql old end))
        (setq old end
              end (-> end
                      (hel-after-whitespace dir)
                      (after-comma dir))))
      end)))

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
A THING is the text between two commas at the level of the parentheses, or
between a comma and a parenthesis, without the whitespace and the comments at
its edges. An empty piece, such as the one after a trailing comma, is no THING.
Each instance keeps OPEN as its data. The time is linear in the size of
the parentheses."
  (save-excursion
    (when-let* ((close (ignore-errors (1- (scan-lists open 1 0)))))
      (let ((begin (1+ open))
            instances)
        (cl-flet ((add (begin end)
                    (save-excursion
                      (when-let* ((text-begin (progn
                                                (goto-char begin)
                                                (forward-comment (buffer-size))
                                                (point)))
                                  (text-end (progn
                                              (goto-char end)
                                              (forward-comment (- (buffer-size)))
                                              (point)))
                                  ((< text-begin text-end))
                                  (bounds (->> (cons text-begin text-end)
                                               (hel-thing-apply-rules thing))))
                        (push (hel-thing-instance-create bounds :data open)
                              instances)))))
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

;;;; Entry
;;
;; An entry is one element of a data structure: an element of a list or
;; an array, a key and its value in a dictionary, a field of a struct.

(put 'entry 'hel-thing-no-jump t)
(put 'entry 'hel-thing-owns-header-comment t)
(put 'entry 'hel-thing-sources (list #'hel-treesit-source))

;;; .
(provide 'hel-text-objects)
;;; hel-text-objects.el ends here
