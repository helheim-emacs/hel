;;; hel-lib.el --- Common functions -*- lexical-binding: t -*-
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Functions shared by the rest of Hel.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'map)
(require 'pcre2el)
(require 'pulse)
(require 'hel-vars)

;;; Macros

(cl-defmacro hel-motion-loop ((dir count) &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with DIR bound to 1 or -1, depending on
the sign of COUNT. Each iteration must move point; if point does not change,
the loop immediately quits. Emacs motions return different values or nothing,
so point is the one reliable sign that a step worked.

Returns the count of steps left to move.  If moving forward, that is
COUNT minus number of steps moved; if backward, COUNT plus number moved."
  (declare (indent 1)
           (debug ((symbolp form) body)))
  (macroexp-let2 symbolp n count
    `(let ((,dir (hel-sign ,n)))
       (while (and (/= ,n 0)
                   (/= (point) (progn ,@body (point))))
         (cl-callf - ,n ,dir))
       ,n)))

(defmacro hel-recenter-point-on-jump (&rest body)
  "Recenter point on jumps during BODY evaluating if it lands out of the screen.
This macro calls `redisplay' internally and should be used with care to avoid
flickering."
  (declare (indent 0) (debug t))
  `(let ((scroll-conservatively 0))
     (prog1 (progn ,@body)
       ;; Update the screen so that the temporary value for
       ;; `scroll-conservatively' is taken into account.
       (unless hel-executing-command-for-secondary-cursor
         (redisplay)))))

(defmacro hel-save-selection (&rest body)
  "Evaluate BODY, then restore the selection.
Unlike `save-mark-and-excursion', the selection follows its text.
Text that BODY inserts at either end of the selection stays outside it,
and when BODY replaces the text of the selection, the selection covers
the new text. The selection comes back as the value that `hel-selection'
returned before BODY, so a bare cursor stays a bare cursor and the line
selection of an empty line stays a line selection."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (begin end dir line?)
    `(-let* ((((,begin . ,end) ,dir ,line?) (hel-selection))
             (,begin (copy-marker ,begin t))
             (,end (copy-marker ,end)))
       (unwind-protect
           (let ((deactivate-mark nil))
             (save-excursion ,@body))
         ;; A bare cursor has no direction. When BODY has replaced its character
         ;; with longer text, the selection of that text is forward.
         (hel-select (hel-range ,begin ,end) (or ,dir 1) ,line?)
         (set-marker ,begin nil)
         (set-marker ,end nil)))))

(defmacro hel-restore-selection-on-error (&rest body)
  "Evaluate BODY, and restore the selection if BODY signals an error."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (selection something-goes-wrong?)
    `(let ((,selection (hel-selection))
           (,something-goes-wrong? t))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,something-goes-wrong? nil))
         (when ,something-goes-wrong?
           (apply #'hel-select ,selection))))))

;;; Utils

(defun hel-unquote (form)
  "Strip a leading `\'' (`quote') or `#\'' (`function') from FORM."
  (if (memq (car-safe form) '(quote function))
      (cadr form)
    form))

(defun hel--exchange-point-and-mark ()
  "Exchange point and mark."
  (goto-char (prog1 (marker-position (mark-marker))
               (set-marker (mark-marker) (point)))))

(defun hel-bolp ()
  "Like `bolp' but consider visual lines when `visual-line-mode' is enabled."
  (if visual-line-mode
      (= (point) (hel-line-begin))
    (bolp)))

(defun hel-line-boundary-p (direction)
  "If DIRECTION is negative number, checks for beginning of line,
positive — end of line."
  (if (< direction 0) (bolp) (eolp)))

(defsubst hel-sign (num)
  "Return the sign of NUM as -1, 0, or 1."
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(defsubst hel-distance (x y)
  "Return the absolute distance between X and Y."
  (abs (- y x)))

(defsubst hel-clamp (min-val val max-val)
  "Return VAL clamped to the range [MIN-VAL, MAX-VAL]."
  (max min-val (min val max-val)))

(cl-defun hel-looking-at (string &optional (direction 1) regexp?)
  "Return t if text directly after point toward the DIRECTION
matches STRING.

If REGEXP? is non-nil STRING will be searched as regexp pattern,
otherwise it will be searched literally.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (if regexp?
      (if (< 0 direction)
          (looking-at string)
        (looking-back string (line-beginning-position)))
    ;; literall
    (let* ((begin (point))
           (end (+ begin (* (length string) direction))))
      (and (<= (point-min) end (point-max))
           (string-equal (buffer-substring-no-properties begin end) string)))))

(defun hel-string-ends-with-newline (string)
  "Return t if STRING ends with newline character."
  (eql (elt string (1- (length string)))
       ?\n))

(cl-defun hel-all-elements-are-the-same-p (list &key (test #'equal))
  "Return t if all elements in the LIST are the same."
  (let ((first (car list)))
    (-all? (lambda (x) (funcall test first x))
           (cdr list))))

(defun hel-range (a b)
  "Return the range between positions A and B, which can come in any order."
  (cons (min a b) (max a b)))

(defun hel-range-length (range)
  "Return the length of RANGE: a (BEGIN . END) pair."
  (- (cdr range) (car range)))

(defun hel-range-strictly-inside-p (inner outer)
  "Return non-nil if the range OUTER holds the range INNER and is larger."
  (and (<= (car outer) (car inner) (cdr inner) (cdr outer))
       (not (equal inner outer))))

(defun hel-char-range (pos)
  "Return the range of the character at POS.
At the end of the buffer there is no character, and the range is empty."
  (cons pos (min (1+ pos) (point-max))))

(defun hel-destructive-filter (predicate list &optional pointer)
  "Destructively remove elements in LIST that satisfy PREDICATE
between start and POINTER.

Returns the modified list, which may have a new starting element
if removals occur at the beginning of the list, therefore, assign
the returned list to the original symbol like this:

  (setq foo (hel-destructive-filter #\\='predicate foo))"
  (let ((tail list)
        elem head)
    (while (and tail (not (eq tail pointer)))
      (setq elem (car tail))
      (if (funcall predicate elem)
          (progn
            (setq tail (cdr tail))
            (if head
                (setcdr head tail)
              (setq list tail)))
        ;; else advance
        (setq head tail
              tail (cdr tail))))
    list))

(defun hel-pcre-to-elisp (regexp)
  "Convert PCRE REGEXP into Elisp one if Hel configured to use PCRE syntax.
When REGEXP is invalid PCRE, show the error and return nil."
  (if (and hel-use-pcre-regex
           (not (string-empty-p regexp)))
      (condition-case err
          (pcre-to-elisp regexp)
        (rxt-invalid-regexp
         (message (-> (error-message-string err)
                      (propertize 'face 'error)))
         nil))
    regexp))

(cl-defun hel-collect-positions (fun &optional (begin (window-start)) (end (window-end)))
  "Consecutively call FUN and collect point positions after each invocation.
Finish as soon as point moves outside of BEGIN END buffer positions.
FUN on each invocation should move point."
  (save-excursion
    (cl-loop with win = (get-buffer-window)
             for old-point = (point)
             do (ignore-errors
                  ;; Bind `last-command' and `this-command' to the same value,
                  ;; to get uniform result in case `fun' behaves differently
                  ;; depending on their values.
                  (let ((last-command fun)
                        (this-command fun))
                    (call-interactively fun)))
             while (and (/= (point) old-point)
                        (<= begin (point) end))
             collect (cons (point) win))))

(cl-defun hel-invert-case-in-range ((begin . end))
  "Invert case of characters in the range BEGIN..END."
  (goto-char begin)
  (while (< (point) end)
    (let ((char (following-char)))
      (delete-char 1)
      (insert-char (if (eq (upcase char) char)
                       (downcase char)
                     (upcase char))))))

(defun hel-letters-are-self-insert-p ()
  "Return t if any of the a-z keys are bound to self-insert command."
  (-any (lambda (key)
          (and-let* ((cmd (key-binding key))
                     ((symbolp cmd))
                     ((string-match-p "\\`.*self-insert.*\\'"
                                      (symbol-name cmd))))))
        ;; This is just a fancy way to produce ("a"..."z") list in compile time.
        ;; I just couldn't help myself :)
        (eval-when-compile
          (-map #'char-to-string (number-sequence ?a ?z)))))

(defun hel-overlay-live-p (overlay)
  "Return non-nil if OVERLAY is not deleted from buffer."
  (-some-> overlay
    (overlay-buffer)
    (buffer-live-p)))

(defun hel-pulse-main-selection (&optional face)
  "Flash the selection of the main cursor with FACE."
  (-let [(begin . end) (hel-selection-bounds)]
    (pulse-momentary-highlight-region begin end face)))

(defun hel-reveal-point-when-on-top (&rest _)
  "Reveal point when it's only partially visible.
For some reason, Emacs can become slow while point is partially visible, so this
function prevents that. It is intended to be used as `:after' advice."
  (unless hel-executing-command-for-secondary-cursor
    (redisplay)
    (when (zerop (cdr (posn-col-row (posn-at-point))))
      (recenter 0))))

(defun hel-split-keyword-args (args)
  "Split ARGS list into keyword-value pairs and remaining arguments.
Returns a cons cell (PLIST . REST) where:
PLIST is a list with keyword-value pairs from the beginning of ARGS list;
REST contains all other elements."
  (let (plist)
    (while (keywordp (car-safe args))
      (cl-callf plist-put plist (car args) (cadr args))
      (cl-callf cddr args)) ; advance by 2
    (cons plist args)))

(defun hel-transpose (lol)
  "Transpose list of lists.
  ((1 2 3)    ((1 1 1)
   (1 2)   =>  (2 2)
   (1))        (3))"
  (let (result)
    (while (progn
             (push (-map #'car lol) result)
             (setq lol (-non-nil (-map #'cdr lol)))))
    (nreverse result)))

(cl-defun hel-replace-chars ((begin . end) char)
  "Replace each non-newline character in the range BEGIN..END with CHAR."
  (save-excursion
    (goto-char begin)
    (while (< (point) end)
      (if (eq (char-after) ?\n)
          (forward-char 1)
        (insert-char char)
        (delete-char 1)))))

(cl-defun hel-read-char-and-replace ((begin . end))
  "Replace characters in the range BEGIN..END with an interactive entered one."
  (when (< begin end)
    (let ((overlay (-doto (make-overlay begin end nil t nil)
                     (overlay-put 'face 'region))))
      (unwind-protect
          (hel-replace-chars (cons begin end) (read-char "replace: " t))
        (delete-overlay overlay)))))

(cl-defun hel-hide-cursor (&optional (window (selected-window)))
  "Hide the cursor in WINDOW and return a function that restores it."
  (cond ((and (fboundp 'window-cursor-type) ;; Emacs 30.1
              (fboundp 'set-window-cursor-type))
         (let ((orig (window-cursor-type window)))
           (set-window-cursor-type window nil)
           (lambda () (set-window-cursor-type window orig))))
        ((local-variable-p 'cursor-type)
         (let ((orig cursor-type))
           (setq-local cursor-type nil)
           (lambda () (setq-local cursor-type orig))))
        (t
         (setq-local cursor-type nil)
         (lambda () (kill-local-variable 'cursor-type)))))

(defun hel-self-insert-command-p (command)
  "Return non-nil if COMMAND inserts the character that was typed.
A major mode may remap `self-insert-command' like Org mode does."
  (or (eq command 'self-insert-command)
      (eq command (command-remapping 'self-insert-command))))

(defun hel-partition-index (vector pred)
  "Return the first index I in VECTOR for which (PRED VECTOR[I]) is non-nil.
VECTOR must be partitioned by PRED: all elements for which PRED is nil
come before all elements for which it is non-nil.  Return (length VECTOR)
when PRED holds for no element.  Call PRED O(log n) times."
  (let ((low 0)
        (high (length vector)))
    (while (< low high)
      (let ((mid (/ (+ low high) 2)))
        (if (funcall pred (aref vector mid))
            (setq high mid)
          (setq low (1+ mid)))))
    low))

(cl-defun hel-lower-bound (vector value &key (key #'identity))
  "Return the first index I in VECTOR for which (KEY VECTOR[I]) >= VALUE.
This is the first index where VALUE can be inserted with the order kept:
before all elements whose key equals VALUE. VECTOR must be sorted by KEY
in ascending order. Return (length VECTOR) when there is no such index."
  (hel-partition-index vector (lambda (x) (<= value (funcall key x)))))

(cl-defun hel-upper-bound (vector value &key (key #'identity))
  "Return the first index I in VECTOR for which (KEY VECTOR[I]) > VALUE.
This is the last index where VALUE can be inserted with the order kept:
after all elements whose key equals VALUE. VECTOR must be sorted by KEY
in ascending order. Return (length VECTOR) when there is no such index."
  (hel-partition-index vector (lambda (x) (< value (funcall key x)))))

;;; Motions

(cl-defun hel-skip-chars (chars &optional (direction 1))
  "Move point across the characters in CHARS in direction DIRECTION.
CHARS is a set in the form that `skip-chars-forward' takes. Return the
distance traveled, with the sign of DIRECTION."
  (if (< 0 direction)
      (skip-chars-forward chars)
    (skip-chars-backward chars)))

(defun hel-move-to-pos (from to extend)
  "Move the cursor to the position TO, as a motion that passes over text.
Without EXTEND, select the text from FROM to TO, with point at TO; when
FROM is nil, leave a bare cursor at TO, on the character after it.  With
EXTEND, ignore FROM: the selection keeps its other end and extends to
TO.  A selection of one character keeps that character."
  (cond (extend
         (let ((anchor (hel-selection-anchor (hel-selection)
                                              (hel-sign (- to (point))))))
           (hel-select (hel-range anchor to)
                       (if (< to anchor) -1 1))))
        (from
         (hel-select (hel-range from to)
                     (if (< to from) -1 1)))
        (t
         (hel-set-cursor to))))

;; These functions switch between two kinds of coordinates: gaps and characters.
;; A ruler shows the difference:
;;
;;   gap      1   2   3   4   5
;;            │ a │ b │ c │ d │
;;   char       1   2   3   4
;;
;; Emacs positions are gaps, the marks on the ruler. A range (BEGIN . END)
;; is the stretch between two marks, and it holds END − BEGIN characters.
;; Emacs names a character by the gap before it: "b" is character 2, because
;; it lies between gaps 2 and 3. So character C covers the range (C . C+1).
;;
;; The functions think in characters: they move the head character and
;; keep the anchor character.  Three expressions convert between the
;; two kinds of coordinates:
;;
;; - (1- END) goes from a gap to a character. The last character of a range ends
;;   at gap END, so it is character END − 1: in the selection "bc" = (2 . 4),
;;   the last character is "c" = 3. The first character starts at BEGIN and
;;   needs no conversion: "b" = 2.
;; - (+ ... DIR) is one step in characters: the neighbours of character 3
;;   are characters 2 and 4.
;; - (1+ (max CHAR ANCHOR)) goes from a character back to a gap. The new
;;   selection ends where its right character ends, one gap after that
;;   character's own gap.
;;
;; This is also why ranges are half-open, [BEGIN, END): the ruler always
;; has one more mark than it has spaces, so a range of N characters is
;; (BEGIN . BEGIN+N), and an empty range is (P . P).
(defun hel-move-onto-char (char extend)
  "Move the cursor onto the character CHAR.
Without EXTEND, CHAR becomes the cursor character: point goes before
it.  With EXTEND, the selection keeps the character at its other end and
stretches to CHAR.  When CHAR is that character, the selection becomes
a bare cursor on it."
  (if extend
      (-let* ((((begin . end) dir) (hel-selection))
              (anchor (if (and dir (< 0 dir)) begin (1- end))))
        (hel-select (cons (min char anchor) (1+ (max char anchor)))
                    (if (< char anchor) -1 1)))
    (hel-set-cursor char)))

;;; Blanks and whitespace

(defconst hel-whitespace-chars "[:space:]\r\n"
  "The set of whitespace: the blanks and the line ends.
See `hel-blank-p' for a blank.")

(defun hel-blank-p (char)
  "Return non-nil if CHAR is a blank.
A blank is a character with whitespace syntax that is not a line end."
  (and (eql (char-syntax char) ?\s)
       (not (memq char '(?\r ?\n)))))

(cl-defun hel-skip-blanks (&optional (dir 1))
  "Move point across the blanks on its line in direction DIR.
Return the distance traveled, with the sign of DIR."
  (if (< 0 dir)
      (skip-syntax-forward " " (line-end-position))
    (skip-syntax-backward " " (line-beginning-position))))

(cl-defun hel-skip-whitespace (&optional (dir 1))
  "Move point across the whitespace in direction DIR.
Return the distance traveled, with the sign of DIR."
  (hel-skip-chars hel-whitespace-chars dir))

(defun hel-after-blanks (pos dir)
  "Return the position after the blanks on the line from POS in direction DIR."
  (save-excursion
    (goto-char pos)
    (hel-skip-blanks dir)
    (point)))

(defun hel-after-whitespace (pos dir)
  "Return the position after the whitespace from POS in direction DIR."
  (save-excursion
    (goto-char pos)
    (hel-skip-whitespace dir)
    (point)))

(cl-defun hel-trim-whitespace-from-range ((begin . end))
  "Return the range BEGIN..END without the whitespace at its edges.
Return nil if nothing remains."
  (save-excursion
    (setq begin (progn
                  (goto-char begin)
                  (skip-chars-forward hel-whitespace-chars end)
                  (point)))
    (setq end (progn
                (goto-char end)
                (skip-chars-backward hel-whitespace-chars begin)
                (point)))
    (if (< begin end)
        (cons begin end))))

;;; Lines

(cl-defun hel-visible-line-bounds (&optional (pos (point)))
  "Return the bounds of the visible logical line at POS, with its line end.
The built-in `line' thing stops at lines that are invisible. This range
holds them, such as the lines that `outline-minor-mode' folds, so the `x'
key selects a folded section as one line."
  (save-excursion
    (goto-char pos)
    (cons (save-excursion
            (hel-beginning-of-visible-line)
            ;; `hel-beginning-of-visible-line' may leave point after invisible
            ;; characters if line starts with such of these (e.g., with
            ;; a link at column 0 in Org mode). Really move to the beginning
            ;; of the current visible line.
            (beginning-of-line)
            (point))
          (progn
            (hel-forward-line 1)
            (point)))))

;; Adopted from `move-end-of-line'.
(defun hel-forward-line (&optional count)
  "Goto COUNT visible logical lines forward (backward if COUNT is negative).
The difference from `forward-line' is that this function ignores invisible parts
of the buffer (lines folded by `outline-minor-mode' for example) and always
moves only over visible lines."
  (or count (setq count 1))
  (let ((goal-column 0)
        (line-move-visual nil)
        (inhibit-field-text-motion (minibufferp))) ; bug#65980
    (and (line-move count t)
         (not (bobp))
         (while (and (not (bobp))
                     (invisible-p (1- (point))))
           (goto-char (previous-single-char-property-change
                       (point) 'invisible))))))

(defun hel-beginning-of-visible-line ()
  "Move point to the beginning of the current visible logical line.
This is actually refactored `move-beginning-of-line' command."
  (let ((init-point (point)))
    ;; Move to beginning-of-line, ignoring fields and invisible text.
    (let ((inhibit-field-text-motion t))
      (beginning-of-line)
      (while (and (not (bobp)) (invisible-p (1- (point))))
        (goto-char (previous-char-property-change (point)))
        (beginning-of-line)))
    ;; Now find first visible char in the line.
    (while (and (< (point) init-point)
                (invisible-p (point)))
      (goto-char (next-char-property-change (point) init-point)))
    ;; Obey field constraints.
    (goto-char (constrain-to-field (point) init-point nil t nil))))

(cl-defun hel-line-begin (&optional (pos (point)))
  "Return the start of the line at POS.
This is the visual line of the selected window when `visual-line-mode' is
active, and otherwise the visible logical line."
  (save-excursion
    (goto-char pos)
    (if visual-line-mode
        (beginning-of-visual-line)
      (hel-beginning-of-visible-line))
    (point)))

(cl-defun hel-line-end (&optional (pos (point)))
  "Return the end of the line at POS.
This is the visual line of the selected window when `visual-line-mode' is
active, and otherwise the logical line."
  (save-excursion
    (goto-char pos)
    (if visual-line-mode
        (end-of-visual-line)
      (move-end-of-line 1))
    (point)))

(cl-defun hel-line-bounds ((begin . end))
  "Return the bounds of the whole lines from BEGIN to END.
The range runs from the start of the line of BEGIN to the end of the line
of END, with its line end. END must not follow a line end."
  (save-excursion
    (cons (progn
            (goto-char begin)
            (line-beginning-position))
          (progn
            (goto-char end)
            (min (1+ (line-end-position))
                 (point-max))))))

(cl-defun hel-only-text-on-lines-p ((begin . end))
  "Return non-nil if BEGIN..END is the only text on its lines.
Indentation and trailing whitespace do not count."
  (save-excursion
    (and (progn (goto-char (hel-after-blanks begin -1)) (bolp))
         (progn (goto-char (hel-after-blanks end 1)) (eolp)))))

(defun hel-expand-selection-to-full-lines (&optional direction)
  "Extend the selection to the whole lines that it covers.
The selection grows from the beginning of the line of its first character
to the end of the line of its last character, so a bare cursor selects its
line, and a selection that ends with a newline does not take the next line.
Return t, or nil when the selection already consists of whole lines and
stays as it is.  DIRECTION is the direction of the new selection, 1 forward
or -1 backward; by default the selection keeps its direction, and a bare
cursor, which has none, becomes forward."
  (-let [(selection &as (begin . end) dir) (hel-selection)]
    (unless (hel-line-selection-p selection)
      (hel-select (cons (car (hel-visible-line-bounds begin))
                        (cdr (hel-visible-line-bounds (max begin (1- end)))))
                  (or direction dir 1)
                  t)
      t)))

(defun hel--add-final-newline (end)
  "Add a newline at the end of the buffer when END is there on a line
without one, so that the last line is a whole line.  Return END, past
the newline when one was added.  A read-only buffer and the minibuffer
stay as they are."
  (if (and (= end (point-max))
           (not buffer-read-only)
           (not (minibufferp))
           (/= end (save-excursion (goto-char end) (line-beginning-position))))
      ;; The newline is a part of the selection that the caller writes,
      ;; so it must not deactivate the mark.
      (let ((deactivate-mark nil))
        (save-excursion (goto-char end) (insert ?\n) (point)))
    end))

;;; Selection
;;
;; Emacs represents the selection with two positions, `point' and the `mark',
;; and a switch, `mark-active', that indicates whether the text between them is
;; selected. When the switch is off, nothing is selected, and point is simply
;; a position between two characters.
;;
;; Hel always has a selection. When no text is selected, the cursor covers
;; a single character, called the cursor character, and Hel calls this a bare
;; cursor. Hel code therefore does not read `point' and `mark' directly.
;; Instead, we provide `hel-selection', which converts them into a value of the
;; form:
;;
;;   ((BEGIN . END) DIRECTION)
;;
;; The pair is the range of the selection, and DIRECTION indicates which end the
;; cursor is at: 1 for the end, -1 for the beginning. A bare cursor has no
;; direction, so DIRECTION is nil. `hel-select' takes such a value and restores
;; the corresponding `point' and `mark'.
;;
;; We represent the selection value as a plain list rather than a struct for two
;; reasons:
;;
;; - Destructuring the selection value with `-let' is very convenient and
;;   simplifies the API:
;;
;;     (-let [((begin . end) dir) selection]
;;       body)
;;
;; - Lists are faster in Emacs (although structs are much faster in SBCL).
;;   A struct accessor checks the type on every read, and Hel reads the
;;   selection many times during each command, once for every cursor.
;;
;; In almost all cases, the range alone tells us what the user sees. When both
;; ends are at the beginnings of different lines, the selection covers whole
;; lines, as it does when made with `x'. A selection of a single character is
;; a bare cursor. These two rules coincide on an empty line: since the line
;; contains only its newline, `x' selects a single character there, giving the
;; same range as a bare cursor on that line. But the two look different to the
;; user, who expects different behavior from them: `p' after `x' pastes the text
;; on a line of its own, whereas with a bare cursor it pastes the text at the
;; cursor position.
;;
;; To distinguish these cases, the value has a third element: a private flag,
;; represented as `t':
;;
;;   ((BEGIN . END) nil t)
;;
;; The reader adds this flag only when the active region covers the newline of
;; an empty line. All other values omit it. Do not inspect the flag directly.
;; Use `hel-line-selection-p' to check whether a selection covers whole lines,
;; and `hel-bare-cursor-p' to check whether it is a bare cursor. These functions
;; account for the special case of an empty line. Destructuring with `-let'
;; ignores the flag, while `apply' passes it along to `hel-select'.
;;
;; A selection value describes the selection at the time it was read. If the
;; buffer changes, its positions may become stale.

(defun hel-selection ()
  "Return the selection as (RANGE DIRECTION).
The selection is the active region, or the cursor character when the
region is inactive or empty.  RANGE is a (BEGIN . END) pair, and
DIRECTION is the way from mark to point: 1 forward, -1 backward.
A selection of at most one character, such as a bare cursor, has no
direction, and DIRECTION is nil.  RANGE is empty only at the end of the
buffer.

The value may have a third element, a private flag.  Never read it: ask
`hel-line-selection-p' whether the selection is whole lines, and
`hel-bare-cursor-p' whether it is a bare cursor.  On an empty line the
bare cursor and the line selection cover the same range, the newline,
and the flag tells them apart.  Destructuring with `-let' ignores the
flag, and `apply' of `hel-select' to the value restores the selection
with it."
  (hel--selection-from (point) (mark t) (region-active-p)))

(defun hel--selection-from (point mark region-active?)
  "Return the Hel selection for POINT, MARK and REGION-ACTIVE?.
Emacs selects the text from MARK to POINT. Hel keeps the same text as
\(RANGE DIRECTION): RANGE is a (BEGIN . END) pair, and DIRECTION is
the way from MARK to POINT: 1 forward, -1 backward.

A selection of at most one character has no direction: DIRECTION is
nil.  So every stored form of one character gives the same value.

On an empty line the bare cursor and the line selection cover the same
range: the newline.  An active region over that newline is the line
selection, so its value has a third element, the private flag, which
is t: (RANGE nil t).  Every other value leaves the flag out, so each
selection has one form.

Without an active region, or with an empty one, the selection is the
cursor character, the one after POINT:

  ((POINT . POINT+1) nil)

At the end of the buffer this selection is empty.

POINT and MARK are buffer positions. MARK may be nil when
REGION-ACTIVE? is nil."
  (cond ((and region-active? (< 1 (abs (- point mark))))
         (if (< point mark)
             (list (cons point mark) -1)
           (list (cons mark point) 1)))
        ((and region-active? (/= point mark))
         (let ((range (hel-range point mark)))
           (if (hel--empty-line-range-p range)
               (list range nil t)
             (list range nil))))
        (t
         (list (hel-char-range point) nil))))

(cl-defun hel--empty-line-range-p ((begin . end))
  "Return non-nil if BEGIN..END is exactly one empty line.
The range is one character, a newline at the start of a line."
  (and (= end (1+ begin))
       (eq (char-after begin) ?\n)
       (or (= begin (point-min))
           (eq (char-before begin) ?\n))))

(cl-defun hel-select (range &optional (direction 1) line?)
  "Select RANGE, with point at the end that DIRECTION gives.
DIRECTION is the way from the `mark' to `point': 1 forward, -1 backward.

A range of one character is a bare cursor, and has no direction.

LINE? is the private flag of `hel-selection'. It matters only on an empty line,
where the bare cursor and the line selection cover the same range, the newline:
there non-nil LINE? selects the line. Pass it through `apply' of `hel-select' to
a value of `hel-selection', or as t to select an empty line as a line.

RANGE is a (BEGIN . END) pair.

`hel-selection' returns the arguments in the same order, so `apply'
`hel-select' to its value restores the selection."
  (-let [(begin . end) range]
    (cond ((< 1 (hel-range-length range))
           (unless direction
             (error "No direction for a selection of %d characters"
                    (hel-range-length range)))
           (if (< direction 0)
               (progn (set-mark end)
                      (goto-char begin))
             (set-mark begin)
             (goto-char end)))
          (line?
           (set-mark end)
           (goto-char begin))
          (t
           (hel-set-cursor begin)))))

(cl-defun hel-selection-bounds (&optional (selection (hel-selection)))
  "Return the bounds of SELECTION: a (BEGIN . END) pair.
SELECTION has the form that `hel-selection' returns, and is the
selection by default."
  (-first-item selection))

(cl-defun hel-selection-direction (&optional (selection (hel-selection)))
  "Return the direction of SELECTION: 1 forward, -1 backward.
Return nil for a selection of at most one character, such as a bare
cursor.  SELECTION has the form that `hel-selection' returns, and is
the selection by default."
  (-second-item selection))

(cl-defun hel-line-selection-p (&optional (selection (hel-selection)))
  "Return non-nil if SELECTION consists of whole lines.
Both bounds of the selection are at the start of a line, because a line
holds its newline character:

   |first line
    second line
   |

On an empty line the bare cursor and the line selection cover the same
range: the newline.  Only the line selection is whole lines there,
because the user sees a selected line, and with the bare cursor only
the cursor.  The private flag of the selection tells the two apart.

SELECTION has the form that `hel-selection' returns, and is the
selection by default."
  (-let [((begin . end)) selection]
    (and (not (hel-bare-cursor-p selection))
         (save-excursion
           (and (progn (goto-char begin) (bolp))
                (progn (goto-char end) (bolp)))))))

(cl-defun hel-selection-head (&optional (selection (hel-selection)))
  "Return the position of the head character of SELECTION.
The head character is the character that the cursor covers: the last
character of a forward selection, and the first character otherwise,
which for a bare cursor is the character after point.  SELECTION is
\((BEGIN . END) DIRECTION), as `hel-selection' returns it."
  (-let [((begin . end) dir) selection]
    (if (and dir (< 0 dir))
        (1- end)
      begin)))

(defun hel-selection-anchor (selection dir)
  "Return the anchor of SELECTION: the edge that stays in place when a motion
in DIR extends the selection. DIR is the direction of the motion, 1 forward
or -1 backward. The anchor is the edge opposite to point. A selection of at
most one character, as at a bare cursor, has its anchor at the edge
opposite to DIR, so the character stays in the selection whichever way the
motion goes.
SELECTION is ((BEGIN . END) DIRECTION), as `hel-selection' returns it."
  (-let [((begin . end) side) selection]
    (cond ((<= (- end begin) 1) (if (< 0 dir) begin end))
          ((< 0 side) begin)
          (t end))))

(cl-defun hel-bare-cursor-p (&optional (selection (hel-selection)))
  "Return non-nil if SELECTION is a bare cursor, which shows only the cursor.
A bare cursor covers at most its cursor character.  On an empty line the
line selection covers the same range, the newline, but the user sees a
selected line, so it is not a bare cursor.  The private flag of the
selection tells the two apart.  SELECTION has the form that
`hel-selection' returns, and is the selection by default."
  (-let [(range _ line?) selection]
    (and (not line?)
         (<= (hel-range-length range) 1))))

(defun hel-set-cursor (pos)
  "Leave a bare cursor at POS: point there, and no active region.
The cursor character is the character after POS."
  (deactivate-mark)
  (goto-char pos))

;;; The region of Emacs
;;
;; Some code reads the region itself, below the level of the selection
;; value: it asks how the region lies on the visual lines.

(defun hel-visual-lines-p ()
  "Return t if active region spawns visual lines."
  (and visual-line-mode
       (use-region-p)
       (= (region-beginning) (hel-line-begin (region-beginning)))
       (= (region-end) (hel-line-begin (region-end)))))

;;; The start of a command
;;
;; The commands that act on structure, text objects and pairs look at the
;; selection without the whitespace at its edges, and at the end of the
;; buffer, where the selection is empty, at the character before point.

(defun hel-trimmed-selection-head ()
  "Return the head of the selection without the whitespace at its edges.
The commands that act on structure, text objects and pairs start from
this position, so whitespace at the head of a selection does not count.
At a bare cursor it is the cursor character.  At the end of the buffer,
where the selection is empty, it is the character before point.  Return
nil when that character is a newline, or when there is none."
  (-when-let (bounds (hel-trimmed-selection-bounds))
    (hel-selection-head (list bounds (hel-selection-direction)))))

(defun hel-trimmed-selection-bounds ()
  "Return the bounds of the selection without the whitespace at its edges.
A selection of whitespace alone keeps its bounds.  At the end of the
buffer the selection is empty, so return the bounds of the character
before point, or nil when that character is a newline."
  (-when-let (bounds (hel--start-bounds (hel-selection-bounds)))
    (or (hel-trim-whitespace-from-range bounds)
        bounds)))

(defun hel--start-bounds (bounds)
  "Return BOUNDS, or the bounds of the character before them when empty.
The selection is empty only at the end of the buffer, where no character
follows point. There a command that needs a character starts from the one
before point. Return nil when that character is a newline, or when there
is none."
  (-let [(begin . end) bounds]
    (cond ((< begin end) bounds)
          ((-some->> (char-before begin) (/= ?\n))
           (cons (1- begin) begin)))))

;;; Mark ring

(cl-defun hel-push-point (&optional (position (point)))
  "Push POSITION (point by default) on the `mark-ring'."
  ;; Don't store POSITION into mark ring if it equals to the last stored one.
  (unless (and mark-ring
               (= position (car mark-ring)))
    (let ((old (nth mark-ring-max mark-ring))
          (history-delete-duplicates nil))
      (add-to-history 'mark-ring (copy-marker position)
                      mark-ring-max t)
      (when old
        (set-marker old nil))))
  ;; Don't store POSITION into global mark ring if the last position there is
  ;; in this same buffer. Also don't do it in a minibuffer: it dies on exit.
  (unless (or (minibufferp)
              (and global-mark-ring
                   (eq (marker-buffer (car global-mark-ring))
                       (current-buffer))))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history 'global-mark-ring (copy-marker position)
                      global-mark-ring-max t)
      (when old
        (set-marker old nil))))
  nil)

(defun hel--jump-over-mark-ring (backward? extend)
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' forward (or BACKWARD)
and jump to the new top position.  EXTEND is as in `hel-move-to-pos'."
  (when mark-ring
    (when (= (point) (car mark-ring))
      (cl-callf hel-rotate-ring mark-ring backward?))
    (hel-recenter-point-on-jump
      (hel-move-to-pos nil (car mark-ring) extend))))

(defun hel--jump-over-global-mark-ring (&optional backward?)
  "Jump to the top location on the `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
forward (or BACKWARD) and jump to new top location.  The jump removes the
secondary cursors, because they have no place in the target buffer."
  ;; Delete entries that refer to non-existent buffers.
  (when (setq global-mark-ring (-filter #'marker-buffer global-mark-ring))
    (when (eq (marker-buffer (car global-mark-ring))
              (current-buffer))
      (cl-callf hel-rotate-ring global-mark-ring backward?))
    (hel-disable-multiple-cursors-mode)
    (hel-recenter-point-on-jump
      (let* ((marker (car global-mark-ring))
             (buffer (marker-buffer marker))
             (position (marker-position marker)))
        ;; `switch-to-buffer' goes first: it moves point back to where
        ;; the window last showed the buffer, when
        ;; `switch-to-buffer-preserve-window-point' is t, as by default.
        (switch-to-buffer buffer)
        (or (<= (point-min) position (point-max))
            (if widen-automatically
                (widen)
              (error "Global mark position is outside accessible part of buffer %s"
                     (buffer-name buffer))))
        (goto-char position)
        (deactivate-mark)))))

(defun hel-rotate-ring (ring &optional backward?)
  "Rotate the RING elements.
This function destructively modify RING and should be used the following way:
`(setq RING (hel-rotate-ring RING))'

RING should be a list like `mark-ring', not the ring structure from
`ring.el'."
  (if backward?
      (nconc (last ring) (nbutlast ring))
    (nconc (cdr ring) (list (car ring)))))

;;; Copy/paste

(defun hel--paste-line-p (selection side)
  "Return non-nil if text pasted at SIDE of SELECTION goes on its own line.
SIDE is 1 for the end of the selection and -1 for its beginning.  At the
end, the selection must end with a line end and start on an earlier
line.  At the beginning, it must start a line and hold the end of that
line.  A bare cursor pastes on its line, also on a newline.  SELECTION
has the form that `hel-selection' returns."
  (-let [((begin . end)) selection]
    (and (not (hel-bare-cursor-p selection))
         (save-excursion
           (if (< side 0)
               (progn
                 (goto-char begin)
                 (and (bolp)
                      (< (line-end-position) end)))
             (goto-char end)
             (and (bolp)
                  (< begin (line-beginning-position))))))))

(cl-defun hel-push-mark (&optional (position (point)) nomsg activate)
  "Set mark to the POSITION and push it on the `mark-ring'.
If NOMSG is nil show `Mark set' message in echo area."
  (hel-push-point position)
  (set-marker (mark-marker) position (current-buffer))
  (or nomsg executing-kbd-macro (< 0 (minibuffer-depth))
      (message "Mark set"))
  (when activate
    (set-mark (mark t)))
  nil)

(defun hel-paste (yank-function side)
  "Paste after the selection when SIDE is 1, and before it when -1.
The selection decides where the text goes, never the text itself, so the user
chooses at the destination. The text goes on lines of its own when the selection
has whole lines at SIDE, and into the line otherwise. At its end, the selection
must end at the start of a line and begin on an earlier line; at its beginning,
it must start a line and hold the end of that line. A bare cursor pastes on its
line: on a newline the text goes before the newline.

The line selection of one empty line is a gap between paragraphs. The text goes
into the gap, whatever SIDE is, and one empty line stays above it and one below.
So the text loses every newline at its ends, and a text of only newlines
changes nothing.

The pasted text becomes the selection, in the direction of the old
selection, or forward when the old one has none. YANK-FUNCTION inserts
the text, as `yank' does."
  (-let* (((selection &as (begin . end) dir) (hel-selection))
          (gap? (and (hel-line-selection-p selection)
                     (hel--empty-line-range-p (cons begin end))))
          (shape (cond (gap? 'gap)
                       ((hel--paste-line-p selection side) 'line)))
          (deactivate-mark nil))
    ;; The text goes in at a bare cursor, which `yank' needs: it sets
    ;; the mark at the start of the text itself.
    (hel-set-cursor (if (or gap?
                            (and (< 0 side)
                                 (not (and (hel-bare-cursor-p selection)
                                           (memq (char-after begin)
                                                 '(?\n nil))))))
                        end
                      begin))
    (let ((range (hel--paste-insert yank-function shape (or dir 1))))
      (when gap?
        (if (null range)
            (apply #'hel-select selection)
          ;; The empty line below the text.  Point stays before it.
          (save-excursion
            (goto-char (cdr range))
            (insert ?\n)))))))

(defun hel--paste-insert (yank-function shape dir)
  "Insert a text at point with YANK-FUNCTION, in SHAPE, and select it.
SHAPE says where the text goes, as `hel--paste-shape' takes it.  The new
selection has the direction DIR.  In a `prog-mode' buffer the text is
indented.  Return the bounds of the inserted text, or nil when the text
is empty."
  (let ((deactivate-mark nil))
    (cl-letf ((yank-transform-functions
               (cons (-partial #'hel--paste-shape shape)
                     yank-transform-functions))
              ;; Intercept `push-mark' so that any time `yank' calls it,
              ;; `hel-push-mark' is executed instead.
              ((symbol-function 'push-mark) #'hel-push-mark))
      (funcall yank-function))
    (let ((empty? (= (point) (mark t))))
      (hel-select (hel-range (point) (mark t)) dir shape)
      (hel-extend-selection -1)
      (when (derived-mode-p 'prog-mode)
        (-let [(begin . end) (hel-selection-bounds)]
          (indent-region begin end)))
      (unless empty?
        (hel-selection-bounds)))))

(defun hel--paste-shape (shape text)
  "Return TEXT in the SHAPE that a paste gives it.
SHAPE nil pastes into a line, and the text loses its last newline.
SHAPE `line' pastes on lines of their own, and the text ends with
a newline. SHAPE `gap' pastes between two empty lines, and the text
loses every newline at its ends and then ends with one newline; a text
of only newlines becomes empty."
  (if (string-empty-p text)
      text
    (pcase-exhaustive shape
      ('nil (string-trim-right text "\n"))
      ('line (if (string-suffix-p "\n" text)
                 text
               (concat text "\n")))
      ('gap (let ((text (string-trim text "\n+" "\n+")))
              (if (string-empty-p text)
                  text
                (concat text "\n")))))))

(defun hel--copy-append (string)
  "Append STRING to the end of the latest kill in the kill ring."
  (let* ((left (or (car kill-ring) ""))
         (right string)
         (separator (if (or (string-suffix-p "\n" left)
                            (string-prefix-p "\n" right))
                        "\n"
                      " "))
         (replace? (or (= (length left) 0)
                       (null (get-text-property 0 'yank-handler left)))))
    (kill-new (concat (string-trim-right left)
                      separator
                      (string-trim-right right))
              replace?)))

;;; Changes

(defun hel-indent (indent-function count)
  "Indent the lines of the selection COUNT times with INDENT-FUNCTION.
A bare cursor indents its line. INDENT-FUNCTION takes the beginning and
the end of the text to indent, as `indent-rigidly-left' does."
  (cl-flet ((indent-selection ()
              (dotimes (_ count)
                (-let [(begin . end) (hel-selection-bounds)]
                  (funcall indent-function begin end)))))
    (let ((selection (hel-selection)))
      (cond ((hel-line-selection-p selection)
             (let ((deactivate-mark nil))
               (indent-selection))
             (hel-extend-selection -1))
            ((hel-bare-cursor-p selection)
             ;; The beginning of the line stays in place while its
             ;; indentation changes, so each step finds the line from it.
             (let ((line-begin (-> (hel-selection-bounds selection)
                                   (car)
                                   (hel-visible-line-bounds)
                                   (car))))
               (hel-save-selection
                 (dotimes (_ count)
                   (-let [(begin . end) (hel-visible-line-bounds line-begin)]
                     (funcall indent-function begin end))))))
            (t
             (hel-save-selection
               (hel-expand-selection-to-full-lines)
               (indent-selection))
             (hel-extend-selection -1))))))

;;; Fold opening

(cl-defun hel-range-visible-p ((begin . end))
  "Test whether the text in the range BEGIN..END can be shown to the user.

Return values:

  nil     The range is truly-invisible — i.e. it is fully hidden by
          an `invisible' text property or by a non-openable overlay.

  list    The range is fully or partially folded — i.e. is hidden by
          openable overlay — the one with `isearch-open-invisible'
          property. Return a list of these overlays.

  t       The range is, at least partially, visible with no folds to open.
          (E.g. Org link with hidden target.)"
  (when (/= begin end)
    (save-excursion
      ;; The range is "showable" if any of its positions is visible, or
      ;; or hidden by openable overlay.
      (let ((visible? nil)
            (openable-overlays nil))
        ;; Walk the whole range, stepping at every text-property or
        ;; overlay boundary.
        (goto-char begin)
        (while (< (point) end)
          (cond ((not (invisible-p (point)))
                 (setq visible? t))
                ;; Hidden by an `invisible' text property — can't be opened.
                ((invisible-p (get-text-property (point) 'invisible)))
                ;; Else hidden by overlay.
                (t
                 (let ((can-be-opened? t)
                       (overlays nil))
                   (dolist (ov (overlays-at (point)))
                     (when (invisible-p (overlay-get ov 'invisible))
                       (if (overlay-get ov 'isearch-open-invisible)
                           (push ov overlays)
                         ;; We found one overlay that cannot be opened, that
                         ;; means the whole chunk cannot be opened.
                         (setq can-be-opened? nil))))
                   (when can-be-opened?
                     (cl-callf append openable-overlays overlays)))))
          (goto-char (next-char-property-change (point) end)))
        (if openable-overlays
            (delete-dups openable-overlays)
          visible?)))))

(defun hel-open-overlay (ov)
  "Permanently open folded OVERLAY.
See `isearch-open-necessary-overlays'."
  (when (invisible-p (overlay-get ov 'invisible))
    (if-let* ((fun (overlay-get ov 'isearch-open-invisible)))
        (funcall fun ov)
      (overlay-put ov 'invisible nil))))

(defun hel-temporary-open-overlay (ov)
  "See `isearch-open-overlay-temporary'."
  ;; Modes can provide custom function to open overlays termporary.
  (if-let* ((fun (overlay-get ov 'isearch-open-invisible-temporary)))
      (funcall fun ov nil)
    ;; Else set `invisible' property to nil, and store the original value to
    ;; `isearch-invisible' property.
    (overlay-put ov 'isearch-invisible (overlay-get ov 'invisible))
    (overlay-put ov 'invisible nil)))

(defun hel-close-temporary-opened-overlay (ov)
  "See `isearch-open-overlay-temporary'."
  ;; If this exists it means that the overlay was opened using this function,
  ;; not by tweaking the overlay properties.
  (if-let* ((fun (overlay-get ov 'isearch-open-invisible-temporary)))
      (funcall fun ov t)
    ;; Else restore the original value of `invisible' property.
    (overlay-put ov 'invisible (overlay-get ov 'isearch-invisible))
    (overlay-put ov 'isearch-invisible nil)))

(defun hel-reveal-position (pos)
  "Permanently open fold at POS."
  (-each (overlays-at pos) #'hel-open-overlay))

;;; Tree-sitter

(declare-function treesit-available-p "treesit.c" ())
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-node-start "treesit.c" (node))
(declare-function treesit-node-end "treesit.c" (node))

(defun hel-treesit-active-p ()
  "Return non-nil if the current buffer has a tree-sitter parser."
  (and (treesit-available-p)
       (treesit-parser-list)
       t))

(defun hel-treesit-node-bounds (node)
  "Return the (BEGIN . END) bounds of the tree-sitter NODE."
  (cons (treesit-node-start node)
        (treesit-node-end node)))

(defun hel-treesit-node-length (node)
  "Return the length of the tree-sitter NODE in characters."
  (hel-range-length (hel-treesit-node-bounds node)))

(defun hel-treesit-nodes-hull (nodes)
  "Return the hull of the tree-sitter NODES: the smallest range
that holds every one of them. The order of NODES does not matter.
The result is a (BEGIN . END) pair."
  (cons (-min (-map #'treesit-node-start nodes))
        (-max (-map #'treesit-node-end nodes))))

(define-hash-table-test 'hel-treesit-node
  #'treesit-node-eq
  (lambda (node)
    (sxhash-equal (hel-treesit-node-bounds node))))

;;; Advices

(declare-function hel-extend-selection "hel-commands" (arg) t)
(declare-function hel-insert-state "hel-core" (&optional arg) t)

(defun hel-keep-selection-a (command &rest args)
  "Keep region active, disable extending selection (`v' key)."
  (prog1 (let ((deactivate-mark nil))
           (apply command args))
    (hel-extend-selection -1)))

(defun hel-deactivate-mark-a (&rest _)
  "Deactivate mark. This function can be used as advice."
  (deactivate-mark))

(defun hel-maybe-deactivate-mark-a (&rest _)
  "Deactivate mark unless extending selection is active. Can be used as advice."
  (or hel-extend-selection
      (deactivate-mark)))

(declare-function hel-disable-multiple-cursors-mode "hel-multiple-cursors-core")

(defun hel-jump-command-a (command &rest args)
  "Aroung advice for COMMAND that moves point."
  (hel-disable-multiple-cursors-mode)
  (deactivate-mark)
  (hel-recenter-point-on-jump
    (prog1 (apply command args)
      ;; We can land in another buffer, so deactivate mark there as well.
      (deactivate-mark)
      (-each (overlays-at (point)) #'hel-open-overlay))))

(defun hel-switch-to-insert-state-a (&rest _)
  "Switch Hel into Insert state.
Can be used as advice."
  (hel-insert-state 1))

(defun hel--execute-for-all-cursors-a (&rest _)
  "Execute selected command for all cursors."
  (setq hel-this-command this-command))

;;; .
(provide 'hel-lib)
;;; hel-lib.el ends here
