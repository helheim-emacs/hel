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
(require 'thingatpt)
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
       (unless hel-executing-command-for-fake-cursor
         (redisplay)))))

(defmacro hel-save-selection (&rest body)
  "Evaluate BODY with preserving original selection.
The difference from `save-mark-and-excursion' is that both point and mark are
saved as markers and correctly handle case when text was inserted before
the selection."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (pnt begin end dir)
    `(if (use-region-p)
         (let ((deactivate-mark nil)
               (,begin (copy-marker (region-beginning) t))
               (,end (copy-marker (region-end)))
               (,dir (hel-region-direction)))
           (unwind-protect
               (save-excursion ,@body)
             (hel-select (cons ,begin ,end) ,dir)
             (set-marker ,begin nil)
             (set-marker ,end nil)))
       ;; else
       (let ((,pnt (copy-marker (point) t)))
         (unwind-protect
             (save-excursion ,@body)
           (if mark-active (deactivate-mark))
           (goto-char ,pnt)
           (set-marker ,pnt nil))))))

(defmacro hel-restore-selection-on-error (&rest body)
  "Restore initial selection if error occured during BODY evaluation."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (selection point something-goes-wrong?)
    `(let ((,selection (hel-selection))
           (,point (point))
           (,something-goes-wrong? t))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,something-goes-wrong? nil))
         (when ,something-goes-wrong?
           (if ,selection
               (apply #'hel-select ,selection)
             (goto-char ,point)))))))

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

(defun hel-region-direction ()
  "Return the direction of region: -1 if point precedes mark, 1 otherwise."
  (if (< (point) (mark-marker)) -1 1))

(defun hel-linewise-selection-p (&optional direction)
  "Return t if active region spawns full logical lines.
DIRECTION: 1 or -1. If provided — check if region spawns at least one full
logical line on desired end of the region."
  (and (use-region-p)
       (save-excursion
         (let ((begin (region-beginning))
               (end (region-end)))
           (cond ((null direction)
                  (and (progn (goto-char begin) (bolp))
                       (progn (goto-char end) (bolp))))
                 ((<= direction 0)
                  (goto-char begin)
                  (and (bolp)
                       ;; at least one full line is selected:
                       ;; point is at BEGIN, END should be on a later line
                       (< (line-end-position) end)))
                 (t
                  (goto-char end)
                  (and (bolp)
                       ;; at least one full line is selected:
                       ;; point is at END, BEGIN should be on an earlier line
                       (< begin (line-beginning-position)))))))))

(defun hel-visual-lines-p ()
  "Return t if active region spawns visual lines."
  (and visual-line-mode
       (use-region-p)
       (= (region-beginning) (hel-line-begin (region-beginning)))
       (= (region-end) (hel-line-begin (region-end)))))

(defconst hel-whitespace-chars "[:space:]\r\n"
  "The `skip-chars-forward' set of the blanks and the line ends.
A blank is a character with whitespace syntax other than a line end. The
syntax of a line end depends on the mode, so the set names them.")

(defun hel-whitespace-p (char)
  "Non-nil when CHAR is a blank: it has whitespace syntax and ends no line."
  (and (eql (char-syntax char) ?\s)
       (not (memq char '(?\r ?\n)))))

(cl-defun hel-trim-whitespace-from-range ((begin . end))
  "Return the range BEGIN..END without the whitespace at its edges.
Whitespace is spaces, tabs and line ends. Return nil if nothing remains."
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

(defun hel-cursor-is-bar-p ()
  "Return non-nil if `cursor-type' is bar."
  (let ((cursor-type (if (eq cursor-type t)
                         (frame-parameter nil 'cursor-type)
                       cursor-type)))
    (or (eq cursor-type 'bar)
        (and (consp cursor-type)
             (eq (car cursor-type) 'bar)))))

(defun hel-range (a b)
  "Return the range between positions A and B, which can come in any order."
  (cons (min a b) (max a b)))

(cl-defun hel-select ((begin . end) &optional (direction 1))
  "Select the range BEGIN..END.
The selection is forward, i.e. from BEGIN to END, when DIRECTION is 1, and
backward when DIRECTION is -1."
  (if (< direction 0)
      (progn (set-mark end)
             (goto-char begin))
    ;; else
    (set-mark begin)
    (goto-char end)))

(defun hel-select-motion (start end)
  "Select from START to END, with point at END.
When extending selection is active, keep the mark and only move point."
  (set-mark (if hel--extend-selection (mark) start))
  (goto-char end))

(defun hel-selection ()
  "Return (RANGE DIRECTION) of the selection, or nil without a selection.
RANGE is (BEGIN . END). `hel-select' takes the returned list as its arguments:

  (let ((selection (hel-selection)))
    ...
    (apply #\='hel-select selection))"
  (if (use-region-p)
      (list (cons (region-beginning) (region-end))
            (hel-region-direction))))

(defun hel-maybe-set-mark ()
  "Set mark at point unless extending selection is active."
  (or hel--extend-selection
      (set-mark (point))))

(defun hel-maybe-deactivate-mark ()
  "Deactivate mark unless extending selection is active."
  (or hel--extend-selection
      (deactivate-mark)))

(defun hel-ensure-region-direction (direction)
  "Exchange point and mark if the region direction mismatches DIRECTION.
DIRECTION should be 1 or -1."
  (when (/= direction (hel-region-direction))
    (hel--exchange-point-and-mark)))

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

(defun hel-comment-at-pos-p (pos)
  "Return non-nil if position POS is inside a comment, or comment starts
right after the point."
  (ignore-errors
    ;; We cannot be in a comment if we are inside a string
    (unless (nth 3 (syntax-ppss pos))
      (or (nth 4 (syntax-ppss pos))
          ;; This test opening and closing comment delimiters... We need
          ;; to check that it is not newline, which is in "comment ender"
          ;; class in elisp-mode, but we just want it to be treated as
          ;; whitespace.
          (and (< pos (point-max))
               (memq (char-syntax (char-after pos)) '(?< ?>))
               (not (eq (char-after pos) ?\n)))
          ;; We also need to test the special syntax flag for comment
          ;; starters and enders, because `syntax-ppss' does not yet know if
          ;; we are inside a comment or not (e.g. / can be a division or
          ;; comment starter...).
          (when-let* ((s (car (syntax-after pos))))
            (or
             ;; First char of 2 chars comment opener
             (and (/= 0 (logand (ash 1 16) s))
                  (nth 4 (syntax-ppss (+ pos 2))))
             ;; Second char of 2 chars comment opener
             (and (/= 0 (logand (ash 1 17) s))
                  (nth 4 (syntax-ppss (+ pos 1))))
             ;; First char of 2 chars comment closer
             (and (/= 0 (logand (ash 1 18) s))
                  (nth 4 (syntax-ppss (- pos 1))))
             ;; Second char of 2 chars comment closer
             (and (/= 0 (logand (ash 1 19) s))
                  (nth 4 (syntax-ppss (- pos 2))))))))))

(defun hel-string-at-pos-p (position)
  "Return non-nil if POSITION is inside string.
This function actually returns the 3rd element of `syntax-ppss' which
can be a number if the string is delimited by that character or t if
the string is delimited by general string fences."
  (ignore-errors
    (save-excursion
      (nth 3 (syntax-ppss position)))))

(defun hel-overlay-live-p (overlay)
  "Return non-nil if OVERLAY is not deleted from buffer."
  (-some-> overlay
    (overlay-buffer)
    (buffer-live-p)))

(defun hel-pulse-main-selection (&optional face)
  (pulse-momentary-highlight-region (region-beginning) (region-end) face))

(defun hel-reveal-point-when-on-top (&rest _)
  "Reveal point when it's only partially visible.
For some reason, Emacs can become slow while point is partially visible, so this
function prevents that. It is intended to be used as `:after' advice."
  (unless hel-executing-command-for-fake-cursor
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
  "Move point toward the DIRECTION stopping after a char is not in CHARS string.
Move backward when DIRECTION is negative number, forward — otherwise.
Returns the distance traveled, positive or negative depending on DIRECTION."
  (if (< 0 direction)
      (skip-chars-forward chars)
    (- (skip-chars-backward chars))))

(cl-defun hel-skip-whitespaces (&optional (direction 1))
  "Move point toward the DIRECTION across whitespace.
Move backward when DIRECTION is negative number, forward — otherwise.
Return the distance traveled positive or negative depending on DIRECTION."
  (if (natnump direction)
      (skip-syntax-forward " " (line-end-position))
    (skip-syntax-backward " " (line-beginning-position))))

(cl-defun hel-next-char (&optional (direction 1))
  "Return the next after point char toward the DIRECTION.
If DIRECTION is positive number — get following char, otherwise preceding char."
  (if (natnump direction)
      (following-char)
    (preceding-char)))

(cl-defun hel-line-begin (&optional (pos (point)))
  "Return the start of the line at POS.
This is the visual line of the selected window when `visual-line-mode' is
active, and otherwise the visible logical line."
  (save-excursion
    (goto-char pos)
    (if visual-line-mode
        (beginning-of-visual-line)
      (hel--beginning-of-visible-line))
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

;;;; `hel--forward-word'

(cl-defun hel--forward-word (&optional (count 1))
  "Move point COUNT words forward (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence of characters that are neither word characters nor whitespace"
  (hel-motion-loop (dir count)
    (hel-skip-chars "\r\n" dir)
    (hel-skip-whitespaces dir)
    (or (hel-line-boundary-p dir)
        (/= 0 (hel-skip-chars "^[:word:][:space:]\r\n" dir))
        (let ((word-separating-categories hel-cjk-word-separating-categories)
              (word-combining-categories  hel-cjk-word-combining-categories))
          (forward-word dir)))))

;;;; `hel--forward-WORD'

(defun hel--forward-WORD (&optional count)
  "Move point COUNT WORDs forward (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is any space separated sequence of characters."
  (hel-motion-loop (dir (or count 1))
    (hel-skip-chars "\r\n" dir)
    (hel-skip-whitespaces dir)
    (unless (hel-line-boundary-p dir)
      (hel-skip-chars "^[:space:]\r\n" dir))))

;;;; `hel--forward-sentence'

(defun hel--forward-sentence (&optional count)
  "Move point COUNT sentences forward (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT.

What is sentence is defined by `forward-sentence-function'."
  (hel-motion-loop (dir (or count 1))
    (ignore-errors (forward-sentence dir))))

;;;; `hel--forward-paragraph'

(defun hel--forward-paragraph (&optional count)
  "Move point COUNT paragraphs forward (backward if COUNT is negative).
Returns then count of paragraphs left to move, positive of negative depending
on sign of COUNT."
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (hel-motion-loop (dir (or count 1))
      (cond ((natnump dir) (forward-paragraph))
            ((not (bobp))
             (start-of-paragraph-text)
             (beginning-of-line))))))

;;;; `hel--forward-function'

(defun hel--forward-function (&optional count)
  "Move point COUNT functions forward (backward if COUNT is negative).
Returns then count of functions left to move, positive of negative depending
on sign of COUNT."
  (hel-motion-loop (dir (or count 1))
    (if (< dir 0) (beginning-of-defun) (end-of-defun))))

;;; Things
;;;; `hel-line'

;; The difference from built-in `line' thing is that `hel-line' ignores
;; invisible parts of the buffer (lines folded by `outline-minor-mode' for
;; example) and always denotes visible lines.
;;
;; The need for this thing arose from the requirement to select a folded section
;; of the buffer (in Org-mode or Outline-mode) using the `x' key command.

(put 'hel-line 'forward-op #'hel--forward-line)
(put 'hel-line 'bounds-of-thing-at-point
     (lambda ()
       (cons (save-excursion
               (hel--beginning-of-visible-line)
               ;; `hel--beginning-of-visible-line' may leave point after invisible
               ;; characters if line starts with such of these (e.g., with
               ;; a link at column 0 in Org mode). Really move to the beginning
               ;; of the current visible line.
               (beginning-of-line)
               (point))
             (save-excursion
               (hel--forward-line 1)
               (point)))))

;; Adopted from `move-end-of-line'.
(defun hel--forward-line (&optional count)
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

(defun hel--beginning-of-visible-line ()
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

;;;; `hel-visual-line'

(put 'hel-visual-line 'forward-op #'vertical-motion)
;; (put 'hel-visual-line 'beginning-op #'beginning-of-visual-line)
;; (put 'hel-visual-line 'end-op       #'end-of-visual-line)

;;;; `hel-sexp'

(defun hel--forward-sexp (&optional count)
  "Move point COUNT sexps forward (backward if COUNT is negative).
Returns the count of sexps left to move, positive or negative depending
on sign of COUNT."
  ;; Make binding only during this motion instead of global one, because some
  ;; major-modes rely on the jump to the buffer end.
  (let ((forward-sexp-function
         (if (memq forward-sexp-function '(forward-sexp-default-function nil))
             #'hel-forward-sexp-only
           ;; Do not rebind in modes that use their own version of
           ;; `forward-sexp-function' (`c-mode', `python-mode').
           forward-sexp-function)))
    (hel-motion-loop (dir (or count 1))
      (ignore-errors
        (forward-sexp dir)))))

(put 'hel-sexp 'forward-op #'hel--forward-sexp)

(defun hel-forward-sexp-only (&optional count)
  "Move forward across COUNT balanced expressions (sexps).
Hel binds `forward-sexp-function' to this function during its own sexp
motions.  Unlike the default `forward-sexp' behavior, it does not move to
the end of the buffer when there is no sexp ahead.

Signal `scan-error' when there is no sexp to move over. The error
arguments follow the format used by `forward-sexp'."
  (or count (setq count 1))
  (if-let* ((pos (scan-sexps (point) count)))
      (progn
        (goto-char pos)
        (if (< count 0) (backward-prefix-chars)))
    (signal 'scan-error (if (< count 0)
                            (list "No previous sexp" (point-min) (point))
                          (list "No next sexp" (point) (point-max))))))

;;;; `hel-comment'

(put 'hel-comment 'bounds-of-thing-at-point #'hel-bounds-of-comment-at-point-ppss)

(defun hel-bounds-of-comment-at-point-ppss ()
  "Return the bounds of a comment at point using Parse-Partial-Sexp Scanner."
  (save-excursion
    (let ((state (syntax-ppss)))
      (when (nth 4 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))

;;; Selection

(defun hel-expand-selection-to-full-lines (&optional direction)
  "Extend the selection so that it consists of complete lines.
When region is active: expand selection to line boundaries to encompass full
line(s). With no region, select current line."
  (unless (hel-linewise-selection-p)
    (if (use-region-p)
        (let ((begin (region-beginning))
              (end (region-end))
              (dir (or direction (hel-region-direction))))
          (hel-select (cons (progn (goto-char begin)
                                   (car (bounds-of-thing-at-point 'hel-line)))
                            (progn (goto-char end)
                                   (cdr (bounds-of-thing-at-point 'hel-line))))
                      dir))
      ;; else no region
      (hel-select (bounds-of-thing-at-point 'hel-line) (or direction 1)))
    (hel--fix-newline-at-end-of-buffer)
    t))

;;; Surround

(defun hel-surround--insert (char)
  "For given CHAR according to `hel-surround-alist' `:insert' key return
cons cell (LEFT . RIGHT) with strings to insert."
  (pcase (-some-> hel-surround-alist
           (map-elt char)
           (map-elt :insert))
    ((and (pred functionp) fn)
     (funcall fn))
    ((and (pred -cons-pair-p) pair)
     pair)
    ('nil
     (cons (char-to-string char)
           (char-to-string char)))))

(cl-defun hel-surround-read-tag (&optional (prompt "tag: "))
  "Read an HTML tag with PROMPT and return the cons cell (OPENING . CLOSING).
The input is a tag name with optional attributes, such as `div class=\"box\"'.
If the selection is linewise, both tags end with a newline."
  (let* ((input (string-trim (read-string prompt)))
         (name (car (split-string input))))
    (unless name
      (user-error "Empty tag name"))
    (let ((newline (if (hel-linewise-selection-p) "\n" "")))
      (cons (concat "<" input ">" newline)
            (concat "</" name ">" newline)))))

(defun hel-surround-4-bounds-of-html-tag-at-point (&optional name)
  "Return 4 bounds of the HTML tag NAME around point, or nil.
If NAME is nil or empty string, match the nearest tag pair.
The result is the list (LEFT-BEGIN LEFT-END RIGHT-BEGIN RIGHT-END).
Nested tag pairs are skipped. Self-closing tags like <br/> do not count
as opening tags."
  (let ((open (if (member name '(nil ""))
                  "<[[:alpha:]]\\(?:[^<>]*[^/<>]\\)?>"
                (concat "<" (regexp-quote name)
                        "\\(?:[[:space:]]\\(?:[^<>]*[^/<>]\\)?\\)?>")))
        (close (if (member name '(nil ""))
                   "</[^<>]+>"
                 (concat "</" (regexp-quote name) "[[:space:]]*>"))))
    (save-excursion
      ;; Point inside a tag belongs to that tag: the search starts before an
      ;; opening tag or after a closing one.
      (when-let* ((begin (save-excursion
                           (skip-chars-backward "^<>")
                           (if (eq (char-before) ?<)
                               (1- (point)))))
                  (end (save-excursion
                         (skip-chars-forward "^<>")
                         (if (eq (char-after) ?>)
                             (1+ (point))))))
        (goto-char (if (eq ?/ (char-after (1+ begin)))
                       end
                     begin)))
      ;; The search covers the whole buffer since modes like `html-mode' treat
      ;; each tag as a defun, so defun bounds cannot limit it.
      (hel-surround-4-bounds-at-point open close nil t t))))

(defun hel-surround--remove (char)
  "For given CHAR according to `hel-surround-alist' `:remove' key return
the list with 4 positions:
  - before left delimiter
  - after left delimiter
  - before right delimiter
  - after right delimiter
or nil if nothing found."
  (let ((spec (alist-get char hel-surround-alist)))
    (if-let* ((spec)
              (fun (plist-get spec :remove))
              ((functionp fun)))
        (funcall fun)
      ;; else
      (-let ((limits (bounds-of-thing-at-point 'defun))
             ((&plist :remove (left . right) :regexp :balanced)
              (if (plist-member spec :remove)
                  spec
                `(:remove ,(cons (char-to-string char)
                                 (char-to-string char))))))
        (hel-surround-4-bounds-at-point left right limits regexp balanced)))))

(declare-function org-at-table-p  "org-table")
(declare-function org-table-begin "org-table")
(declare-function org-table-end   "org-table")

(defun hel-bounds-of-quoted-at-point (quote-mark)
  "Return a cons cell (BEGIN . END) with bounds of text region
enclosed in QUOTE-MARKs."
  (if-let* ((limits (or (bounds-of-thing-at-point 'hel-comment)
                        (bounds-of-thing-at-point 'string)
                        (if (and (require 'org-table nil t)
                                 (org-at-table-p))
                            (cons (org-table-begin) (org-table-end))))))
      (-if-let ((begin _ _ end) (hel-surround-4-bounds-at-point
                                 (char-to-string quote-mark)
                                 (char-to-string quote-mark)
                                 limits))
          (cons begin end))
    ;; else
    (hel--bounds-of-quoted-at-point-ppss quote-mark)))

(defun hel-surround-4-bounds-at-point
    (left right &optional limits regexp? balanced?)
  "Return 4 bounds of the text region enclosed in LEFT and RIGHT strings or nil.

If LEFT and RIGHT are different, then point can be either: directly before
LEFT,directly after RIGHT, or somewhere between them. If LEFT and RIGHT are
equal — point should be between them.

The search can be bounded within the LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), otherwise they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs will be skipped.

Return the list (LEFT-BEGIN LEFT-END RIGHT-LEFT RIGHT-END) with
4 positions: before/after LEFT and before/after RIGHT, or nil."
  (save-excursion
    (when (string-equal left right)
      (setq balanced? nil))
    ;; Check if we can use Parse-Partial-Sexp Scanner
    (if (and balanced?
             (length= left 1)
             (length= right 1)
             (eq ?\( (char-syntax (string-to-char left)) )
             (eq ?\) (char-syntax (string-to-char right)) ))
        (-if-let ((begin . end) (hel-bounds-of-brackets-at-point
                                 (string-to-char left) (string-to-char right)))
            (list begin (1+ begin) (1- end) end))
      ;; else
      (hel-surround--4-bounds-at-point-1 left right limits regexp? balanced?))))

(defun hel-surround--4-bounds-at-point-1
    (left right &optional limits regexp? balanced?)
  "The internal function for `hel-surround-4-bounds-at-point' when
Parse-Partial-Sexp Scanner can't be used."
  (save-excursion
    (let ((left-not-equal-right? (not (string-equal left right))))
      (cond
       ;; point is before LEFT
       ((and left-not-equal-right?
             (hel-looking-at left 1 regexp?))
        (let* ((left-begin (point))
               (left-end (if regexp? (match-end 0)
                           (+ left-begin (length left)))))
          (goto-char left-end)
          (if-let* ((right-end (hel-surround-search-outward
                                left right 1 limits regexp? balanced?))
                    (right-begin (if regexp? (match-beginning 0)
                                   (- right-end (length right)))))
              (list left-begin left-end right-begin right-end))))
       ;; point is after RIGHT
       ((and left-not-equal-right?
             (hel-looking-at right -1 regexp?))
        (let* ((right-end (point))
               (right-begin (if regexp? (match-beginning 0)
                              (- right-end (length right)))))
          (goto-char right-begin)
          (if-let* ((left-begin (hel-surround-search-outward
                                 left right -1 limits regexp? balanced?))
                    (left-end (if regexp? (match-end 0)
                                (+ left-begin (length left)))))
              (list left-begin left-end right-begin right-end))))
       (t
        (if-let* ((left-begin (hel-surround-search-outward
                               left right -1 limits regexp? balanced?))
                  (left-end (if regexp? (match-end 0)
                              (+ left-begin (length left))))
                  (right-end (hel-surround-search-outward
                              left right 1 limits regexp? balanced?))
                  (right-begin (if regexp? (match-beginning 0)
                                 (- right-end (length right)))))
            (list left-begin left-end right-begin right-end)))))))

(defun hel-bounds-of-brackets-at-point (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `hel-surround-4-bounds-at-point'.

Return the cons cell (BEGIN . END) with positions before LEFT and
after RIGHT."
  (when (eq left right)
    (user-error "Left and right brackets should not be equal"))
  (if-let* ((string-or-comment-bounds
             (or (bounds-of-thing-at-point 'hel-comment)
                 (bounds-of-thing-at-point 'string)))
            (bounds (hel-surround--4-bounds-at-point-1
                     (char-to-string left) (char-to-string right)
                     string-or-comment-bounds
                     nil t)))
      ;; If inside comment or string use manual algorithm.
      (-let [(begin _ _ end) bounds]
        (cons begin end))
    ;; Else if not or nothing have found — go out ...
    (when string-or-comment-bounds
      (goto-char (car string-or-comment-bounds)))
    ;; ... and try Parse-Partial-Sexp Scanner
    (save-excursion
      (let* ((pnt (point))
             (syntax-table (if (and (eq (char-syntax left) ?\()
                                    (eq (char-syntax right) ?\)))
                               (syntax-table)
                             (let ((table (copy-syntax-table (syntax-table))))
                               (modify-syntax-entry left  (format "(%c" right) table)
                               (modify-syntax-entry right (format ")%c" left) table)
                               table))))
        (with-syntax-table syntax-table
          (cond ((eq (following-char) left) ; point is before LEFT
                 (if-let* ((end (scan-lists pnt 1 0)))
                     (cons pnt end)))
                ((eq (preceding-char) right) ; point is after RIGHT
                 (if-let* ((begin (scan-lists pnt -1 0)))
                     (cons begin pnt)))
                (t
                 (ignore-errors
                   (while (progn (up-list -1 t)
                                 (/= (following-char) left)))
                   (if-let* ((end (scan-lists (point) 1 0)))
                       (cons (point) end))))))))))

(defun hel-4-bounds-of-brackets-at-point (left right)
  "Return 4 bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `hel-surround-4-bounds-at-point'.

Return the list (LEFT-BEGIN LEFT-END RIGHT-LEFT RIGHT-END) with 4 positions:
1. Before LEFT bracket;
2. After LEFT bracket all following whitespaces and newlines;
3. Before RIGHT bracket all preceding whitespaces and newlines;
4. After RIGHT bracket.
When the brackets hold only whitespace, positions 2 and 3 are both the
position after LEFT bracket."
  (-when-let*
      (((l-begin . r-end) (hel-bounds-of-brackets-at-point left right))
       ((l-end . r-begin) (or (hel-trim-whitespace-from-range (cons (1+ l-begin) (1- r-end)))
                              (cons (1+ l-begin) (1+ l-begin)))))
    (list l-begin l-end r-begin r-end)))

(defun hel-surround-search-outward
    (left right &optional direction limits regexp? balanced?)
  "Return the position before LEFT or after RIGHT depending on DIRECTION.

This function assumes, that point is somewhere between LEFT RIGHT
delimiters, which should be strings.

DIRECTION should be either 1 — return the position after RIGHT,
or -1 — before LEFT.

The search is optionally bounded within LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), else they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs on the way will
be skipped."
  (or direction (setq direction 1))
  (save-excursion
    (if balanced?
        (hel-surround--search-outward-balanced left right direction limits regexp?)
      (let ((string (if (< direction 0) left right))
            (limit  (if (< direction 0) (car limits) (cdr limits))))
        (hel-surround--search string direction limit regexp?)))))

(defun hel-surround--search-outward-balanced
    (left right &optional direction limits regexp?)
  "This is an internal function for `hel-surround-search-outward'
that is used when BALANCED? argument is non-nil."
  (save-excursion
    (let (open close limit)
      (if (> direction 0)
          (setq open left
                close right
                limit (cdr limits))
        (setq open right
              close left
              limit (car limits)))
      ;; The algorithm assume we are *inside* a pair: level of nesting is 1.
      (let ((level 1))
        (cl-block nil
          (while (> level 0)
            (let* ((pnt (point))
                   (open-pos (hel-surround--search open direction limit regexp?))
                   (close-pos (progn
                                (goto-char pnt)
                                (hel-surround--search close direction limit regexp?))))
              (cond ((and close-pos open-pos)
                     (let ((close-dist (hel-distance pnt close-pos))
                           (open-dist  (hel-distance pnt open-pos)))
                       (cond ((< open-dist close-dist)
                              (cl-incf level)
                              (goto-char open-pos))
                             (t
                              (cl-decf level)
                              (goto-char close-pos)))))
                    (close-pos
                     (cl-decf level)
                     (goto-char close-pos))
                    (t (cl-return))))))
        (if (eql level 0)
            (point))))))

(defun hel--bounds-of-quoted-at-point-ppss (quote-mark)
  "Return a cons cell (BEGIN . END) with bounds of region around
the point enclosed in QUOTE-MARK character.

Internally uses Emacs' built-in Parse-Partial-Sexp Scanner for
balanced expressions."
  (save-excursion
    (let ((syntax-table (if (eq (char-syntax quote-mark) ?\")
                            (syntax-table)
                          (let ((table (copy-syntax-table (syntax-table))))
                            (modify-syntax-entry quote-mark "\"" table)
                            table))))
      (with-syntax-table syntax-table
        (let* ((curpoint (point))
               (state (progn
                        (beginning-of-defun)
                        (parse-partial-sexp (point) curpoint nil nil (syntax-ppss)))))
          (if (nth 3 state)
              ;; Inside the string
              (ignore-errors
                (goto-char (nth 8 state))
                (cons (point)
                      (progn (forward-sexp) (point))))
            ;; At the beginning of the string
            (if-let* ((ca (char-after))
                      ;; ((eq (char-syntax ca) ?\"))
                      ((eq ca quote-mark))
                      (bounds (bounds-of-thing-at-point 'sexp))
                      ((<= (car bounds) (point)))
                      ((< (point) (cdr bounds))))
                bounds)))))))

(cl-defun hel-surround--search (string &optional (direction 1) bound regexp? visible?)
  "Search for STRING toward the DIRECTION.

DIRECTION: 1 — search forward, -1 — search backward.

BOUND is a buffer position that bounds the search toward the DIRECTION.
The match found must not end after that position.

If REGEXP? is non-nil STRING will considered a regexp pattern,
otherwise — literally.

If VISIBLE? is non-nil skip invisible matches.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (let ((search-fun (if regexp? #'re-search-forward #'search-forward))
        (found nil))
    (while (and (not found)
                (funcall search-fun string bound t direction))
      (if (or (not visible?)
              (hel-range-visible? (cons (match-beginning 0) (match-end 0))))
          (setq found t)))
    (if found (point))))

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
  ;; Don't store POSITION into global mark ring if the last position there
  ;; is in this same buffer.
  (unless (and global-mark-ring
               (eq (marker-buffer (car global-mark-ring))
                   (current-buffer)))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history 'global-mark-ring (copy-marker position)
                      global-mark-ring-max t)
      (when old
        (set-marker old nil))))
  nil)

(defun hel--jump-over-mark-ring (&optional backward?)
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' forward (or BACKWARD)
and jump to the new top position."
  (when mark-ring
    (hel-maybe-deactivate-mark)
    (when (= (point) (car mark-ring))
      (cl-callf hel-rotate-ring mark-ring backward?))
    (hel-recenter-point-on-jump
      (goto-char (car mark-ring)))))

(defun hel--jump-over-global-mark-ring (&optional backward?)
  "Jump to the top location on the `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
forward (or BACKWARD) and jump to new top location."
  ;; Delete entries that refer to non-existent buffers.
  (when (setq global-mark-ring (-filter #'marker-buffer global-mark-ring))
    (when (eq (marker-buffer (car global-mark-ring))
              (current-buffer))
      (cl-callf hel-rotate-ring global-mark-ring backward?))
    (hel-recenter-point-on-jump
      (let* ((marker (car global-mark-ring))
             (buffer (marker-buffer marker))
             (position (marker-position marker)))
        (set-buffer buffer)
        (or (<= (point-min) position (point-max))
            (if widen-automatically
                (widen)
              (error "Global mark position is outside accessible part of buffer %s"
                     (buffer-name buffer))))
        (goto-char position)
        (switch-to-buffer buffer)
        (deactivate-mark)))))

(defun hel-rotate-ring (ring &optional backward-p)
  "Rotate the RING elements.
This function destructively modify RING and should be used the following way:
`(setq RING (hel-rotate-ring RING))'

RING should be a list like `mark-ring', not the ring structure from
`ring.el'."
  (if backward-p
      (nconc (last ring) (nbutlast ring))
    (nconc (cdr ring) (list (car ring)))))

;;; Copy/paste

(defun hel-push-mark (&optional position nomsg activate)
  "Set mark to the POSITION and push it on the `mark-ring'.
If NOMSG is nil show `Mark set' message in echo area."
  (or position (setq position (point)))
  (hel-push-point position)
  (set-marker (mark-marker) position (current-buffer))
  (or nomsg executing-kbd-macro (< 0 (minibuffer-depth))
      (message "Mark set"))
  (when activate
    (set-mark (mark t)))
  nil)

(defvar hel--yank-transform-linewise-selection? nil)

(defun hel-paste (yank-function direction)
  "Paste before/after selection depending on DIRECTION.
YANK-FUNCTION should be a `yank' like function."
  (let ((region-dir (if (use-region-p) (hel-region-direction) 1))
        (deactivate-mark nil))
    (setq hel--yank-transform-linewise-selection?
          (when (use-region-p)
            (hel-ensure-region-direction direction)
            (hel--fix-newline-at-end-of-buffer)
            (hel-linewise-selection-p direction)))
    (cl-letf ((yank-transform-functions (cons #'hel--yank-transform
                                              yank-transform-functions))
              ;; Intercept `push-mark' so that any time `yank' calls it,
              ;; `hel-push-mark' is executed instead.
              ((symbol-function 'push-mark) #'hel-push-mark))
      (funcall yank-function))
    (hel-select (hel-range (mark t) (point)) region-dir)
    (hel-extend-selection -1)
    (when (and (derived-mode-p 'prog-mode)
               (use-region-p))
      (indent-region (region-beginning) (region-end)))))

(defun hel--yank-transform (str)
  (if (and (not (string-empty-p str))
           (xor hel--yank-transform-linewise-selection?
                (hel-string-ends-with-newline str)))
      (if hel--yank-transform-linewise-selection?
          (concat str "\n")
        (string-trim-right str "[\r\n]+"))
    str))

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
  "Indent active region COUNT times. With no selection indent current line.
INDENT-FUNCTION should be a `indent-rigidly-left' like function that takes
BEGIN, END position and done the indentation."
  (cond ((hel-linewise-selection-p)
         (let ((deactivate-mark nil))
           (dotimes (_ count)
             (funcall indent-function (region-beginning) (region-end))))
         (hel-extend-selection -1))
        ((use-region-p)
         (hel-save-selection
           (hel-expand-selection-to-full-lines)
           (dotimes (_ count)
             (funcall indent-function (region-beginning) (region-end))))
         (hel-extend-selection -1))
        (t
         (-let [(begin . end) (bounds-of-thing-at-point 'hel-line)]
           (dotimes (_ count)
             (funcall indent-function begin end))))))

(defun hel--fix-newline-at-end-of-buffer ()
  "If selection ends at the end of buffer, and buffer doesn't ends with newline
character -- add it and adjust selection."
  ;; This function assumes that region is active, but doesn't check it!
  (cond ((or (minibufferp) buffer-read-only)
         nil)
        ;; region is forward
        ((< (mark-marker) (point))
         (when (and (eobp) (not (bolp)))
           (let ((deactivate-mark nil))
             (insert ?\n))))
        ;; else region is backward
        ((= (mark-marker) (point-max))
         (save-excursion
           (goto-char (mark-marker))
           (when (and (eobp) (not (bolp)))
             (let ((deactivate-mark nil))
               (insert ?\n))
             (move-marker (mark-marker) (point)))))))

;;; Fold opening

(cl-defun hel-range-visible? ((begin . end))
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
  (or hel--extend-selection
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
