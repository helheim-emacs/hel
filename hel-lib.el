;;; hel-lib.el --- Common functions -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.12.0
;; Homepage: https://github.com/anuvyklack/hel
;;
;; This file is not part of GNU Emacs.
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
the loop immediately quits.

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

(defmacro hel-save-region (&rest body)
  "Evaluate BODY with preserving original region.
The difference from `save-mark-and-excursion' is that both point and mark are
saved as markers and correctly handle case when text was inserted before region."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (pnt beg end dir)
    `(if (use-region-p)
         (let ((deactivate-mark nil)
               (,beg (copy-marker (region-beginning) t))
               (,end (copy-marker (region-end)))
               (,dir (hel-region-direction)))
           (unwind-protect
               (save-excursion ,@body)
             (hel-set-region ,beg ,end ,dir)
             (set-marker ,beg nil)
             (set-marker ,end nil)))
       ;; else
       (let ((,pnt (copy-marker (point) t)))
         (unwind-protect
             (save-excursion ,@body)
           (if mark-active (deactivate-mark))
           (goto-char ,pnt)
           (set-marker ,pnt nil))))))

(defmacro hel-restore-region-on-error (&rest body)
  "Restore initial region if error occured during BODY evaluation."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (region point something-goes-wrong?)
    `(let ((,region (hel-region))
           (,point (point))
           (,something-goes-wrong? t))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,something-goes-wrong? nil))
         (when ,something-goes-wrong?
           (if ,region
               (apply #'hel-set-region ,region)
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
      (hel-visual-bolp)
    (bolp)))

(defun hel-visual-bolp ()
  "Return t if point is at the beginning of visual line."
  (save-excursion
    (let ((p (point)))
      (beginning-of-visual-line)
      (= p (point)))))

(defun hel-eolp ()
  "Like `eolp' but consider visual lines when `visual-line-mode' is enabled."
  (if visual-line-mode
      (hel-visual-eolp)
    (eolp)))

(defun hel-visual-eolp ()
  "Return t if point is at the end of visual line."
  (save-excursion
    (let ((p (point)))
      (end-of-visual-line)
      (= p (point)))))

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
         (let ((beg (region-beginning))
               (end (region-end)))
           (cond ((null direction)
                  (and (progn (goto-char beg) (bolp))
                       (progn (goto-char end) (bolp))))
                 ((<= direction 0)
                  (and (progn (goto-char beg) (bolp))
                       ;; at least one full line is selected
                       (< 0 (- (line-number-at-pos end)
                               (line-number-at-pos beg)))))
                 (t
                  (and (progn (goto-char end) (bolp))
                       ;; at least one full line is selected
                       (< 0 (- (line-number-at-pos end)
                               (line-number-at-pos beg))))))))))

(defun hel-visual-lines-p ()
  "Return t if active region spawns visual lines."
  (and visual-line-mode
       (use-region-p)
       (save-excursion (goto-char (region-beginning))
                       (hel-visual-bolp))
       (save-excursion (goto-char (region-end))
                       (hel-visual-bolp))))

(defun hel-whitespace? (char)
  "Non-nil when CHAR belongs to whitespace syntax class."
  (and (eql (char-syntax char) ?\s)
       (not (memq char '(?\r ?\n))))
  ;; Alternative:
  ;; (memq char '(?\s ?\t))
  )

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
    (let* ((beg (point))
           (end (+ beg (* (length string) direction))))
      (and (<= (point-min) end (point-max))
           (string-equal (buffer-substring-no-properties beg end) string)))))

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

(defun hel-set-region (start end &optional direction)
  "Set the active region between START and END positions.

DIRECTION of the region:
  nil      Region direction is from START to END.
   1       Force forward region (mark < point).
  -1       Force backward region (point < mark).

When DIRECTION is specified, START and END can be provided in any order."
  (when (and (numberp direction)
             (xor (< 0 direction)
                  (<= start end)))
    (cl-rotatef start end))
  (set-mark start)
  (goto-char end))

(defun hel-region ()
  "Region list with parameters of the active region. If no region return nil.

The result is a list with following elements:

  (BEG END DIRECTION)

It is suitable to restore region with `hel-set-region':

  (let ((region (hel-region)))
    ...
    (apply #'hel-set-region region))"
  (if (use-region-p)
      (list (region-beginning) (region-end) (hel-region-direction))))

(defun hel-maybe-set-mark ()
  "Set mark at point unless extending selection is active."
  (or hel--extend-selection
      (set-mark (point))))

(defun hel-maybe-deactivate-mark ()
  "Deactivate mark unless extending selection is active."
  (or hel--extend-selection
      (deactivate-mark)))

(defun hel-ensure-region-direction (direction)
  "Exchange point and mark if region direction mismatch DIRECTION.
DIRECTION should be 1 or -1."
  (when (/= direction (hel-region-direction))
    (hel--exchange-point-and-mark)))

(defun hel-undo-command-p (command)
  "Return non-nil if COMMAND is implementing undo/redo functionality."
  (memq command hel-undo-commands))

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
  "Convert PCRE REGEXP into Elisp one if Hel configured to use PCRE syntax."
  (if (and hel-use-pcre-regex
           (not (string-empty-p regexp)))
      (condition-case err
          (pcre-to-elisp regexp)
        (rxt-invalid-regexp
         (message (-> (error-message-string err)
                      (propertize 'face 'error)))))
    regexp))

(cl-defun hel-collect-positions (fun &optional (start (window-start))
                                               (end (window-end)))
  "Consecutively call FUN and collect point positions after each invocation.
Finish as soon as point moves outside of START END buffer positions.
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
                        (<= start (point) end))
             collect (cons (point) win))))

(defun hel-invert-case-in-region (start end)
  "Invert case of characters within START...END buffer positions."
  (goto-char start)
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
          (when-let ((s (car (syntax-after pos))))
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

(defun hel-pulse-main-region (&optional face)
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

(defun hel-replace-chars (beg end char)
  "Replace each non-newline character between BEG and END with CHAR."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if (eq (char-after) ?\n)
          (forward-char 1)
        (insert-char char)
        (delete-char 1)))))

(defun hel-read-char-and-replace (beg end)
  "Replace characters in BEG..END with an interactive entered one."
  (when (< beg end)
    (let ((overlay (-doto (make-overlay beg end nil t nil)
                     (overlay-put 'face 'region))))
      (unwind-protect
          (hel-replace-chars beg end (read-char "replace: " t))
        (delete-overlay overlay)))))

(cl-defun hel-hide-cursor (&optional (window (selected-window)))
  "Hide the cursor in WINDOW and return a function that restores it."
  (cond ((fboundp 'set-window-cursor-type) ;; Emacs 31
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

;;; Motions

(defun hel-forward-following-thing (thing &optional count)
  "Move forward to the end of the COUNT following THING.
`forward-thing' first moves to the  boundary of the current THING, then to the
next THING. This function skips first step and always moves to the next THING."
  (or count (setq count 1))
  (if (zerop count) 0
    (-when-let ((beg . end) (bounds-of-thing-at-point thing))
      (goto-char (if (< count 0) beg end)))
    (forward-thing thing count)))

(defun hel-forward-beginning-of-thing (thing &optional count)
  "Move to the beginning of COUNT-th next THING.
Move backward if COUNT is negative.
Returns the count of steps left to move.

Works only with THINGs, that returns the count of steps left to move,
such as `hel-word', `hel-sentence', `hel-line', `paragraph'."
  (or count (setq count 1))
  (if (zerop count) 0
    (let ((rest (hel-forward-following-thing thing count)))
      (when-let* (((/= rest count))
                  ((natnump count)) ; moving forward
                  (bounds (bounds-of-thing-at-point thing)))
        (goto-char (car bounds)))
      rest)))

(defun hel-forward-end-of-thing (thing &optional count)
  "Move to the end of COUNT-th next THING.
Move backward if COUNT is negative.
Returns the count of steps left to move.

Works only with THINGs, that returns the count of steps left to move,
such as `hel-word', `hel-sentence', `hel-line', `paragraph'."
  (or count (setq count 1))
  (if (zerop count) 0
    (let ((rest (hel-forward-following-thing thing count)))
      (when (and (/= rest count)
                 (< count 0)) ;; moving backward
        (forward-thing thing))
      rest)))

(defun hel-skip-chars (chars &optional direction)
  "Move point toward the DIRECTION stopping after a char is not in CHARS string.
Move backward when DIRECTION is negative number, forward — otherwise.
Return t if point has moved."
  (/= 0 (if (natnump (or direction 1))
            (skip-chars-forward chars)
          (skip-chars-backward chars))))

(defun hel-skip-whitespaces (&optional direction)
  "Move point toward the DIRECTION across whitespace.
Move backward when DIRECTION is negative number, forward — otherwise.
Return the distance traveled positive or negative depending on DIRECTION."
  ;; Alternative: (hel-skip-chars " \t" dir)
  (if (natnump (or direction 1))
      (skip-syntax-forward " " (line-end-position))
    (skip-syntax-backward " " (line-beginning-position))))

(defun hel-next-char (&optional direction)
  "Return the next after point char toward the DIRECTION.
If DIRECTION is positive number — get following char, otherwise preceding char."
  (if (natnump (or direction 1))
      (following-char)
    (preceding-char)))

(defun hel-beginning-of-line (&optional count)
  "Move point to the beginning of current line.
Move over visual line when `visual-line-mode' is active."
  (if visual-line-mode
      (beginning-of-visual-line count)
    (hel--beginning-of-line count))
  (point))

(defun hel-end-of-line (&optional count)
  "Move point to the end of current line.
Move over visual line when `visual-line-mode' is active."
  (if visual-line-mode
      (end-of-visual-line count)
    (move-end-of-line count))
  (point))

(defun hel--forward-word-start (thing count)
  "Move to the COUNT-th next start of a word-like THING."
  (cl-assert (< 0 count))
  (skip-chars-forward "\r\n")
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (when (hel-whitespace? (following-char))
                           (cl-decf count))
                         (forward-thing thing count)
                         (hel-skip-whitespaces)
                         (point))))

(defun hel--backward-word-start (thing count)
  "Move to the COUNT-th previous start of a word-like THING."
  (cl-assert (< 0 count))
  (skip-chars-backward "\r\n")
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (forward-thing thing (- count))
                         (point))))

(defun hel--forward-word-end (thing count)
  "Move to the COUNT-th next word-like THING end."
  (interactive "p")
  (cl-assert (< 0 count))
  (skip-chars-forward "\r\n")
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (forward-thing thing count)
                         (point))))

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
               (hel--beginning-of-line 1)
               ;; `hel--beginning-of-line' may leave point after invisible
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

(defun hel--beginning-of-line (&optional count)
  "Move point to the beginning of the current visible logical line.
This is actually refactored `move-beginning-of-line' command."
  (or count (setq count 1))
  (let ((init-point (point)))
    ;; Move by lines, if COUNT is not 1 (the default).
    (when (/= count 1)
      (let ((line-move-visual nil))
        (line-move (1- count) t)))
    ;; Move to beginning-of-line, ignoring fields and invisible text.
    (let ((inhibit-field-text-motion t))
      (goto-char (line-beginning-position))
      (while (and (not (bobp)) (invisible-p (1- (point))))
        (goto-char (previous-char-property-change (point)))
        (goto-char (line-beginning-position))))
    ;; Now find first visible char in the line.
    (while (and (< (point) init-point)
                (invisible-p (point)))
      (goto-char (next-char-property-change (point) init-point)))
    ;; Obey field constraints.
    (goto-char (constrain-to-field (point) init-point (/= count 1) t nil))))

;;;; `hel-visual-line'

(put 'hel-visual-line 'forward-op #'vertical-motion)
;; (put 'hel-visual-line 'beginning-op #'beginning-of-visual-line)
;; (put 'hel-visual-line 'end-op       #'end-of-visual-line)

;;;; `hel-word'

(defun forward-hel-word (&optional count)
  "Move point COUNT words forward (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence non-word non-whitespace characters matching `[^[:word:]\\n\\r\\t\\f ]'"
  (hel-motion-loop (dir (or count 1))
    (hel-skip-chars "\r\n" dir)
    (hel-skip-whitespaces dir)
    (or (hel-line-boundary-p dir)
        (hel-skip-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories hel-cjk-word-separating-categories)
              (word-combining-categories  hel-cjk-word-combining-categories))
          (forward-word dir)))))

;;;; `hel-WORD'

(defun forward-hel-WORD (&optional count)
  "Move point COUNT WORDs forward (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is any space separated sequence of characters."
  (hel-motion-loop (dir (or count 1))
    (hel-skip-chars "\r\n" dir)
    (hel-skip-whitespaces dir)
    (unless (hel-line-boundary-p dir)
      (hel-skip-chars "^\n\r\t\f " dir))))

;;;; `hel-sentence'

(defun forward-hel-sentence (&optional count)
  "Move point COUNT sentences forward (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT.

What is sentence is defined by `forward-sentence-function'."
  (hel-motion-loop (dir (or count 1))
    (ignore-errors (forward-sentence dir))))

;;;; `hel-paragraph'

(defun forward-hel-paragraph (&optional count)
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

;;;; `hel-function'

(defun forward-hel-function (&optional count)
  "Move point COUNT functions forward (backward if COUNT is negative).
Returns then count of functions left to move, positive of negative depending
on sign of COUNT."
  (hel-motion-loop (dir (or count 1))
    (if (< dir 0) (beginning-of-defun) (end-of-defun))))

;;;; `hel-sexp'

(defun forward-hel-sexp (&optional count)
  (hel-motion-loop (dir (or count 1))
    (ignore-errors
      (forward-sexp dir))))

(defun hel-forward-sexp-only (&optional count)
  "Default value for `forward-sexp-function'.
Unlike `forward-sexp-default-function' this one doesn't move to the end of the
buffer if no sexp forward."
  (when-let ((pos (scan-sexps (point) count)))
    (goto-char pos)
    (if (< count 0) (backward-prefix-chars))))

(defun hel--setup-default-forward-sexp-func-h ()
  (setq-default forward-sexp-function (if hel-mode
                                          #'hel-forward-sexp-only
                                        nil)))

(add-hook 'hel-mode-hook #'hel--setup-default-forward-sexp-func-h)

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
        (let ((beg (region-beginning))
              (end (region-end))
              (dir (or direction (hel-region-direction))))
          (hel-set-region (progn (goto-char beg)
                                 (car (bounds-of-thing-at-point 'hel-line)))
                          (progn (goto-char end)
                                 (cdr (bounds-of-thing-at-point 'hel-line)))
                          dir))
      ;; else no region
      (-let [(beg . end) (bounds-of-thing-at-point 'hel-line)]
        (hel-set-region beg end direction)))
    (hel--fix-newline-at-end-of-buffer)
    t))

(cl-defun hel-mark-inner-thing (thing &optional (count 1))
  (cl-assert (/= count 0))
  (-let (((beg . end) (hel-bounds-of-count-things-at-point thing count)))
    (hel-set-region beg end (hel-sign count))))

(defun hel-mark-a-thing (thing count)
  "Select COUNT THINGs with spacing around.
Works only with THINGs, that returns the count of steps left to move,
such as `paragraph', `hel-function'."
  (-let* (((thing-beg . thing-end) (hel-bounds-of-count-things-at-point thing count))
          ((beg . end)
           (or (progn
                 (goto-char thing-end)
                 (-if-let ((_ . space-end)
                           (hel-bounds-of-complement-of-thing-at-point thing))
                     (cons thing-beg space-end)))
               (progn
                 (goto-char thing-beg)
                 (-if-let ((space-beg . _)
                           (hel-bounds-of-complement-of-thing-at-point thing))
                     (cons space-beg thing-end)))
               (cons thing-beg thing-end))))
    (hel-set-region beg end (hel-sign count))))

(defun hel-bounds-of-count-things-at-point (thing count)
  "Return the bounds of COUNT things at point.
Count things forward if COUNT is positive, or backward if negative."
  (cl-assert (/= count 0))
  (save-excursion
    (let* ((dir (hel-sign count))
           (beg (-if-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
                    (progn
                      (if (< dir 0) (cl-rotatef thing-beg thing-end))
                      (prog1 thing-beg
                        (goto-char thing-end)
                        (cl-callf - count dir)))
                  ;; else
                  (forward-thing thing dir)
                  (forward-thing thing (- dir))
                  (point)))
           (end (progn
                  (setq count (forward-thing thing count))
                  (point))))
      (when (/= count 0)
        (goto-char beg)
        (forward-thing thing (- count))
        (setq beg (point)))
      (if (< end beg) (cl-rotatef beg end))
      (cons beg end))))

(defun hel-bounds-of-complement-of-thing-at-point (thing)
  "Return the bounds of the gap between two THINGs at point.
If there is a THING at point — return nil.

Works only with THINGs, that returns the count of steps left to move,
such as `hel-word', `hel-sentence', `hel-line', `paragraph'."
  (let ((orig-point (point)))
    (if-let* ((beg (save-excursion
                     (and (zerop (forward-thing thing -1))
                          (forward-thing thing))
                     (if (<= (point) orig-point)
                         (point))))
              (end (save-excursion
                     (and (zerop (forward-thing thing))
                          (forward-thing thing -1))
                     (if (<= orig-point (point))
                         (point))))
              ((and (<= beg (point) end)
                    (< beg end))))
        (cons beg end))))

(defun hel-mark-thing-forward (thing count)
  "Select from point to the end of the THING (or COUNT following THINGs).
If no THING at point select COUNT following THINGs."
  (hel-restore-region-on-error
    (let ((point-pos (point))
          (dir (hel-sign count)))
      (if (< dir 0)
          (when (bobp) (user-error "Beginning of buffer"))
        (when (eobp) (user-error "End of buffer")))
      (hel-push-point point-pos)
      (let ((start (if hel--extend-selection
                       (mark)
                     (when (-if-let ((thing-beg . thing-end)
                                     (bounds-of-thing-at-point thing))
                               ;; We are at the boundary of the THING toward
                               ;; the motion direction.
                               (= (point)
                                  (if (< dir 0) thing-beg thing-end))
                             ;; No thing at point at all.
                             t)
                       (hel-forward-following-thing thing dir)
                       (forward-thing thing (- dir)))
                     (point)))
            (end (progn (forward-thing thing count)
                        (point))))
        (hel-set-region start end)
        (when (= (region-beginning) (region-end))
          (hel-mark-thing-forward thing dir))
        (hel-reveal-point-when-on-top)))))

(defun hel--mark-a-word (thing)
  "Inner implementation of `hel-mark-a-word' and `hel-mark-a-WORD' commands."
  (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
    (-let [(beg . end)
           (or (progn
                 (goto-char thing-end)
                 (with-restriction
                     (line-beginning-position) (line-end-position)
                   (-if-let ((_ . space-end)
                             (hel-bounds-of-complement-of-thing-at-point thing))
                       (cons thing-beg space-end))))
               (progn
                 (goto-char thing-beg)
                 (with-restriction
                     (save-excursion (back-to-indentation) (point))
                     (line-end-position)
                   (-if-let ((space-beg . _)
                             (hel-bounds-of-complement-of-thing-at-point thing))
                       (cons space-beg thing-end))))
               (cons thing-beg thing-end))]
      (hel-set-region beg end))))

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
              (if (null spec)
                  `(:remove ,(cons (char-to-string char)
                                   (char-to-string char)))
                spec)))
        (hel-surround-4-bounds-at-point left right limits regexp balanced)))))

(declare-function org-at-table-p  "org-table")
(declare-function org-table-begin "org-table")
(declare-function org-table-end   "org-table")

(defun hel-bounds-of-quoted-at-point (quote-mark)
  "Return a cons cell (START . END) with bounds of text region
enclosed in QUOTE-MARKs."
  (if-let* ((limits (or (bounds-of-thing-at-point 'hel-comment)
                        (bounds-of-thing-at-point 'string)
                        (if (and (require 'org-table nil t)
                                 (org-at-table-p))
                            (cons (org-table-begin) (org-table-end))))))
      (-if-let ((beg _ _ end) (hel-surround-4-bounds-at-point
                               (char-to-string quote-mark)
                               (char-to-string quote-mark)
                               limits))
          (cons beg end))
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

Return the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with
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
        (-if-let ((beg . end) (hel-bounds-of-brackets-at-point
                               (string-to-char left) (string-to-char right)))
            (list beg (1+ beg) (1- end) end))
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
        (let* ((left-beg (point))
               (left-end (if regexp? (match-end 0)
                           (+ left-beg (length left)))))
          (goto-char left-end)
          (if-let* ((right-end (hel-surround-search-outward
                                left right 1 limits regexp? balanced?))
                    (right-beg (if regexp? (match-beginning 0)
                                 (- right-end (length right)))))
              (list left-beg left-end right-beg right-end))))
       ;; point is after RIGHT
       ((and left-not-equal-right?
             (hel-looking-at right -1 regexp?))
        (let* ((right-end (point))
               (right-beg (if regexp? (match-beginning 0)
                            (- right-end (length right)))))
          (goto-char right-beg)
          (if-let* ((left-beg (hel-surround-search-outward
                               left right -1 limits regexp? balanced?))
                    (left-end (if regexp? (match-end 0)
                                (+ left-beg (length left)))))
              (list left-beg left-end right-beg right-end))))
       (t
        (if-let* ((left-beg (hel-surround-search-outward
                             left right -1 limits regexp? balanced?))
                  (left-end (if regexp? (match-end 0)
                              (+ left-beg (length left))))
                  (right-end (hel-surround-search-outward
                              left right 1 limits regexp? balanced?))
                  (right-beg (if regexp? (match-beginning 0)
                               (- right-end (length right)))))
            (list left-beg left-end right-beg right-end)))))))

(defun hel-bounds-of-brackets-at-point (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `hel-surround-4-bounds-at-point'.

Return the cons cell (START . END) with positions before LEFT and
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
      (-let [(beg _ _ end) bounds]
        (cons beg end))
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
                 (if-let* ((beg (scan-lists pnt -1 0)))
                     (cons beg pnt)))
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

Return the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with 4 positions:
1. Before LEFT bracket;
2. After LEFT bracket all following whitespaces and newlines;
3. Before RIGHT bracket all preceding whitespaces and newlines;
4. After RIGHT bracket."
  (-if-let ((left-beg . right-end) (hel-bounds-of-brackets-at-point left right))
      (save-excursion
        (let ((left-end (progn
                          (goto-char (1+ left-beg))
                          (skip-chars-forward " \t\r\n")
                          (point)))
              (right-beg (progn
                           (goto-char (1- right-end))
                           (skip-chars-backward " \t\r\n")
                           (point))))
          (list left-beg left-end right-beg right-end)))))

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
          (-setq open left
                 close right
                 (_ . limit) limits)
        (-setq open right
               close left
               (limit . _) limits))
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
  "Return a cons cell (START . END) with bounds of region around
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
              (hel-range-visible? (match-beginning 0) (match-end 0)))
          (setq found t)))
    (if found (point))))

;;; Mark ring

(defun hel-push-point (&optional position)
  "Push POSITION (point by default) on the `mark-ring'."
  (or position (setq position (point)))
  ;; Don't store POSITION into mark ring if it equals to the last stored one.
  (unless (and mark-ring
               (= (point) (car mark-ring)))
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

RING should be a list like `mark-ring' and not the ring structure from `ring.el'."
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
    (hel-set-region (mark t) (point) region-dir)
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
BEG, END position and done the indentation."
  (cond ((hel-linewise-selection-p)
         (let ((deactivate-mark nil))
           (dotimes (_ count)
             (funcall indent-function (region-beginning) (region-end))))
         (hel-extend-selection -1))
        ((use-region-p)
         (hel-save-region
           (hel-expand-selection-to-full-lines)
           (dotimes (_ count)
             (funcall indent-function (region-beginning) (region-end))))
         (hel-extend-selection -1))
        (t
         (-let [(beg . end) (bounds-of-thing-at-point 'hel-line)]
           (dotimes (_ count)
             (funcall indent-function beg end))))))

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

(defun hel-range-visible? (start end)
  "Test whether the text in [START, END) range can be shown to the user.

Return values:

  nil     The range is truly-invisible — i.e. it is fully hidden by
          an `invisible' text property or by a non-openable overlay.

  list    The range is fully or partially folded — i.e. is hidden by
          openable overlay — the one with `isearch-open-invisible'
          property. Return a list of these overlays.

  t       The range is, at least partially, visible with no folds to open.
          (E.g. Org link with hidden target.)"
  (when (/= start end)
    (when (< end start) (cl-rotatef end start))
    (save-excursion
      ;; The range is "showable" if any of its positions is visible, or
      ;; or hidden by openable overlay.
      (let ((visible? nil)
            (openable-overlays nil))
        ;; Walk the whole range, stepping at every text-property or
        ;; overlay boundary.
        (goto-char start)
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

(declare-function hel-extend-selection "hel-commands")
(declare-function hel-insert-state "hel-core")

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
