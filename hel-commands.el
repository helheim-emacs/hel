;;; hel-commands.el --- Interactive commands -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.10.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'pcre2el)
(require 'thingatpt)
(require 'hel-vars)
(require 'hel-common)
(require 'hel-multiple-cursors-core)
(require 'hel-core)
(require 'hel-search)
(require 'avy)

;; ESC in normal state
(hel-define-command hel-normal-state-escape ()
  "Command for ESC key in Hel Normal state."
  :multiple-cursors t
  (interactive)
  (cond (hel--extend-selection
         (hel-extend-selection -1))
        (t
         (deactivate-mark))))

;;; Motions

;; h
(hel-define-command hel-backward-char (count)
  "Move backward COUNT chars."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel-maybe-deactivate-mark)
  (backward-char count))

;; l
(hel-define-command hel-forward-char (count)
  "Move forward COUNT chars."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel-maybe-deactivate-mark)
  (forward-char count))

;; j
(hel-define-command hel-next-line (count)
  "Move to the next COUNT line. Move upward if COUNT is negative.
If both linewise selection (`x' key) and extending selection (`v' key)
are active — works like `hel-expand-line-selection'."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (if (and hel--extend-selection (hel-linewise-selection-p))
      (hel-expand-line-selection count)
    ;; else
    (hel-maybe-deactivate-mark)
    ;; Preserve the column: the behaviour is hard-coded and the column is
    ;; preserved if and only if the previous was `next-line' or `previous-line'.
    (setq this-command (if (natnump count) 'next-line 'previous-line))
    (funcall-interactively 'next-line count)))

;; k
(hel-define-command hel-previous-line (count)
  "Move to the previous COUNT line. Move downward if COUNT is negative.
If both linewise selection (`x' key) and extending selection (`v' key)
are active — works like `hel-expand-line-selection-backward'."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel-next-line (- count)))

;; w
(hel-define-command hel-forward-word-start (count)
  "Move to the COUNT-th next word start."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--forward-word-start 'hel-word count))

;; W
(hel-define-command hel-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--forward-word-start 'hel-WORD count))

;; b
(hel-define-command hel-backward-word-start (count)
  "Move to the COUNT-th previous word start."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--backward-word-start 'hel-word count))

;; B
(hel-define-command hel-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--backward-word-start 'hel-WORD count))

;; e
(hel-define-command hel-forward-word-end (count)
  "Move to the COUNT-th next word end."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--forward-word-end 'hel-word count))

;; E
(hel-define-command hel-forward-WORD-end (count)
  "Move COUNT-th next WORD end."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (hel--forward-word-end 'hel-WORD count))

;; gg
(hel-define-command hel-beginning-of-buffer (num)
  "Move point to the beginning of the buffer.
With numeric arg NUM, put point NUM/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.
Push mark at previous position, unless extending selection."
  :multiple-cursors nil
  (interactive "P")
  (hel-delete-all-fake-cursors)
  (hel-push-point)
  (hel-maybe-deactivate-mark)
  (if num
      (progn
        (goto-char (+ (point-min)
                      (/ (* (- (point-max) (point-min))
                            (prefix-numeric-value num))
                         10)))
        (forward-line 1))
    ;; else
    (goto-char (point-min))
    (recenter 0)))

;; G
(hel-define-command hel-end-of-buffer ()
  "Move point the end of the buffer."
  :multiple-cursors nil
  (interactive)
  (hel-delete-all-fake-cursors)
  (hel-push-point)
  (hel-maybe-deactivate-mark)
  (goto-char (point-max)))

;; gs
(hel-define-command hel-beginning-of-line-command ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only hel-beginning-of-line-command))
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (hel-beginning-of-line)))

;; gh
(hel-define-command hel-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (hel-beginning-of-line)
                         (skip-syntax-forward " " (line-end-position))
                         (backward-prefix-chars)
                         (point))))

;; gl
(hel-define-command hel-end-of-line-command ()
  "Move point to end of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only hel-end-of-line-command))
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (hel-end-of-line))
  ;; "Stick" cursor to the end of line after moving to it. Vertical
  ;; motions right after "gl" will place point at the end of each line.
  (when (and (not visual-line-mode)
             (eolp))
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line)))

;; }
(hel-define-command hel-forward-paragraph (count &optional move-to-bound?)
  "Select to the beginning of the COUNT-th next paragraph."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((thing 'hel-paragraph)
        (dir (hel-sign count)))
    (hel-restore-region-on-error
      (if (< dir 0)
          (when (bobp) (user-error "Beginning of buffer"))
        (when (eobp) (user-error "End of buffer")))
      (hel-push-point (point))
      (hel-set-region (if hel--extend-selection (mark) (point))
                      (progn
                        (cond ((and (< dir 0) move-to-bound?)
                               (hel-forward-end-of-thing thing count))
                              ((and (< 0 dir) (not move-to-bound?))
                               (hel-forward-beginning-of-thing thing count))
                              (t
                               (forward-thing thing count)))
                        (point))
                      (unless hel--extend-selection dir))
      (hel-reveal-point-when-on-top))))

;; {
(hel-define-command hel-backward-paragraph (count)
  "Select to the beginning of the COUNT-th previous paragraph."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-forward-paragraph (- count)))

;; ]p
(hel-define-command hel-forward-paragraph-end (count)
  "Select to the end of the COUNT-th next paragraph."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-forward-paragraph count t))

;; [p
(hel-define-command hel-backward-paragraph-end (count)
  "Select to the end of the COUNT-th previous paragraph."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-forward-paragraph (- count) t))

;; ]f (alternative version)
(hel-define-command hel-mark-function-forward (count)
  "Select from point to the end of the function (or COUNT-th next functions).
If no function at point select COUNT next functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-thing-forward 'hel-function count))

;; [f (alternative version)
(hel-define-command hel-mark-function-backward (count)
  "Select from point to the end of the function (or COUNT-th next functions).
If no function at point select COUNT previous functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-thing-forward 'hel-function (- count)))

;; ]s
(hel-define-command hel-mark-sentence-forward (count)
  "Select from point to the end of the sentence (or COUNT-th next sentences).
If no sentence at point select COUNT next sentences."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-thing-forward 'hel-sentence count))

;; [s
(hel-define-command hel-mark-sentence-backward (count)
  "Select from point to the start of the sentence (or COUNT-th next sentences).
If no sentence at point select COUNT previous sentences."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-thing-forward 'hel-sentence (- count)))

;; mm
;; TODO: The most bare-boned version. Need upgrade.
(hel-define-command hel-jump-to-match-item ()
  "Jump between matching brackets."
  :multiple-cursors t
  (interactive)
  (hel-maybe-deactivate-mark)
  (ignore-errors
    (cond
     ;; before open bracket
     ((and (/= (point) (point-max))
           (eq 4 (syntax-class (syntax-after (point)))))
      (forward-list 1))
     ;; after close bracket
     ((and (/= (point) (point-min))
           (eq 5 (syntax-class (syntax-after (1- (point))))))
      (forward-list -1))
     (t
      (up-list 1))))
  (hel-reveal-point-when-on-top))

;; C-s
(hel-define-command hel-save-point-to-mark-ring ()
  "Store main cursor position to `mark-ring'."
  :multiple-cursors nil
  (interactive)
  (hel-push-point))

;; C-o
(hel-define-command hel-backward-mark-ring ()
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' forward (like revolver cylinder)
and jump to the new top position.

In Emacs, mark has two purposes: when active, it acts as a boundary for region;
when inactive, it can be used to store the previous significant position. The
Hel approach is based on selections, and mark is never used to store previous
position. So unlike the `pop-to-mark-command' which puts the value from `mark'
into `point' and value from `mark-ring' into `mark', this command puts the value
from `mark-ring' directly into `point' skipping `mark'.

\(Does not affect global mark ring)."
  :multiple-cursors t
  (interactive)
  (hel--jump-over-mark-ring))

;; C-i
(hel-define-command hel-forward-mark-ring ()
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' backward and jump to new top
position. See `hel-backward-mark-ring'.

\(Does not affect global mark ring)."
  :multiple-cursors t
  (interactive)
  (hel--jump-over-mark-ring t))

;; C-S-o
(hel-define-command hel-backward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
forward (like revolver cylinder) and jump to new top location."
  :multiple-cursors t
  (interactive)
  (hel--jump-over-global-mark-ring))

;; C-S-i
(hel-define-command hel-forward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
backward and jump to new top location."
  :multiple-cursors t
  (interactive)
  (hel--jump-over-global-mark-ring t))

;;; Avy (Easymotion)

;; gw
(hel-define-command hel-avy-word-forward ()
  "Move to a word start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (point) (window-end nil t))
                (avy-process)))
      (hel-delete-all-fake-cursors)
      (hel-push-point orig-point)
      (hel-set-region (if hel--extend-selection (mark) (point))
                      (progn (forward-thing 'hel-word)
                             (point))))))

;; gb
(hel-define-command hel-avy-word-backward ()
  "Move to a word start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (window-start) (point))
                (nreverse)
                (avy-process)))
      (hel-delete-all-fake-cursors)
      (hel-push-point orig-point)
      (if hel--extend-selection
          (hel-set-region (mark) (point))
        (hel-set-region (point)
                        (progn (forward-thing 'hel-word)
                               (point)))))))

;; gW
(hel-define-command hel-avy-WORD-forward ()
  "Move to a WORD start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (point) (window-end nil t))
                (avy-process)))
      (hel-delete-all-fake-cursors)
      (hel-push-point orig-point)
      (hel-set-region (if hel--extend-selection (mark) (point))
                      (progn (forward-thing 'hel-WORD)
                             (point))))))

;; gB
(hel-define-command hel-avy-WORD-backward ()
  "Move to a WORD start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (window-start) (point))
                (nreverse)
                (avy-process)))
      (hel-delete-all-fake-cursors)
      (hel-push-point orig-point)
      (if hel--extend-selection
          (hel-set-region (mark) (point))
        (hel-set-region (point)
                        (progn (forward-thing 'hel-WORD)
                               (point)))))))

;; gj
(hel-define-command hel-avy-next-line (direction)
  "Move to a following line, selected using Avy.
When both linewise selection is active (via `x') and selection expansion
is enabled (via `v'), the selection will expand linewise to include all lines
to the chosen one."
  :multiple-cursors nil
  (interactive "p")
  (cl-callf hel-sign direction)
  (when-let* ((pos (save-excursion
                     (let ((goal-column (window-hscroll)))
                       (-> (lambda () (interactive) (line-move direction))
                           (hel-collect-positions)
                           (avy-process)))))
              ((natnump pos)))
    (hel-delete-all-fake-cursors)
    (hel-push-point)
    (if hel--extend-selection
        (let ((lines? (hel-linewise-selection-p)))
          (hel-set-region (mark) pos)
          (if lines? (hel-expand-selection-to-full-lines)))
      ;; else
      (deactivate-mark)
      (let ((column (current-column)))
        (goto-char pos)
        (move-to-column column)))))

;; gk
(hel-define-command hel-avy-previous-line (direction)
  "Move to a preceding line, selected using Avy.
When both linewise selection is active (via `x') and selection expansion
is enabled (via `v'), the selection will expand linewise to include all lines
to the chosen one."
  :multiple-cursors nil
  (interactive "p")
  (hel-avy-next-line (- direction)))

;;; Changes

;; i
(hel-define-command hel-insert ()
  "Switch to Insert state before region."
  :multiple-cursors nil
  (interactive "*")
  (when (use-region-p)
    (hel-with-each-cursor
      (hel-ensure-region-direction -1)))
  (hel-insert-state 1))

;; a
(hel-define-command hel-append ()
  "Switch to Insert state after region."
  :multiple-cursors nil
  (interactive "*")
  (when (use-region-p)
    (hel-with-each-cursor
      (hel-ensure-region-direction 1)
      (when (hel-linewise-selection-p)
        (backward-char))))
  (hel-insert-state 1))

;; I
(hel-define-command hel-insert-line ()
  "Switch to insert state at beginning of current line."
  :multiple-cursors nil
  (interactive "*")
  (hel--insert-or-append-on-line -1))

;; A
(hel-define-command hel-append-line ()
  "Switch to Insert state at the end of the current line."
  :multiple-cursors nil
  (interactive "*")
  (hel--insert-or-append-on-line 1))

(defun hel--insert-or-append-on-line (direction)
  "Switch to insert state at beginning or end of current line
depending on DIRECTION."
  ;; Remain only one cursor on each line.
  (when hel-multiple-cursors-mode
    (hel-with-real-cursor-as-fake
      ;; Line numbers start from 1, so 0 as initial value is out of scope.
      (let ((current-line 0))
        (-each (hel-all-fake-cursors :sort)
          (lambda (cursor)
            (let ((line (line-number-at-pos (overlay-get cursor 'point))))
              (if (= line current-line)
                  (hel--delete-fake-cursor cursor)
                (setq current-line line))))))))
  (hel-with-each-cursor
    (if (< direction 0)
        (hel-first-non-blank)
      (hel-end-of-line))
    (set-marker (mark-marker) (point)))
  (hel-insert-state 1))

;; o
(hel-define-command hel-open-below ()
  "Open new line below selection."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    (when (use-region-p) (hel-ensure-region-direction 1))
    (if (hel-linewise-selection-p)
        (backward-char)
      (move-end-of-line nil))
    (newline-and-indent)
    (set-marker (mark-marker) (point)))
  (hel-insert-state 1))

;; O
(hel-define-command hel-open-above ()
  "Open new line above selection."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    (when (use-region-p) (hel-ensure-region-direction -1))
    (move-beginning-of-line nil)
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (set-marker (mark-marker) (point)))
  (hel-insert-state 1))

;; ] SPC
(hel-define-command hel-add-blank-line-below (count)
  "Add COUNT blank lines below selection."
  :multiple-cursors t
  (interactive "p")
  (hel-save-region
    (when (use-region-p)
      (hel-ensure-region-direction 1))
    (unless (hel-linewise-selection-p 1)
      (hel--forward-line 1))
    (newline count)))

;; [ SPC
(hel-define-command hel-add-blank-line-above (count)
  "Add COUNT blank lines above selection."
  :multiple-cursors t
  (interactive "p")
  (hel-save-region
    (when (use-region-p)
      (hel-ensure-region-direction -1))
    (hel--beginning-of-line)
    (newline count)))

;; c
(hel-define-command hel-change ()
  "Delete region and switch to Insert state."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    (cond ((use-region-p)
           (let ((logical-lines? (hel-linewise-selection-p))
                 (visual-lines? (hel-visual-lines-p)))
             (kill-region nil nil t)
             (cond (logical-lines?
                    (newline)
                    (backward-char)
                    (indent-according-to-mode))
                   (visual-lines?
                    (insert " ")
                    (backward-char)))))
          ((not (hel-bolp))
           (delete-char -1))
          ((bolp)
           (indent-according-to-mode))))
  (hel-insert-state 1))

;; TODO:
;; - If point is surrounded by (balanced) whitespace and a brace delimiter
;; ({} [] ()), delete a space on either side of the cursor.
;; - If point is at BOL and surrounded by braces on adjacent lines,
;; collapse newlines:
;; {
;; |
;; } => {|}
;; d
(hel-define-command hel-cut (count)
  "Kill (cut) text in selection — i.e. delete it and put in the `kill-ring'.
Without selection delete COUNT characters before point."
  :multiple-cursors t
  :merge-selections t
  (interactive "*p")
  (if (use-region-p)
      (progn
        (kill-region nil nil t)
        (hel-maybe-set-killed-rectangle))
    (delete-char (- count)))
  (hel-extend-selection -1))

;; D
(hel-define-command hel-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
Without selection delete COUNT characters after point."
  :multiple-cursors t
  :merge-selections t
  (interactive "*p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char count))
  (hel-extend-selection -1))

;; C-w in insert state
(hel-define-command hel-delete-backward-word ()
  :multiple-cursors t
  (interactive "*")
  (delete-region (point) (progn
                           (hel-backward-word-start 1)
                           (point))))

;; u
(hel-define-command hel-undo ()
  "Undo."
  :multiple-cursors nil
  (interactive "*")
  ;; Deactivate mark to trigger global undo instead of region undo.
  (deactivate-mark)
  (let ((deactivate-mark nil))
    (undo-only)))

;; U
(hel-define-command hel-redo ()
  "Redo."
  :multiple-cursors nil
  (interactive "*")
  ;; Deactivate mark to trigger global undo instead of region undo.
  (deactivate-mark)
  (let ((deactivate-mark nil))
    (undo-redo)))

;; y
(hel-define-command hel-copy ()
  (format "Copy selection into `kill-ring'.
If there are multiple selections and they are not indetical, copy them to
the `killed-rectangle'. You can paste them later with %s (`hel-paste-after')
or `yank-rectangle'."
          (propertize "M-u p" 'face 'help-key-binding))
  :multiple-cursors nil
  (interactive)
  ;; (unless (use-region-p)
  ;;   (user-error "No active selection"))
  (when (use-region-p)
    (let ((deactivate-mark nil)
          any?)
      (hel-with-each-cursor
        (when (use-region-p)
          (copy-region-as-kill (region-beginning) (region-end))
          (setq any? t))
        (hel-extend-selection -1))
      (when any? (message "Copied into kill-ring")))
    (hel-maybe-set-killed-rectangle)
    (hel-pulse-main-region)))

(defun hel-maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry of each cursor to `killed-rectangle',
unless they all are equal. You can paste them later with `yank-rectangle'."
  (when hel-multiple-cursors-mode
    (let ((entries (hel-with-real-cursor-as-fake
                     (-map (lambda (cursor)
                             (car-safe (overlay-get cursor 'kill-ring)))
                           (hel-all-fake-cursors :sort)))))
      (unless (hel-all-elements-are-the-same-p entries)
        (setq killed-rectangle entries)))))

;; p
(hel-define-command hel-paste-after (arg)
  "Paste after selection.
With \\[universal-argument] paste the last coppied multiple selections from the
`killed-rectangle' instead."
  :multiple-cursors t
  (interactive "*P")
  (pcase arg
    ('(4) (insert-rectangle killed-rectangle))
    (_ (hel-paste #'yank 1))))

;; P
(hel-define-command hel-paste-before ()
  "Paste before selection."
  :multiple-cursors t
  (interactive "*")
  (hel-paste #'yank -1))

;; C-p
(hel-define-command hel-paste-pop (count)
  "Replace just-pasted text with next COUNT element from `kill-ring'."
  :multiple-cursors t
  (interactive "*p")
  (let ((deactivate-mark nil))
    (unless (eq last-command 'yank)
      (setq hel--yank-transform-linewise-selection?
            (and (use-region-p)
                 (hel-linewise-selection-p (hel-region-direction)))))
    (let ((yank-transform-functions (cons #'hel--yank-transform
                                          yank-transform-functions))
          (yank-pop (or (command-remapping 'yank-pop)
                        #'yank-pop)))
      (funcall-interactively yank-pop count))
    (if (and (mark t)
             (/= (point) (mark-marker)))
        (activate-mark)
      (deactivate-mark))))

;; C-n
(hel-define-command hel-paste-undo-pop (count)
  "Replace just-pasted text with previous COUNT element from `kill-ring'."
  :multiple-cursors t
  (interactive "*p")
  (hel-paste-pop (- count)))

;; R
(hel-define-command hel-replace-with-kill-ring ()
  "Replace selection content with yanked text from `kill-ring'."
  :multiple-cursors t
  (interactive "*")
  (when (use-region-p)
    (let ((deactivate-mark nil)
          (dir (hel-region-direction)))
      (setq hel--yank-transform-linewise-selection?
            (hel-linewise-selection-p dir))
      (delete-region (region-beginning) (region-end))
      (cl-letf ((yank-transform-functions (cons #'hel--yank-transform
                                                yank-transform-functions))
                ((symbol-function 'push-mark) #'hel-push-mark))
        (yank))
      (hel-set-region (mark t) (point) dir)
      (hel-extend-selection -1)
      (when (and (derived-mode-p 'prog-mode)
                 (use-region-p))
        (indent-region (region-beginning) (region-end))))))

;; J
(hel-define-command hel-join-line ()
  "Join the selected lines."
  :multiple-cursors t
  (interactive "*")
  (hel-save-region
    (let* ((deactivate-mark nil)
           (region? (use-region-p))
           (count (let ((n (if region?
                               (count-lines (region-beginning) (region-end))
                             1)))
                    (if (> n 1) (1- n) n))))
      (when region? (goto-char (region-beginning)))
      ;; All these `let' bindings moves point.
      (let ((in-comment? (progn (move-beginning-of-line nil)
                                (skip-chars-forward " \t")
                                (hel-comment-at-pos-p (point))))
            (ubeg (progn (forward-line 1)
                         (line-beginning-position)))
            (uend (progn (forward-line (1- count))
                         (line-end-position))))
        (when in-comment?
          (uncomment-region ubeg uend)))
      (dotimes (_ count)
        (forward-line 0)
        (delete-char -1)
        (fixup-whitespace)))))

;; <
(hel-define-command hel-indent-left (count)
  :multiple-cursors t
  (interactive "p")
  (cl-assert (/= count 0))
  (hel-indent #'indent-rigidly-left count))

;; >
(hel-define-command hel-indent-right (count)
  :multiple-cursors t
  (interactive "p")
  (cl-assert (/= count 0))
  (hel-indent #'indent-rigidly-right count))

;; ~
(hel-define-command hel-invert-case ()
  "Invert case of characters."
  :multiple-cursors t
  (interactive "*")
  (if-let ((region (hel-region)))
      (-let (((beg end) region)
             (deactivate-mark nil))
        (hel-invert-case-in-region beg end)
        (apply #'hel-set-region region))
    ;; else
    (hel-invert-case-in-region (point) (1+ (point)))))

;; ` or gu
(hel-define-command hel-downcase ()
  "Convert text in selection to lower case.
With no selection downcase the character after point."
  :multiple-cursors t
  (interactive "*")
  (if (use-region-p)
      (let ((deactivate-mark nil))
        (downcase-region (region-beginning) (region-end)))
    (downcase-region (point) (progn (forward-char) (point)))))

;; M-` or gU
(hel-define-command hel-upcase ()
  "Convert text in selection to upper case.
With no selection upcase the character after point."
  :multiple-cursors t
  (interactive "*")
  (if (use-region-p)
      (let ((deactivate-mark nil))
        (upcase-region (region-beginning) (region-end)))
    (upcase-region (point) (progn (forward-char) (point)))))

;;; Selections

;; M-;
(hel-define-command hel-exchange-point-and-mark ()
  "Exchange point and mark."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (hel--exchange-point-and-mark)
    (when (and (not hel-executing-command-for-fake-cursor)
               (called-interactively-p 'any))
      (hel-reveal-point-when-on-top))))

;; v
(hel-define-command hel-extend-selection (arg)
  "Enable or disable extending selections.
If ARG positive number — enable, negative — disable.
When called interactively — toggle extending selection."
  :multiple-cursors t
  (interactive (list (if hel--extend-selection -1 1)))
  (setq hel--extend-selection (/= arg -1))
  (when (and hel--extend-selection
             (not (region-active-p)))
    (set-mark (point)))
  (unless hel-executing-command-for-fake-cursor
    (hel-update-cursor)))

;; ;
(hel-define-command hel-collapse-selection ()
  "Deactivate selection."
  :multiple-cursors t
  (interactive)
  (if hel--extend-selection
      (set-mark (point))
    (deactivate-mark)))

;; x
(hel-define-command hel-expand-line-selection (count)
  "Expand or contract current selection linewise downward COUNT times."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((motion-dir (hel-sign count)))
    (and (hel-expand-selection-to-full-lines motion-dir)
         (cl-callf - count motion-dir))
    (unless (zerop count)
      (let* ((region-dir (hel-region-direction))
             (end (progn (forward-thing 'hel-line count)
                         (when (= (point) (mark-marker))
                           (forward-thing 'hel-line motion-dir))
                         (point)))
             (start (if (/= region-dir (hel-region-direction))
                        (save-excursion
                          (goto-char (mark-marker))
                          (forward-thing 'hel-line (- motion-dir))
                          (point))
                      (mark))))
        (hel-set-region start end)))
    (when (= 1 (count-lines (region-beginning) (region-end) t))
      (hel-ensure-region-direction -1))
    (setq disable-point-adjustment t)))

;; X
(hel-define-command hel-expand-line-selection-backward (count)
  "Expand or contract current selection linewise upward COUNT times."
  :multiple-cursors t
  (interactive "p")
  (hel-expand-line-selection (- count)))

;; %
(hel-define-command hel-mark-whole-buffer ()
  :multiple-cursors nil
  (interactive)
  (hel-delete-all-fake-cursors)
  (hel-push-point)
  ;; `minibuffer-prompt-end'is really `point-min' in most cases, but if we're
  ;; in the minibuffer, this is at the end of the prompt.
  (hel-set-region (minibuffer-prompt-end) (point-max) -1))

;; s
(hel-define-command hel-select-regex (&optional invert)
  "Create new selections for all matches to the regexp entered withing current
selections.

If INVERT is non-nil — create new selections for all regions that NOT match to
entered regexp withing current selections."
  :multiple-cursors nil
  (interactive)
  (when (region-active-p)
    (hel-with-real-cursor-as-fake
      (let* ((cursors (hel-all-fake-cursors))
             (ranges (-map (lambda (cursor)
                             (if (overlay-get cursor 'mark-active)
                                 (let ((point (marker-position
                                               (overlay-get cursor 'point)))
                                       (mark (marker-position
                                              (overlay-get cursor 'mark))))
                                   (if (< point mark)
                                       (cons point mark)
                                     (cons mark point)))))
                           cursors)))
        (-each cursors #'hel-hide-fake-cursor)
        (if (hel-select-interactively-in ranges invert)
            (-each cursors #'hel--delete-fake-cursor)
          ;; Restore original cursors
          (-each cursors #'hel-show-fake-cursor))))))

;; S
(hel-define-command hel-split-region ()
  "Split each selection according to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (hel-select-regex t))

;; M-s
(hel-define-command hel-split-region-on-newline ()
  "Split selections on line boundaries."
  :multiple-cursors nil
  (interactive)
  (hel-with-each-cursor
    (hel-extend-selection -1)
    (when (use-region-p)
      (let ((end (region-end)))
        (hel-ensure-region-direction 1)
        (goto-char (mark-marker))
        (catch 'done
          (let (border)
            (while t
              (hel-end-of-line)
              (when (<= end (point))
                (goto-char end)
                (throw 'done nil))
              (setq border (point))
              (forward-char)
              (when (<= end (point))
                (goto-char border)
                (throw 'done nil))
              (hel-create-fake-cursor border (mark))
              (set-marker (mark-marker) (point)))))))))

;; K
(hel-define-command hel-keep-selections ()
  "Keep selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (hel-filter-selections))

;; M-K
(hel-define-command hel-remove-selections ()
  "Remove selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (hel-filter-selections t))

;; _
(hel-define-command hel-trim-whitespaces-from-selection ()
  "Trim whitespaces and newlines from the both ends of selections."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (let* ((dir (hel-region-direction))
           (point (progn (hel-skip-chars " \t\r\n" (- dir))
                         (point)))
           (mark  (progn (goto-char (mark-marker))
                         (hel-skip-chars " \t\r\n" dir)
                         (point))))
      (hel-set-region mark point))))

;; &
(hel-define-command hel-align-selections ()
  "Align selections by column."
  :multiple-cursors nil
  (interactive)
  (hel-with-real-cursor-as-fake
    (hel-save-window-scroll
      (dolist (cursors (->> (hel-all-fake-cursors :sort)
                            ;; split cursors into groups by line
                            (-partition-by (lambda (cursor)
                                             (-> (overlay-get cursor 'point)
                                                 (line-number-at-pos))))
                            ;; Transpose columns and rows to align all first
                            ;; cursors in each line, than all second and so on.
                            (hel-transpose)))
        (let ((column (-reduce-from (lambda (column cursor)
                                      (goto-char (overlay-get cursor 'point))
                                      (max column (current-column)))
                                    0 cursors)))
          (dolist (cursor cursors)
            (hel-with-fake-cursor cursor
              (unless (= (current-column) column)
                (let ((deactivate-mark nil)
                      (padding (make-string (- column (current-column)) ?\s)))
                  (if (and (use-region-p)
                           (natnump (hel-region-direction)))
                      (progn
                        (hel--exchange-point-and-mark)
                        (insert padding)
                        (hel--exchange-point-and-mark))
                    ;; else
                    (insert padding)))))))))))

;; C
(hel-define-command hel-copy-selection (count)
  "Copy selections COUNT times down if COUNT is positive, or up if negative."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-with-each-cursor
    (hel-motion-loop (dir count)
      (if (use-region-p)
          (hel--copy-region dir)
        (hel--copy-cursor dir)))))

;; M-c
(hel-define-command hel-copy-selection-up (count)
  "Copy each selection COUNT times up."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-copy-selection (- count)))

(defun hel--copy-cursor (direction)
  "Copy point toward the DIRECTION."
  (when-let* ((pos (save-excursion
                     (cl-loop with column = (current-column)
                              while (zerop (forward-line direction))
                              when (= (move-to-column column) column)
                              return (point))))
              ((not (hel-fake-cursor-at pos))))
    (unless (hel-fake-cursor-at (point))
      (hel-create-fake-cursor-from-point))
    (deactivate-mark)
    (goto-char pos)))

(defun hel--copy-region (direction)
  "Copy region toward the DIRECTION."
  (-let* (((beg end region-dir) (hel-region))
          (num-of-lines (count-lines beg end))
          (beg-column (save-excursion (goto-char beg) (current-column)))
          (end-column (save-excursion (goto-char end) (current-column))))
    (when-let* ((bounds (save-excursion
                          (goto-char (if (< direction 0) beg end))
                          (hel--bounds-of-following-region
                           beg-column end-column num-of-lines direction))))
      (let (point mark)
        (if (< region-dir 0)
            (-setq (point . mark) bounds)
          (-setq (mark . point) bounds))
        (if-let* ((cursor (hel-fake-cursor-at point))
                  ((= mark (overlay-get cursor 'mark))))
            nil ;; Do nothing — fake cursor is already at desired position.
          ;; else
          (hel-create-fake-cursor-from-point)
          (goto-char point)
          (set-marker (mark-marker) mark))))))

(defun hel--bounds-of-following-region ( start-column end-column
                                         number-of-lines direction)
  "Return bounds of following region toward the DIRECTION that starts
at START-COLUMN, ends at END-COLUMN and consists of NUMBER-OF-LINES."
  (when (< direction 0)
    (cl-rotatef start-column end-column))
  (let (start end)
    (cl-block nil
      (while (not (and start end))
        (unless (zerop (forward-line direction))
          (cl-return))
        (when (eql (move-to-column start-column)
                   start-column)
          (setq start (point))
          (unless (zerop (forward-line (* (1- number-of-lines)
                                          direction)))
            (cl-return))
          (when (eql (move-to-column end-column)
                     end-column)
            (setq end (point))))))
    (if (and start end)
        (if (< 0 direction)
            (cons start end)
          (cons end start)))))

;; ,
(hel-define-command hel-delete-all-fake-cursors ()
  (format "Delete all fake cursors from current buffer.
You may restore them with %s (`hel-restore-cursors')."
          (propertize "g v" 'face 'help-key-binding))
  (interactive)
  (when hel-multiple-cursors-mode
    (setq hel--cursors-positions-history (hel-cursors-positions))
    (hel-multiple-cursors-mode -1)))

;; M-,
(hel-define-command hel-remove-main-cursor ()
  "Delete main cursor and activate the next fake one."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (hel-restore-point-from-fake-cursor (or (hel-next-fake-cursor (point))
                                            (hel-first-fake-cursor)))
    (hel-auto-multiple-cursors-mode)))

;; gv
(hel-define-command hel-restore-cursors ()
  "Restore last multiple cursors positions."
  :multiple-cursors nil
  (interactive)
  (when hel--cursors-positions-history
    (hel-position-cursors hel--cursors-positions-history)))

;; M-minus
(hel-define-command hel-merge-selections ()
  "Merge all cursors into single selection."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (let ((beg (let ((cursor (hel-first-fake-cursor)))
                 (min (overlay-get cursor 'point)
                      (overlay-get cursor 'mark)
                      (point)
                      (if (use-region-p) (mark) most-positive-fixnum))))
          (end (let ((cursor (hel-last-fake-cursor)))
                 (max (overlay-get cursor 'point)
                      (overlay-get cursor 'mark)
                      (point)
                      (if (use-region-p) (mark) 0)))))
      (hel-delete-all-fake-cursors)
      (hel-set-region beg end))))

;; )
(hel-define-command hel-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (hel-next-fake-cursor (point))
                          (hel-first-fake-cursor))))
          (hel-create-fake-cursor-from-point)
          (hel-restore-point-from-fake-cursor cursor))))))

;; (
(hel-define-command hel-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (hel-previous-fake-cursor (point))
                          (hel-last-fake-cursor))))
          (hel-create-fake-cursor-from-point)
          (hel-restore-point-from-fake-cursor cursor))))))

;; M-)
(hel-define-command hel-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (hel--rotate-selections-content count))

;; M-(
(hel-define-command hel-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (hel--rotate-selections-content count :backward))

(defun hel--rotate-selections-content (count &optional backward)
  (when (and hel-multiple-cursors-mode
             (use-region-p))
    (let ((dir (hel-region-direction)))
      ;; To correctly rotate the content of adjacent selections, we all
      ;; regions need to have negative direction.  This is due to marker
      ;; insertion type of point and mark markers of fake cursor (see
      ;; `set-marker-insertion-type'). Point-marker insertion type is t,
      ;; mark-marker — nil.  We want beginning of a region to be advanced
      ;; on insertion at its position, and end of a region — not.
      (when (natnump dir)
        (hel-with-each-cursor (hel--exchange-point-and-mark)))
      (hel-with-real-cursor-as-fake
        (let ((cursors (hel-all-fake-cursors :sort)))
          (when backward
            (cl-callf nreverse cursors))
          (dotimes (_ count)
            (hel--rotate-selections-content-1 cursors))))
      ;; Restore original regions direction.
      (unless (eql dir (hel-region-direction))
        (hel-with-each-cursor (hel--exchange-point-and-mark))))))

(defun hel--rotate-selections-content-1 (cursors)
  "Rotate regions content for all CURSORS."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (overlay-get first-cursor 'point)
                                    (overlay-get first-cursor 'mark))))
    (dolist (cursor (cdr cursors))
      (setq content (hel-exchange-fake-region-content cursor content)))
    (hel-exchange-fake-region-content first-cursor content)))

(defun hel-exchange-fake-region-content (cursor content)
  "Exchange the CURSORs region content with CONTENT and return the old one."
  (hel-with-fake-cursor cursor
    (let ((deactivate-mark nil) ;; Do not deactivate mark after insertion.
          (dir (hel-region-direction))
          (new-content (buffer-substring (point) (mark))))
      (delete-region (point) (mark))
      (insert content)
      (hel-ensure-region-direction dir)
      new-content)))

;; (keymap-lookup nil "M-<down-mouse-1>")

;; M-<right-mouse>
(hel-define-command hel-toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  :multiple-cursors nil
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (unless (windowp (posn-window position))
      (error "Position not in text area of window"))
    (select-window (posn-window position))
    (when-let* ((pos (posn-point position))
                ((numberp pos)))
      (if-let* ((cursor (hel-fake-cursor-at pos)))
          (hel-delete-fake-cursor cursor)
        (hel-create-fake-cursor pos)))))

;;;; Mark

(hel-define-command hel-m-digit-argument (arg)
  "Like `digit-argument' but keep `m' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (digit-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

(hel-define-command hel-m-negative-argument (arg)
  "Like `negative-argument' but keep `m' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (negative-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

(hel-define-command hel-mi-digit-argument (arg)
  "Like `digit-argument' but keep `mi' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (digit-argument arg)
  (set-transient-map (keymap-lookup nil "m i")))

(hel-define-command hel-mi-negative-argument (arg)
  "Like `negative-argument' but keep `mi' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (negative-argument arg)
  (set-transient-map (keymap-lookup nil "m i")))

(hel-define-command hel-ma-digit-argument (arg)
  "Like `digit-argument' but keep `ma' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (digit-argument arg)
  (set-transient-map (keymap-lookup nil "m a")))

(hel-define-command hel-ma-negative-argument (arg)
  "Like `negative-argument' but keep `ma' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (negative-argument arg)
  (set-transient-map (keymap-lookup nil "m a")))

;; miw
(hel-define-command hel-mark-inner-word (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'hel-word count))

;; miW
(hel-define-command hel-mark-inner-WORD (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'hel-WORD count))

;; maw
(hel-define-command hel-mark-a-word ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel--mark-a-word 'hel-word))

;; maW
(hel-define-command hel-mark-a-WORD ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel--mark-a-word 'hel-WORD))

;; mis
(hel-define-command hel-mark-inner-sentence (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'hel-sentence count))

;; mas
(hel-define-command hel-mark-a-sentence (&optional thing)
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (or thing (setq thing 'hel-sentence))
  (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
    (-let [(beg . end)
           (or (progn
                 (goto-char thing-end)
                 (with-restriction (line-beginning-position) (line-end-position)
                   (-if-let ((_ . space-end)
                             (hel-bounds-of-complement-of-thing-at-point thing))
                       (cons thing-beg space-end))))
               (progn
                 (goto-char thing-beg)
                 (with-restriction (line-beginning-position) (line-end-position)
                   (-if-let ((space-beg . _)
                             (hel-bounds-of-complement-of-thing-at-point thing))
                       (cons space-beg thing-end))))
               (cons thing-beg thing-end))]
      (hel-set-region beg end))))

;; mip
(hel-define-command hel-mark-inner-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-push-point)
  (hel-mark-inner-thing 'hel-paragraph count)
  (hel-reveal-point-when-on-top))

;; map
(hel-define-command hel-mark-a-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-push-point)
  (hel-mark-a-thing 'hel-paragraph count)
  (hel-reveal-point-when-on-top))

;; mif
(hel-define-command hel-mark-inner-function (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-push-point)
  (hel-mark-inner-thing 'hel-function count)
  (hel-ensure-region-direction -1)
  (hel-reveal-point-when-on-top))

;; maf
(hel-define-command hel-mark-a-function (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-push-point)
  (-let* ((thing 'hel-function)
          ((thing-beg . thing-end) (hel-bounds-of-count-things-at-point thing count))
          beg end)
    (or (-if-let ((_ . space-end)
                  (progn
                    (goto-char thing-end)
                    (hel-bounds-of-complement-of-thing-at-point thing)))
            (setq beg (progn
                        ;; Take comments that belongs to the current function.
                        (goto-char thing-beg)
                        (car (bounds-of-thing-at-point 'hel-paragraph)))
                  end (progn
                        ;; Exclude comments that belongs to the next function.
                        (goto-char space-end)
                        (car (bounds-of-thing-at-point 'hel-paragraph)))))
        (-if-let ((space-beg . _)
                  (progn
                    (goto-char thing-beg)
                    (hel-bounds-of-complement-of-thing-at-point thing)))
            (setq beg space-beg
                  end thing-end))
        (setq beg (progn
                    ;; Take comments that belongs to the current function.
                    (goto-char thing-beg)
                    (car (bounds-of-thing-at-point 'hel-paragraph)))
              end thing-end))
    (hel-set-region beg end (hel-sign count)))
  (hel-reveal-point-when-on-top))

;; mi"
(hel-define-command hel-mark-inner-double-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?\"))
    (hel-set-region (1+ beg) (1- end))))

;; ma"
(hel-define-command hel-mark-a-double-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?\"))
    (hel-set-region beg end)))

;; mi'
(hel-define-command hel-mark-inner-single-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?'))
    (hel-set-region (1+ beg) (1- end))))

;; ma'
(hel-define-command hel-mark-a-single-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?'))
    (hel-set-region beg end)))

;; mi`
(hel-define-command hel-mark-inner-back-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?`))
    (hel-set-region (1+ beg) (1- end))))

;; ma`
(hel-define-command hel-mark-a-back-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?`))
    (hel-set-region beg end)))

;; mi( mi)
(hel-define-command hel-mark-inner-paren ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (hel-4-bounds-of-brackets-at-point ?\( ?\)))
    (hel-set-region beg end)))

;; ma( ma)
(hel-define-command hel-mark-a-paren ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-brackets-at-point ?\( ?\)))
    (hel-set-region beg end)))

;; mi[ mi]
(hel-define-command hel-mark-inner-bracket ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (hel-4-bounds-of-brackets-at-point ?\[ ?\]))
    (hel-set-region beg end)))

;; ma[ ma]
(hel-define-command hel-mark-a-bracket ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-brackets-at-point ?\[ ?\]))
    (hel-set-region beg end)))

;; mi{ mi}
(hel-define-command hel-mark-inner-curly ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (hel-4-bounds-of-brackets-at-point ?{ ?}))
    (hel-set-region beg end)))

;; ma{ ma}
(hel-define-command hel-mark-a-curly ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (hel-bounds-of-brackets-at-point ?{ ?}))
    (hel-set-region beg end)))

;; mi< mi>
(hel-define-command hel-mark-inner-angle ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (hel-4-bounds-of-brackets-at-point ?< ?>))
    (hel-set-region beg end)))

;; ma< ma>
(hel-define-command hel-mark-an-angle ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg _ _ end) (hel-4-bounds-of-brackets-at-point ?< ?>))
    (hel-set-region beg end)))

(hel-define-command hel-mark-inner-surround ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (hel-surround--remove char)))
    (-let [(_ beg end _) bounds]
      (hel-set-region beg end))))

(hel-define-command hel-mark-a-surround ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (hel-surround--remove char)))
    (-let [(beg _ _ end) bounds]
      (hel-set-region beg end))))

;;; Search

;; f
(hel-define-command hel-find-char-forward (count)
  "Prompt user for CHAR and move to the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (let ((char (read-char "f" t)))
    (hel-maybe-set-mark)
    (hel-motion-loop (dir count)
      (hel-find-char char dir nil))))

;; F
(hel-define-command hel-find-char-backward (count)
  "Prompt user for CHAR and move to the previous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (cl-callf - count)
  (let ((char (read-char "F" t)))
    (hel-maybe-set-mark)
    (hel-motion-loop (dir count)
      (hel-find-char char dir nil))))

;; t
(hel-define-command hel-till-char-forward (count)
  "Prompt user for CHAR and move before the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (let ((char (read-char "t" t)))
    (hel-maybe-set-mark)
    (hel-motion-loop (dir count)
      (hel-find-char char dir t))))

;; T
(hel-define-command hel-till-char-backward (count)
  "Prompt user for CHAR and move before the prevous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive "p")
  (cl-callf - count)
  (let ((char (read-char "T" t)))
    (hel-maybe-set-mark)
    (hel-motion-loop (dir count)
      (hel-find-char char dir t))))

;; /
(hel-define-command hel-search-forward (count)
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when (hel-search-interactively)
    (setq hel-search--direction 1)
    (hel-search-next count)))

;; ?
(hel-define-command hel-search-backward (count)
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when (hel-search-interactively -1)
    (setq hel-search--direction -1)
    (hel-search-next count)))

;; n
(hel-define-command hel-search-next (count)
  "Select next COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (unless hel-search--direction (setq hel-search--direction 1))
  (when (< hel-search--direction 0)
    (cl-callf - count))
  (let ((regexp (hel-search-pattern))
        (region-dir (if (use-region-p) (hel-region-direction) 1)))
    (hel-recenter-point-on-jump
      (hel-motion-loop (search-dir count)
        (-when-let ((beg . end) (save-excursion
                                  (helf-search--search regexp search-dir)))
          ;; Push mark on first invocation.
          (unless (or (memq last-command '(hel-search-next hel-search-previous))
                      (hel-search--keep-highlight-p last-command))
            (hel-push-point))
          (when (and hel--extend-selection (use-region-p))
            (hel-create-fake-cursor-from-point))
          (hel-set-region beg end region-dir))))
    (hel-highlight-search-pattern regexp)))

;; N
(hel-define-command hel-search-previous (count)
  "Select previous COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-search-next (- count)))

;; *
(hel-define-command hel-construct-search-pattern ()
  "Construct search pattern from all current selections and store it to / register.
Auto-detect word boundaries at the beginning and end of the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote-fn (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (open-word-boundary
                (cond ((eql beg (pos-bol))
                       (->> (buffer-substring-no-properties beg (1+ beg))
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- beg) (1+ beg))
                            (string-match-p "[^[:word:]][[:word:]]")))))
               (close-word-boundary
                (cond ((eql end (pos-eol))
                       (->> (buffer-substring-no-properties (1- end) end)
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- end) (1+ end))
                            (string-match-p "[[:word:]][^[:word:]]")))))
               (string (->> (buffer-substring-no-properties (point) (mark))
                            (funcall quote-fn))))
          (push (concat (if open-word-boundary "\\b")
                        string
                        (if close-word-boundary "\\b"))
                patterns))))
    (setq patterns (nreverse (-uniq patterns)))
    (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
           (regexp (apply #'concat (-interpose separator patterns))))
      (hel-add-to-regex-history regexp)
      (hel-highlight-search-pattern regexp))))

;; M-*
(hel-define-command hel-construct-search-pattern-no-bounds ()
  "Construct search pattern from all current selection and store it to / register.
Do not auto-detect word boundaries in the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (push (funcall quote (buffer-substring-no-properties (point) (mark)))
              patterns)))
    (cl-callf nreverse patterns)
    (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
           (regexp (apply #'concat (-interpose separator patterns))))
      (hel-add-to-regex-history regexp)
      (hel-highlight-search-pattern regexp))))

;;; Surround

;; ms
(hel-define-command hel-surround ()
  "Enclose the active region with the chosen delimiters.

Left brackets — `(', `[', `{' — add extra spaces around the surrounded
region. If the selection is linewise (made with \"x\"), the delimiters
are placed on separate lines.

Right brackets — `)', `]', `}' — do the opposite: they remove all
spaces or newlines from the selection and are inserted tightly.

For custom delimiters, the general rule is: if either delimiter
contains a newline character, they are inserted on separate lines."
  :multiple-cursors t
  (interactive)
  (when-let* (((use-region-p))
              (key (read-char "Surround: " t)))
    (-let* (((left . right) (hel-surround--insert key))
            (linewise? (or (string-search "\n" left)
                           (string-search "\n" right))))
      (if linewise?
          (progn
            (cl-callf string-trim left)
            (cl-callf string-trim right))
        (hel-trim-whitespaces-from-selection))
      (let ((beg (copy-marker (region-beginning)))
            (end (copy-marker (region-end) t)))
        (hel-save-region
          (goto-char beg)
          (insert left)
          (when linewise? (newline))
          (goto-char end)
          (insert right)
          (when linewise? (newline))
          (indent-region beg end)
          (set-marker beg nil)
          (set-marker end nil))))
    (hel-extend-selection -1)))

;; md
(hel-define-command hel-surround-delete ()
  "Delete surround."
  :multiple-cursors t
  (interactive)
  (when-let* ((key (read-char "Delete pair: " t))
              (bounds (hel-surround--remove key)))
    (-let (((left-beg left-end right-beg right-end) bounds)
           (deactivate-mark nil))
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

;; mr
(hel-define-command hel-surround-change ()
  "Change surround."
  :multiple-cursors t
  (interactive)
  (when-let* ((remove-key (read-char "Delete pair: " t))
              (insert-key (read-char "Insert pair: " t))
              (4-bounds (hel-surround--remove remove-key)))
    (-let (((left-beg left-end right-beg right-end) 4-bounds)
           ((left . right) (hel-surround--insert insert-key))
           (deactivate-mark nil))
      (hel-save-region
        (delete-region right-beg right-end)
        (goto-char right-beg)
        (insert right)
        (delete-region left-beg left-end)
        (goto-char left-beg)
        (insert left)))))

;;; Window navigation

(hel-define-command hel-window-split (window-to-split)
  "Split current window horisontally.
All children of the parent of the splitted window will be rebalanced."
  :multiple-cursors nil
  (interactive `(,(selected-window)))
  (let ((new-window (split-window-below nil window-to-split)))
    (select-window new-window)
    (balance-windows (window-parent new-window))
    new-window))

(hel-define-command hel-root-window-split ()
  "Split root window of current frame horisontally and rebalance all windows."
  :multiple-cursors nil
  (interactive)
  (hel-window-split (frame-root-window)))

(hel-define-command hel-window-vsplit (window-to-split)
  "Split the current window vertically.
All children of the parent of the splitted window will be rebalanced."
  :multiple-cursors nil
  (interactive `(,(selected-window)))
  (let ((new-window (split-window-right nil window-to-split)))
    (select-window new-window)
    (balance-windows (window-parent new-window))
    new-window))

(hel-define-command hel-root-window-vsplit ()
  "Split root window of current frame vertically and rebalance all windows."
  :multiple-cursors nil
  (interactive)
  (hel-window-vsplit (frame-root-window)))

(defmacro hel-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (cl-with-gensyms (sides)
    `(let ((,sides (window-with-parameter 'window-side)))
       (when ,sides (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides (window-toggle-side-windows))))))

(defun hel-move-window (side)
  "SIDE has the same meaning as in `split-window'."
  (hel-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(hel-define-command hel-move-window-left ()
  "Swap window with one to the left."
  :multiple-cursors nil
  (interactive)
  (hel-move-window 'left))

(hel-define-command hel-move-window-right ()
  "Swap window with one to the right."
  :multiple-cursors nil
  (interactive)
  (hel-move-window 'right))

(hel-define-command hel-move-window-up ()
  "Swap window with one upwards."
  :multiple-cursors nil
  (interactive)
  (hel-move-window 'up))

(hel-define-command hel-move-window-down ()
  "Swap window with one downwards."
  :multiple-cursors nil
  (interactive)
  (hel-move-window 'down))

;; C-w c
(hel-define-command hel-window-delete ()
  "Delete the current window or tab.
Rebalance all children of the deleted window's parent window.
If `tab-bar-mode' is enabled and the window is the only window on the tab —
close the tab."
  :multiple-cursors nil
  (interactive)
  (let ((parent (window-parent)))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null parent))
        (tab-close)
      (delete-window)
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (ignore-errors (balance-windows parent)))))

(hel-define-command hel-clone-indirect-buffer-same-window ()
  "Create indirect buffer and open it in the current window.
This command was written because `clone-indirect-buffer' calls `pop-to-buffer'
and opens a new window."
  :multiple-cursors nil
  (interactive)
  (-doto (clone-indirect-buffer nil nil)
    (switch-to-buffer)))

;; C-w q
(hel-define-command hel-kill-current-buffer-and-window ()
  "Kill current buffer and close the window.
If `tab-bar-mode' is enabled and the window is the only window on the tab —
close the tab."
  :multiple-cursors nil
  (interactive)
  (let ((parent-win (window-parent)))
    (kill-buffer (current-buffer))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null parent-win))
        (tab-close)
      ;; else
      (delete-window)
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (ignore-errors (balance-windows parent-win)))))

;; zn
(hel-define-command hel-narrow-to-region-indirectly ()
  "Restrict editing in this buffer to the current region, indirectly.
This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`hel-widen-indirectly-narrowed' to undo it (incrementally)."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (let ((orig-buffer (current-buffer))
          (name (or buffer-file-name
                    list-buffers-directory))
          (beg (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (hel-clone-indirect-buffer-same-window)
      (setq list-buffers-directory name
            hel--narrowed-base-buffer orig-buffer)
      (narrow-to-region beg end))))

;; zw
(hel-define-command hel-widen-indirectly-narrowed (&optional arg)
  "Widens narrowed buffers.
Incrementally kill indirect buffers (under the assumption they were created by
`hel-narrow-to-region-indirectly') and switch to their base buffer.

With \\[universal-argument] undone all recursive levels of inderect narrowing.

If the current buffer is not an indirect buffer, works like `widen'."
  :multiple-cursors nil
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer hel--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (-let (((base-buffer . narrowed-buffers)
                   (->> orig-buffer
                        (-unfold (lambda (buffer)
                                   (when buffer
                                     (cons buffer (buffer-local-value
                                                   'hel--narrowed-base-buffer
                                                   buffer)))))
                        (nreverse))))
             (switch-to-buffer base-buffer)
             (-each narrowed-buffers #'kill-buffer)))
          (t
           (switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

;; C-w :
(hel-define-command hel-execute-extended-command-other-window ()
  :multiple-cursors nil
  (interactive)
  (other-window-prefix)
  (call-interactively #'execute-extended-command))

;; C-w C-:
(hel-define-command hel-execute-extended-command-for-buffer-other-window ()
  :multiple-cursors nil
  (interactive)
  (other-window-prefix)
  (call-interactively #'execute-extended-command-for-buffer))

;;; .
(provide 'hel-commands)
;;; hel-commands.el ends here
