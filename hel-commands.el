;;; hel-commands.el --- Interactive commands -*- lexical-binding: t -*-
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
;; Interactive editing commands: motions, changes, selections, text objects,
;; surround. Window navigation commands.
;;
;; The word motions and the text objects choose the thing and the parameters
;; of an algorithm in `hel-text-objects.el'. The node commands call the
;; algorithms over trees in `hel-structural-editing.el'.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'pcre2el)
(require 'thingatpt)
(require 'rect)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-cursors)
(require 'hel-core)
(require 'hel-search)
(require 'hel-text-objects)
(require 'hel-structural-editing)
(require 'hel-surround)
(require 'avy)

;; ESC in normal state
(hel-define-command hel-normal-state-escape ()
  (format "Command for ESC key in Hel Normal state.
Execute in order:
- Disable extending selection (enabled with %s command);
- Collaps the selection to the cursor;
- Disable search results highlighting."
          (propertize "v" 'face 'help-key-binding))
  :multiple-cursors t
  (interactive)
  (if hel-extend-selection
      (hel-extend-selection -1)
    (hel-collapse-selection)))

;;; Motions

;; h
(hel-define-command hel-backward-char (count &optional extend)
  "Move the cursor character backward COUNT characters.
When extending the selection is on, the selection keeps the character at
its other end."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-forward-char (- count) extend))

;; l
(hel-define-command hel-forward-char (count &optional extend)
  "Move the cursor character forward COUNT characters.
Move backward if COUNT is negative.  When extending the selection is on,
the selection keeps the character at its other end.  Without extending,
a step past the last character leaves the empty selection at the end of
the buffer."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (let ((dir (hel-sign count)))
    (dotimes (_ (abs count))
      (-let* (((selection &as (begin . end)) (hel-selection))
              (char (+ (hel-selection-head selection) dir)))
        (cond ((< char (point-min))
               (signal 'beginning-of-buffer nil))
              ((< char (point-max))
               (hel-move-onto-char char extend))
              ((or extend (= begin end))
               (signal 'end-of-buffer nil))
              (t
               (hel-set-cursor (point-max))))))))

;; j
(hel-define-command hel-next-line (count &optional extend)
  "Move the cursor COUNT lines down, or up when COUNT is negative.
An active selection first collapses onto its head character.  When
extending the selection is on, the selection keeps its other end and
extends to the new line.  A selection of whole lines, as `x' makes it,
then grows by whole lines."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (let ((selection (hel-selection)))
    (if (and extend (hel-line-selection-p selection))
        (hel-expand-line-selection count)
      (let ((anchor (and extend
                         (hel-selection-anchor selection (hel-sign count)))))
        (hel-set-cursor (hel-selection-head selection))
        ;; Emacs keeps the column if and only if the previous command was
        ;; `next-line' or `previous-line'.
        (setq this-command (if (natnump count) 'next-line 'previous-line))
        (funcall-interactively 'next-line count)
        (when anchor
          (-let [(begin . end) (hel-char-range (point))]
            (hel-select (cons (min anchor begin) (max anchor end))
                        (if (< (point) anchor) -1 1))))))))

;; k
(hel-define-command hel-previous-line (count &optional extend)
  "Move to the previous COUNT line. Move downward if COUNT is negative.
If both linewise selection (`x' key) and extending selection (`v' key)
are active — works like `hel-expand-line-selection-backward'."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-next-line (- count) extend))

;; w
(hel-define-command hel-forward-word-start (count &optional extend)
  "Move to the COUNT-th next word start."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'word count :begin nil extend))

;; W
(hel-define-command hel-forward-WORD-start (count &optional extend)
  "Move to the COUNT-th next WORD start."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'WORD count :begin nil extend))

;; b
(hel-define-command hel-backward-word-start (count &optional extend)
  "Move to the COUNT-th previous word start."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'word (- count) :begin nil extend))

;; B
(hel-define-command hel-backward-WORD-start (count &optional extend)
  "Move to the COUNT-th previous WORD start."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'WORD (- count) :begin nil extend))

;; e
(hel-define-command hel-forward-word-end (count &optional extend)
  "Move to the COUNT-th next word end."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'word count :end nil extend))

;; E
(hel-define-command hel-forward-WORD-end (count &optional extend)
  "Move COUNT-th next WORD end."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'WORD count :end nil extend))

;; no key in Helix, "ge" in Vim
(hel-define-command hel-backward-word-end (count &optional extend)
  "Move to the COUNT-th previous word end."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'word (- count) :end nil extend))

;; no key in Helix, "gE" in Vim
(hel-define-command hel-backward-WORD-end (count &optional extend)
  "Select from the cursor back to the end of the COUNT-th previous WORD."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'WORD (- count) :end nil extend))

;; gg
(hel-define-command hel-beginning-of-buffer (num &optional extend)
  "Move point to the beginning of the buffer.
With numeric arg NUM, put point NUM/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.
Push mark at previous position, unless extending selection."
  :multiple-cursors nil
  (interactive (list current-prefix-arg hel-extend-selection))
  (hel-disable-multiple-cursors-mode)
  (hel-push-point)
  (let ((pos (if num
                 (save-excursion
                   (goto-char (+ (point-min)
                                 (-> (- (point-max) (point-min))
                                     (* (prefix-numeric-value num))
                                     (/ 10))))
                   (forward-line 1)
                   (point))
               (point-min))))
    (hel-move-to-pos nil pos extend))
  (unless num
    (recenter 0)))

;; G
(hel-define-command hel-end-of-buffer (count &optional extend)
  "Move point to the COUNT line, or to the end of the buffer."
  :multiple-cursors nil
  (interactive (list current-prefix-arg hel-extend-selection))
  (hel-disable-multiple-cursors-mode)
  (hel-push-point)
  (let ((pos (if count
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (prefix-numeric-value count)))
                   (point))
               (point-max))))
    (hel-move-to-pos nil pos extend)))

;; gh
(hel-define-command hel-beginning-of-line-command (&optional extend)
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only hel-beginning-of-line-command))
  (interactive (list hel-extend-selection))
  (hel--select-to-line-pos #'hel-line-begin extend))

;; gs
(hel-define-command hel-first-non-blank (&optional extend)
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (interactive (list hel-extend-selection))
  (hel--select-to-line-pos (lambda (pos)
                             (save-excursion
                               (goto-char (hel-line-begin pos))
                               (hel-skip-blanks 1)
                               (backward-prefix-chars)
                               (point)))
                           extend))

;; gl
(hel-define-command hel-end-of-line-command (&optional extend)
  "Move point to end of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only hel-end-of-line-command))
  (interactive (list hel-extend-selection))
  (let ((end (hel--select-to-line-pos #'hel-line-end extend)))
    ;; "Stick" cursor to the end of line after moving to it. Vertical motions
    ;; right after "gl" will place point at the end of each line. A selection
    ;; of one character leaves point before its character, so the test is on
    ;; END and not on point.
    (when (and (not visual-line-mode)
               (save-excursion (goto-char end) (eolp)))
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))))

(defun hel--select-to-line-pos (target-fun extend)
  "Select from the head character of the selection to a place on its line.
TARGET-FUN is a function that takes the position of the head character and
returns the place. The selection takes the head character in, because the cursor
stands on it, unless a word ends right past it on the way to the place: then
the character stays with its word and the selection starts past it. A selection
that would be empty keeps the head character, so the cursor does not leave it.
With EXTEND, extend the selection as `hel-move-to-pos' does. Return the place."
  (-let* ((head (hel-selection-head (hel-selection)))
          (to   (funcall target-fun head))
          (dir  (if (< head to) 1 -1))
          ((head-range &as begin . end) (hel-char-range head))
          (near (if (< 0 dir) begin end))
          (far  (if (< 0 dir) end begin))
          (producer (hel-thing-producer 'word head))
          (from (if (and (/= far to)
                         ;; The motion over words to either edge stops right
                         ;; past the head character.
                         (or (equal (hel-thing-motion-range 'word producer near dir :begin nil)
                                    head-range)
                             (equal (hel-thing-motion-range 'word producer near dir :end nil)
                                    head-range)))
                    far
                  near)))
    (hel-move-to-pos from to extend)
    to))

;; mm
(hel-define-command hel-jump-to-match-item (&optional extend)
  "Move the cursor character to the bracket that matches it.
From an opening bracket go to its closing bracket, and back.  From any
other character go to the closing bracket of the innermost pair around
it.  When extending the selection is on, move the head character of the
selection instead, and keep its anchor character."
  :multiple-cursors t
  :merge-selections hel-extend-selection
  (interactive (list hel-extend-selection))
  (-when-let (char (hel--matching-bracket (hel-selection-head (hel-selection))))
    (hel-move-onto-char char extend))
  (hel-reveal-point-when-on-top))

(defun hel--matching-bracket (char)
  "Return the bracket that matches the bracket at character CHAR.
At any other character, return the closing bracket of the innermost
pair around CHAR.  Return nil when there is no such bracket.  The
syntax table decides what a bracket is."
  (ignore-errors
    (pcase (syntax-class (syntax-after char))
      (4 (1- (scan-lists char 1 0)))
      (5 (scan-lists (1+ char) -1 0))
      (_ (1- (scan-lists char 1 1))))))

;; C-s
(hel-define-command hel-save-point-to-mark-ring ()
  "Store main cursor position to `mark-ring'."
  :multiple-cursors nil
  (interactive)
  (hel-push-point))

;; C-o
(hel-define-command hel-backward-mark-ring (&optional extend)
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
  (interactive (list hel-extend-selection))
  (hel--jump-over-mark-ring nil extend))

;; C-i
(hel-define-command hel-forward-mark-ring (&optional extend)
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' backward and jump to new top
position. See `hel-backward-mark-ring'.

\(Does not affect global mark ring)."
  :multiple-cursors t
  (interactive (list hel-extend-selection))
  (hel--jump-over-mark-ring t extend))

;; C-S-o
(hel-define-command hel-backward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
forward (like revolver cylinder) and jump to new top location."
  :multiple-cursors nil
  (interactive)
  (hel--jump-over-global-mark-ring))

;; C-S-i
(hel-define-command hel-forward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
backward and jump to new top location."
  :multiple-cursors nil
  (interactive)
  (hel--jump-over-global-mark-ring t))

;;; Avy (Easymotion)

;; gw
(hel-define-command hel-avy-word-forward (&optional extend)
  "Move to a word start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive (list hel-extend-selection))
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (point) (window-end nil t))
                (avy-process)))
      (hel-disable-multiple-cursors-mode)
      (hel-push-point orig-point)
      (hel-move-to-pos (point) (progn
                                 (hel-word-motion 1)
                                 (point))
                       extend))))

;; gb
(hel-define-command hel-avy-word-backward (&optional extend)
  "Move to a word start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive (list hel-extend-selection))
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (window-start) (point))
                (nreverse)
                (avy-process)))
      (hel-disable-multiple-cursors-mode)
      (hel-push-point orig-point)
      (if extend
          (hel-move-to-pos nil (point) t)
        (hel-select (cons (point) (progn
                                    (hel-word-motion 1)
                                    (point))))))))

;; gW
(hel-define-command hel-avy-WORD-forward (&optional extend)
  "Move to a WORD start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive (list hel-extend-selection))
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (point) (window-end nil t))
                (avy-process)))
      (hel-disable-multiple-cursors-mode)
      (hel-push-point orig-point)
      (hel-move-to-pos (point) (progn
                                 (hel-WORD-motion 1)
                                 (point))
                       extend))))

;; gB
(hel-define-command hel-avy-WORD-backward (&optional extend)
  "Move to a WORD start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive (list hel-extend-selection))
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (window-start) (point))
                (nreverse)
                (avy-process)))
      (hel-disable-multiple-cursors-mode)
      (hel-push-point orig-point)
      (if extend
          (hel-move-to-pos nil (point) t)
        (hel-select (cons (point) (progn
                                    (hel-WORD-motion 1)
                                    (point))))))))

;; gj
(hel-define-command hel-avy-next-line (direction &optional extend)
  "Move to a following line, selected using Avy.
When both linewise selection is active (via `x') and selection expansion
is enabled (via `v'), the selection will expand linewise to include all lines
to the chosen one."
  :multiple-cursors nil
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (cl-callf hel-sign direction)
  (when-let* ((pos (save-excursion
                     (let ((goal-column (window-hscroll)))
                       (-> (lambda () (interactive) (line-move direction))
                           (hel-collect-positions)
                           (avy-process)))))
              ((natnump pos)))
    (hel-disable-multiple-cursors-mode)
    (hel-push-point)
    (if extend
        (let ((lines? (hel-line-selection-p)))
          (hel-move-to-pos nil pos t)
          (when lines?
            (hel-expand-selection-to-full-lines)
            (-let [((begin . end) dir) (hel-selection)]
              (hel-select (cons begin (hel--add-final-newline end))
                          (or dir 1) t))))
      ;; else
      (let ((column (save-excursion
                      (goto-char (hel-selection-head (hel-selection)))
                      (current-column))))
        (hel-set-cursor pos)
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
  "Switch to Insert state at the beginning of the selection."
  :multiple-cursors nil
  (interactive "*")
  (setq hel--selections-before-insert (hel-selections))
  (hel-with-each-cursor
    (goto-char (car (hel-selection-bounds))))
  (hel-insert-state 1))

;; a
(hel-define-command hel-append ()
  "Switch to Insert state at the end of the selection.
When the selection ends with a newline, point goes before it, so the
typing stays on the last line of the selection."
  :multiple-cursors nil
  (interactive "*")
  (setq hel--selections-before-insert (hel-selections))
  (hel-with-each-cursor
    (-let [(begin . end) (hel-selection-bounds)]
      (goto-char end)
      (when (and (< begin end) (eq ?\n (char-before)))
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
  (setq hel--selections-before-insert (hel-selections))
  ;; Remain only one cursor on each line.
  (when hel-multiple-cursors-mode
    (hel-with-main-cursor-as-secondary
      ;; Lines are identified by their beginning position. Buffer positions
      ;; start from 1, so 0 as initial value is out of scope.
      (let ((current-line 0))
        (-each (hel-all-secondary-cursors :sort)
          (lambda (cursor)
            (let ((line (save-excursion
                          (goto-char (hel--secondary-cursor-point cursor))
                          (line-beginning-position))))
              (if (= line current-line)
                  (hel--delete-secondary-cursor cursor)
                (setq current-line line))))))))
  (hel-with-each-cursor
    (if (< direction 0)
        (hel-first-non-blank)
      (goto-char (hel-line-end))))
  (hel-insert-state 1))

;; o
(hel-define-command hel-open-below ()
  "Open new line below selection."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    ;; The new line goes after the line of the last character of the
    ;; selection.
    (-let [(begin . end) (hel-selection-bounds)]
      (goto-char (max begin (1- end))))
    (move-end-of-line nil)
    (newline-and-indent))
  (hel-insert-state 1))

;; O
(hel-define-command hel-open-above ()
  "Open new line above selection."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    (goto-char (car (hel-selection-bounds)))
    (move-beginning-of-line nil)
    (newline)
    (forward-line -1)
    (indent-according-to-mode))
  (hel-insert-state 1))

;; ] SPC
(hel-define-command hel-add-blank-line-below (count)
  "Add COUNT blank lines below selection."
  :multiple-cursors t
  (interactive "p")
  (hel-save-selection
    ;; The blank lines go after the line of the last character of the
    ;; selection.
    (-let [(begin . end) (hel-selection-bounds)]
      (goto-char (max begin (1- end))))
    (hel-forward-line 1)
    (newline count)))

;; [ SPC
(hel-define-command hel-add-blank-line-above (count)
  "Add COUNT blank lines above selection."
  :multiple-cursors t
  (interactive "p")
  (hel-save-selection
    (goto-char (car (hel-selection-bounds)))
    (hel-beginning-of-visible-line)
    (newline count)))

;; c
(hel-define-command hel-change ()
  "Kill the selection and switch to Insert state in its place.
The character of a bare cursor stays out of the `kill-ring', where
a single character only clutters it.  A selection of
whole lines leaves an empty line, indented by the rules of the mode, and
so does a bare cursor on an empty line."
  :multiple-cursors nil
  (interactive "*")
  (hel-with-each-cursor
    (-let [(selection &as (begin . end)) (hel-selection)]
      (cond ((not (hel-bare-cursor-p selection))
             (let ((logical-lines? (hel-line-selection-p selection))
                   (visual-lines? (hel-visual-line-selection-p)))
               (kill-region nil nil t)
               (cond (logical-lines?
                      (newline)
                      (backward-char)
                      (indent-according-to-mode))
                     (visual-lines?
                      (insert " ")
                      (backward-char)))))
            ((and (bolp) (eolp))
             (indent-according-to-mode))
            ((< begin end)
             (delete-region begin end)))))
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
(hel-define-command hel-cut ()
  "Kill (cut) the selection: delete it and put it in the `kill-ring'.
The cursor stays in place, on the character after the deleted text, as
in Helix and Vim.  A bare cursor is the exception for the `kill-ring':
its character stays out of it, where a single character only clutters it."
  :multiple-cursors t
  :merge-selections t
  (interactive "*")
  (-let [(selection &as (begin . end)) (hel-selection)]
    ;; At the end of the buffer the selection is empty: there is
    ;; nothing to delete, and the cursor stays.
    (when (< begin end)
      (if (hel-bare-cursor-p selection)
          (delete-region begin end)
        ;; `region-extract-function' handles a region that is not
        ;; contiguous, such as one of `rectangle-mark-mode'.
        (kill-region nil nil t))
      (hel-set-cursor begin)))
  (hel-extend-selection -1))

;; D
(hel-define-command hel-delete ()
  "Delete the selection, without modifying the `kill-ring'.
The cursor stays in place, on the character after the deleted text, as
in Helix and Vim.  A bare cursor is the exception: the cursor goes onto
the character before the deleted one, so `D' works like Backspace."
  :multiple-cursors t
  :merge-selections t
  (interactive "*")
  (-let [(selection &as (begin . end)) (hel-selection)]
    (when (< begin end)
      (delete-region begin end)
      (hel-set-cursor (if (hel-bare-cursor-p selection)
                          (max (1- begin) (point-min))
                        begin))))
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
  ;; Deactivate mark to trigger global undo instead of region undo.  When
  ;; there is nothing to undo, the selection stays.
  (hel-restore-selection-on-error
    (deactivate-mark)
    (let ((deactivate-mark nil))
      (undo-only))))

;; U
(hel-define-command hel-redo ()
  "Redo."
  :multiple-cursors nil
  (interactive "*")
  ;; Deactivate mark to trigger global undo instead of region undo.  When
  ;; there is nothing to undo, the selection stays.
  (hel-restore-selection-on-error
    (deactivate-mark)
    (let ((deactivate-mark nil))
      (undo-redo))))

;; y
(hel-define-command hel-copy ()
  (format "Copy selection into `kill-ring'.

With \\[universal-argument] append selection to the last item in the `kill-ring'.

If there are multiple selections and they are not all the same — also
copy them to the `killed-rectangle'. You can paste them later with %s
(`hel-paste-after') or `yank-rectangle'.

The copied text is filtered by `filter-buffer-substring' before it is saved
in the kill ring."
          (propertize "M-u p" 'face 'help-key-binding))
  :multiple-cursors nil
  (interactive)
  (setq this-command 'hel-copy)
  (let ((deactivate-mark nil)
        (kill-fun (pcase current-prefix-arg
                    ('(4) #'hel--copy-append)
                    (_    #'kill-new))))
    (hel-with-each-cursor
      ;; At the end of the buffer the selection is empty, and an empty
      ;; copy would only push an empty string on the `kill-ring'.
      (-let [(begin . end) (hel-selection-bounds)]
        (when (< begin end)
          (funcall kill-fun (filter-buffer-substring begin end))))
      (hel-extend-selection -1))
    (message "Copied into kill-ring"))
  (hel--maybe-set-killed-rectangle)
  (when (called-interactively-p 'interactive)
    (hel-pulse-main-selection)))

(defun hel--maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry of each cursor to `killed-rectangle',
unless they all are equal. You can paste them later with `yank-rectangle'."
  (when hel-multiple-cursors-mode
    (let ((entries (-map (-lambda ((id))
                           (car-safe (if (eql id 0)
                                         kill-ring
                                       (-> (gethash id hel--cursors-table)
                                           (overlay-get 'kill-ring)))))
                         (hel-selections))))
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
  (-let* (((selection &as (begin . end) dir) (hel-selection))
          (dir (or dir 1))
          ;; After a paste the selection is the pasted text, so its shape
          ;; tells how that paste shaped the text.  A paste into a gap
          ;; leaves an empty line above the text and one below.
          (shape (cond ((not (eq last-command 'yank))
                        (and (hel--paste-line-p selection dir) 'line))
                       ((not (hel-line-selection-p selection))
                        nil)
                       ((and (hel--empty-line-range-p (cons (1- begin) begin))
                             (hel--empty-line-range-p (cons end (1+ end))))
                        'gap)
                       (t 'line)))
          (yank-pop (or (command-remapping 'yank-pop)
                        #'yank-pop)))
    (hel--paste-insert (lambda () (funcall-interactively yank-pop count))
                       shape dir)))

;; C-n
(hel-define-command hel-paste-undo-pop (count)
  "Replace just-pasted text with previous COUNT element from `kill-ring'."
  :multiple-cursors t
  (interactive "*p")
  (hel-paste-pop (- count)))

;; r
(hel-define-command hel-replace-char ()
  "Replace each character of the selection with one read from the keyboard.
Newlines stay as they are, so a selection of only newlines reads no
character, and neither does an empty selection at the end of the buffer."
  :multiple-cursors t
  (interactive "*")
  (-let [(begin . end) (hel-selection-bounds)]
    (unless (save-excursion
              (goto-char begin)
              (skip-chars-forward "\n" end)
              (= (point) end))
      (let ((deactivate-mark nil))
        (hel-read-char-and-replace (cons begin end))))))

;; R
(hel-define-command hel-replace-with-kill-ring ()
  "Replace the selection with the text from the `kill-ring'.
The pasted text becomes the selection, in the direction of the replaced
selection, and forward in place of a bare cursor, which has no
direction.  An empty selection at the end of the buffer is left as it
is."
  :multiple-cursors t
  (interactive "*")
  (-let* (((selection &as (begin . end) dir) (hel-selection))
          (dir (or dir 1)))
    (when (< begin end)
      (let ((shape (and (hel--paste-line-p selection dir) 'line))
            (deactivate-mark nil))
        (delete-region begin end)
        (hel--paste-insert #'yank shape dir)))))

;; J
(hel-define-command hel-join-line ()
  "Join the selected lines."
  :multiple-cursors t
  (interactive "*")
  (hel-save-selection
    (-let* ((deactivate-mark nil)
            ((begin . end) (hel-selection-bounds))
            ;; The selected lines join into one line.  A selection within
            ;; one line joins it with the next line.  The last line has
            ;; no next line, so there it joins nothing: neither the line
            ;; above, which is not selected, nor the final newline.
            (count (min (max 1 (1- (count-lines begin end)))
                        (1- (count-lines (progn (goto-char begin) (pos-bol))
                                         (point-max))))))
      (when (< 0 count)
        (goto-char begin)
        ;; All these `let' bindings moves point.
        (let ((in-comment? (progn (move-beginning-of-line nil)
                                  (skip-chars-forward " \t")
                                  (hel-bounds-of-comment-at (point))))
              (ubeg (progn (forward-line 1)
                           (line-beginning-position)))
              (uend (progn (forward-line (1- count))
                           (line-end-position))))
          (when in-comment?
            (uncomment-region ubeg uend)))
        (dotimes (_ count)
          (forward-line 0)
          (delete-char -1)
          (fixup-whitespace))))))

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
  "Invert the case of the characters in the selection."
  :multiple-cursors t
  (interactive "*")
  ;; Inverting the case keeps the length of the text, so point and the
  ;; mark go back to the positions where they were.
  (let ((point (point))
        (mark (mark t))
        (deactivate-mark nil))
    (hel-invert-case-in-range (hel-selection-bounds))
    (goto-char point)
    (set-marker (mark-marker) mark)))

;; ` or gu
(hel-define-command hel-downcase ()
  "Convert the text in the selection to lower case."
  :multiple-cursors t
  (interactive "*")
  (let ((deactivate-mark nil))
    (-let [(begin . end) (hel-selection-bounds)]
      (downcase-region begin end))))

;; M-` or gU
(hel-define-command hel-upcase ()
  "Convert the text in the selection to upper case."
  :multiple-cursors t
  (interactive "*")
  (let ((deactivate-mark nil))
    (-let [(begin . end) (hel-selection-bounds)]
      (upcase-region begin end))))

;;; Selections

;; M-;
(hel-define-command hel-exchange-point-and-mark ()
  "Move point to the other end of the selection.
A bare cursor has no other end and stays as it is."
  :multiple-cursors t
  (interactive)
  (unless (hel-bare-cursor-p)
    (hel--exchange-point-and-mark)
    (when (and (not hel-executing-command-for-secondary-cursor)
               (called-interactively-p 'any))
      (hel-reveal-point-when-on-top))))

;; ;
(hel-define-command hel-collapse-selection ()
  "Collapse the selection to the character at its head.
The head character is the last character of a forward selection and the
first character of a backward one. It becomes the cursor character.
A bare cursor has nothing to collapse, so there also stop the search."
  :multiple-cursors t
  (interactive)
  (let* ((selection (hel-selection))
         (bare? (hel-bare-cursor-p selection)))
    (hel-set-cursor (hel-selection-head selection))
    (when (and bare? hel-search--session)
      (hel-search-stop))))

;; x
(hel-define-command hel-expand-line-selection (count)
  "Expand or contract the line selection downward COUNT times.
A selection that is not whole lines first grows to its whole lines, and
this takes one step of COUNT.  Each other step moves the cursor end of the
selection by one line: down for a positive COUNT, up for a negative one.
When the cursor end would meet the other end, it passes over it, and the
other end moves back by one line, so the line that the cursor end has left
stays selected.  A selection of one line has the cursor at its start."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((motion-dir (hel-sign count)))
    (when (hel-expand-selection-to-full-lines motion-dir)
      (cl-callf - count motion-dir))
    (unless (zerop count)
      (cl-flet ((line-after (pos count)
                  (save-excursion
                    (goto-char pos)
                    (hel-forward-line count)
                    (point))))
        (-let* (((selection &as (begin . end)) (hel-selection))
                ;; The line selection of an empty line has no direction.
                ;; It has the cursor at its start, as each selection of
                ;; one line.
                (dir (or (hel-selection-direction selection) -1))
                (anchor (if (< 0 dir) begin end))
                (head (line-after (if (< 0 dir) end begin) count))
                (head (if (= head anchor)
                          (line-after head motion-dir)
                        head))
                (new-dir (if (< anchor head) 1 -1))
                (anchor (if (= dir new-dir)
                            anchor
                          (line-after anchor (- motion-dir)))))
          (hel-select (hel-range anchor head) new-dir t))))
    ;; A last line without a newline gets one, so that the selection
    ;; holds it as a whole line.
    (-let* ((((begin . end) dir) (hel-selection))
            (new-end (hel--add-final-newline end)))
      (cond ((= 1 (count-lines begin new-end t))
             (hel-select (cons begin new-end) -1 t))
            ((/= end new-end)
             (hel-select (cons begin new-end) dir t))))
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
  (hel-disable-multiple-cursors-mode)
  (hel-push-point)
  ;; `minibuffer-prompt-end'is really `point-min' in most cases, but if we're
  ;; in the minibuffer, this is at the end of the prompt.
  ;; `%' selects every line, so the last line gets its newline, as with
  ;; `x'.
  (hel-select (cons (minibuffer-prompt-end)
                    (hel--add-final-newline (point-max)))
              -1))

;; M-s
(hel-define-command hel-split-selection-on-newline ()
  "Split each selection into its lines, at the newline characters."
  :multiple-cursors nil
  (interactive)
  (hel-with-each-cursor
    (hel-extend-selection -1)
    (-let [(selection &as (begin . end)) (hel-selection)]
      (unless (hel-bare-cursor-p selection)
        ;; Each line of the selection becomes a forward selection without
        ;; its newline. The cursor keeps the last line.
        (let ((ranges (cl-loop for start = begin then (1+ line-end)
                               for line-end = (save-excursion
                                                (goto-char start)
                                                (min (pos-eol) end))
                               collect (cons start line-end)
                               until (<= end (1+ line-end)))))
          (-each (-butlast ranges)
            (lambda (range) (hel-create-secondary-selection range 1)))
          (hel-select (-last-item ranges) 1))))))

;; _
(hel-define-command hel-trim-whitespace-from-selection ()
  "Trim the whitespace from both ends of the selection.
A bare cursor, and a selection of whitespace only, stay as they are."
  :multiple-cursors t
  (interactive)
  (-let [(selection &as range dir) (hel-selection)]
    (unless (hel-bare-cursor-p selection)
      (-some-> (hel-trim-whitespace-from-range range)
        (hel-select dir)))))

;; &
(hel-define-command hel-align-selections ()
  "Align selections by column: the cursor characters line up.
Each selection gets spaces before it, so that its head character, the
one that the cursor covers, stands in the column of the rightmost one."
  :multiple-cursors nil
  (interactive)
  (hel-with-main-cursor-as-secondary
    (hel-save-window-scroll
      (dolist (cursors (->> (hel-all-secondary-cursors :sort)
                            ;; split cursors into groups by line
                            ;; (lines are identified by their beginning position)
                            (-partition-by (lambda (cursor)
                                             (save-excursion
                                               (goto-char
                                                (hel--secondary-cursor-point
                                                 cursor))
                                               (line-beginning-position))))
                            ;; Transpose columns and rows to align all first
                            ;; cursors in each line, than all second and so on.
                            (hel-transpose)))
        (cl-flet ((head-column (selection)
                    (save-excursion
                      (goto-char (hel-selection-head selection))
                      (current-column))))
          (let ((column (-reduce-from
                         (lambda (column cursor)
                           (max column
                                (head-column
                                 (cdr (hel--secondary-cursor-value cursor)))))
                         0 cursors)))
            (dolist (cursor cursors)
              (hel-with-secondary-cursor cursor
                (when-let* ((selection (hel-selection))
                            (width (- column (head-column selection)))
                            ((< 0 width)))
                  ;; The padding goes before the selection.
                  (hel-save-selection
                    (goto-char (car (hel-selection-bounds selection)))
                    (insert (make-string width ?\s))))))))))))

;; C
(hel-define-command hel-copy-selection (count)
  "Copy selections COUNT times down if COUNT is positive, or up if negative."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-with-each-cursor
    (hel-motion-loop (dir count)
      (if (hel-bare-cursor-p)
          (hel--copy-cursor dir)
        (hel--copy-selection dir)))))

;; M-c
(hel-define-command hel-copy-selection-up (count)
  "Copy each selection COUNT times up."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-copy-selection (- count)))

(defun hel--copy-cursor (direction)
  "Copy the bare cursor to the next line in DIRECTION with its column.
The copy goes to the nearest line that reaches that column."
  (-let* (((begin . end) (hel-selection-bounds))
          (pos (save-excursion
                 (goto-char begin)
                 (cl-loop with column = (current-column)
                          while (zerop (forward-line direction))
                          when (= (move-to-column column) column)
                          return (point)))))
    (when (and pos (not (hel-secondary-cursor-at pos)))
      (unless (hel-secondary-cursor-at begin)
        (hel-create-secondary-selection (cons begin end)))
      (hel-set-cursor pos))))

(defun hel--copy-selection (direction)
  "Copy the selection to the lines next to it in DIRECTION.
The copy has the columns, the number of lines and the direction of the
selection.  Nothing happens when a secondary cursor already holds the copy."
  (-let* (((selection &as (begin . end)) (hel-selection))
          (num-of-lines (count-lines begin end))
          (begin-column (save-excursion (goto-char begin) (current-column)))
          (end-column (save-excursion (goto-char end) (current-column))))
    (when-let* ((bounds (save-excursion
                          (goto-char (if (< direction 0) begin end))
                          (hel--bounds-of-following-range
                           begin-column end-column num-of-lines direction))))
      (let* ((copy (cons bounds (cdr selection)))
             (copy-point (if (eql (hel-selection-direction copy) 1)
                             (cdr bounds)
                           (car bounds))))
        (unless (equal copy (-some-> (hel-secondary-cursor-at copy-point)
                              (hel--secondary-cursor-value)
                              (cdr)))
          (hel-create-secondary-selection)
          (apply #'hel-select copy))))))

(defun hel--bounds-of-following-range ( start-column end-column
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
(defalias 'hel-remove-all-secondary-cursors #'hel-disable-multiple-cursors-mode
  (format "Delete all secondary cursors from current buffer.
You may restore them with %s (`hel-restore-cursors')."
          (propertize "g v" 'face 'help-key-binding)))

;; M-,
(hel-define-command hel-remove-main-cursor ()
  "Delete the main cursor and make the next secondary cursor the main one."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (hel-make-cursor-main (or (hel-next-secondary-cursor (point))
                              (hel-first-secondary-cursor)))
    (hel-auto-multiple-cursors-mode)))

;; gv
(hel-define-command hel-restore-cursors ()
  "Restore last multiple cursors positions."
  :multiple-cursors nil
  (interactive)
  (when hel--selections-history
    (hel-set-selections hel--selections-history)))

;; M-minus
(hel-define-command hel-merge-selections ()
  "Merge all cursors into single selection.
The selection goes from the beginning of the first selection to the end
of the last one."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (let ((ranges (-map (-lambda ((_id . selection))
                          (hel-selection-bounds selection))
                        (hel-selections))))
      (hel-disable-multiple-cursors-mode)
      (hel-select (cons (car (-first-item ranges))
                        (cdr (-last-item ranges)))))))

;; gg
(hel-define-command hel-first-selection ()
  "Make the first selection the main selection."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (hel-create-secondary-selection)
      (hel-make-cursor-main (hel-first-secondary-cursor)))))

;; G
(hel-define-command hel-last-selection ()
  "Make the last selection the main selection."
  :multiple-cursors nil
  (interactive)
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (hel-create-secondary-selection)
      (hel-make-cursor-main (hel-last-secondary-cursor)))))

;; )
(hel-define-command hel-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (hel-next-secondary-cursor (point))
                          (hel-first-secondary-cursor))))
          (hel-create-secondary-selection)
          (hel-make-cursor-main cursor))))))

;; (
(hel-define-command hel-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when hel-multiple-cursors-mode
    (hel-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (hel-previous-secondary-cursor (point))
                          (hel-last-secondary-cursor))))
          (hel-create-secondary-selection)
          (hel-make-cursor-main cursor))))))

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
  (when hel-multiple-cursors-mode
    (let ((dir (hel-selection-direction)))
      ;; To rotate the content of adjacent selections, all regions need
      ;; a negative direction. Text inserted at the point of a secondary
      ;; cursor goes before it, and text inserted at its mark goes after it,
      ;; see `hel--save-cursor'. The beginning of a region must move on an
      ;; insertion at its position, and the end must not. A bare cursor gets
      ;; the region of its cursor character, which it rotates, so the region
      ;; is set here and not with `hel-select'.
      (hel-with-each-cursor
        (-let [(begin . end) (hel-selection-bounds)]
          (set-mark end)
          (goto-char begin)))
      (hel-with-main-cursor-as-secondary
        (let ((cursors (hel-all-secondary-cursors :sort)))
          (when backward
            (cl-callf nreverse cursors))
          (dotimes (_ count)
            (hel--rotate-selections-content-1 cursors))))
      ;; Give the regions the direction of the main region again.
      (when (eql dir 1)
        (hel-with-each-cursor (hel--exchange-point-and-mark))))))

(defun hel--rotate-selections-content-1 (cursors)
  "Rotate regions content for all CURSORS."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (hel--secondary-cursor-point first-cursor)
                                    (hel--secondary-cursor-mark first-cursor))))
    (dolist (cursor (cdr cursors))
      (setq content (hel--exchange-region-content cursor content)))
    (hel--exchange-region-content first-cursor content)))

(defun hel--exchange-region-content (cursor content)
  "Replace the text in the region of the secondary CURSOR with CONTENT.
Return the text that was there."
  (hel-with-secondary-cursor cursor
    (-let* ((deactivate-mark nil) ;; Do not deactivate mark after insertion.
            ((begin . end) (hel-range (point) (mark)))
            (new-content (buffer-substring begin end)))
      (delete-region begin end)
      (goto-char begin)
      (insert content)
      ;; The region covers the new text backward, as the rotation set it.
      (set-marker (mark-marker) (point))
      (goto-char begin)
      new-content)))

;; (keymap-lookup nil "M-<down-mouse-1>")

;; M-<right-mouse>
(hel-define-command hel-toggle-cursor-on-click (event)
  "Add a secondary cursor where you click, or remove the one that is there."
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
      (if-let* ((cursor (hel-secondary-cursor-at pos)))
          (hel-delete-secondary-cursor cursor)
        (hel-create-secondary-selection (hel-char-range pos))))))

;;;; Digit arguments

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

;;;; Text objects
;;
;; Each text object has four commands. `mi' selects COUNT things at the
;; selection, and `ma' adds the header comment and the gap. `]' selects
;; from the cursor to the end of the thing under it, or the whole next
;; thing when the cursor is already there, and `[' does the same backward.

;; miw
(hel-define-command hel-mark-word (count)
  "Select the word at the cursor, or COUNT words.
A repeated call adds COUNT more words."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'word :inner count))

;; maw
(hel-define-command hel-mark-around-word (count)
  "Select the word at the cursor with the blanks after it.
On the last word of a line, take the blanks before it. With COUNT,
select COUNT words."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'word :around count))

;; miW
(hel-define-command hel-mark-WORD (count)
  "Select the WORD at the cursor, or COUNT WORDs.
A repeated call adds COUNT more WORDs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'WORD :inner count))

;; maW
(hel-define-command hel-mark-around-WORD (count)
  "Select the WORD at the cursor with the blanks after it.
With COUNT, select COUNT WORDs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'WORD :around count))

;; mis
(hel-define-command hel-mark-sentence (count)
  "Select the sentence at the cursor, or COUNT sentences.
The sentences stay inside the paragraph of the first one."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'sentence :inner count))

;; mas
(hel-define-command hel-mark-around-sentence (count)
  "Select the sentence at the cursor with the whitespace after it.
On the last sentence of a paragraph, take the whitespace before it. With
COUNT, select COUNT sentences."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'sentence :around count))

;; ]s
(hel-define-command hel-mark-sentence-forward (count &optional extend)
  "Select from the cursor to the end of the sentence under the cursor.
When point is already there, select the whole next sentence. With COUNT,
go on to the end of the COUNT-th sentence."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'sentence count :end t extend))

;; [s
(hel-define-command hel-mark-sentence-backward (count &optional extend)
  "Select from the cursor to the start of the sentence under the cursor.
When point is already there, select the whole previous sentence. With
COUNT, go on to the start of the COUNT-th sentence."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'sentence (- count) :begin t extend))

;; mip
(hel-define-command hel-mark-paragraph (count)
  "Select the paragraph at the cursor, or COUNT paragraphs.
A paragraph covers whole lines."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'paragraph :inner count))

;; map
(hel-define-command hel-mark-around-paragraph (count)
  "Select the paragraph at the cursor with the blank lines after it.
On the last paragraph, take the blank lines before it. With COUNT,
select COUNT paragraphs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'paragraph :around count))

;; }
(hel-define-command hel-forward-paragraph (count &optional extend)
  "Select from the cursor to the beginning of the COUNT-th next paragraph.
Without a next paragraph, select to the end of the buffer."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (unless (hel--report-buffer-edge count)
    (hel-mark-to-thing-edge 'paragraph count :begin nil extend)))

;; {
(hel-define-command hel-backward-paragraph (count &optional extend)
  "Select to the beginning of the COUNT-th previous paragraph.
Without a previous paragraph, select to the start of the buffer."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (unless (hel--report-buffer-edge (- count))
    (hel-mark-to-thing-edge 'paragraph (- count) :begin nil extend)))

(defun hel--report-buffer-edge (dir)
  "Return non-nil if point is at the buffer edge in DIR, and say so.
The message goes to the echo area and stays out of the *Messages* buffer."
  (when (if (< 0 dir) (eobp) (bobp))
    (let ((message-log-max nil))
      (message (if (< 0 dir) "End of buffer" "Beginning of buffer")))
    t))

;; mif
(hel-define-command hel-mark-function (count)
  "Select the function at the cursor, or COUNT functions.
The functions stay inside the parent of the first one. A repeated call
adds COUNT more functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'function :inner count))

;; maf
(hel-define-command hel-mark-around-function (count)
  "Select the function at the cursor with its header comment and gap.
The gap is the text up to the next function, or from the previous one
when no function follows. With COUNT, select COUNT functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'function :around count))

;; ]f
(hel-define-command hel-mark-function-forward (count &optional extend)
  "Select from the cursor to the end of the function under the cursor.
When point is already there, select the whole next function. With COUNT,
go on to the end of the COUNT-th function."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'function count :end t extend))

;; [f
(hel-define-command hel-mark-function-backward (count &optional extend)
  "Select from the cursor to the start of the function under the cursor.
When point is already there, select the whole previous function. With
COUNT, go on to the start of the COUNT-th function."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'function (- count) :begin t extend))

;; mit
(hel-define-command hel-mark-class (count)
  "Select the class or type at the cursor, or COUNT of them."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'class :inner count))

;; mat
(hel-define-command hel-mark-around-class (count)
  "Select the class at the cursor with its header comment and gap.
With COUNT, select COUNT classes."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'class :around count))

;; ]t
(hel-define-command hel-mark-class-forward (count &optional extend)
  "Select from the cursor to the end of the class under the cursor.
When point is already there, select the whole next class. With COUNT,
go on to the end of the COUNT-th class."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'class count :end t extend))

;; [t
(hel-define-command hel-mark-class-backward (count &optional extend)
  "Select from the cursor to the start of the class under the cursor.
When point is already there, select the whole previous class. With
COUNT, go on to the start of the COUNT-th class."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'class (- count) :begin t extend))

;; mia
(hel-define-command hel-mark-argument (count)
  "Select the argument or parameter at the cursor, or COUNT of them."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'argument :inner count))

;; maa
(hel-define-command hel-mark-around-argument (count)
  "Select the argument at the cursor with the separator after it.
On the last argument, take the separator before it. With COUNT, select
COUNT arguments."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'argument :around count))

;; ]a
(hel-define-command hel-mark-argument-forward (count &optional extend)
  "Select from the cursor to the end of the argument under the cursor.
When point is already there, select the whole next argument. With COUNT,
go on to the end of the COUNT-th argument."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'argument count :end t extend))

;; [a
(hel-define-command hel-mark-argument-backward (count &optional extend)
  "Select from the cursor to the start of the argument under the cursor.
When point is already there, select the whole previous argument. With
COUNT, go on to the start of the COUNT-th argument."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'argument (- count) :begin t extend))

;; mie
(hel-define-command hel-mark-entry (count)
  "Select the entry at the cursor, or COUNT of them.
An entry is one element of a data structure: an element of a list, a key
with its value in a dictionary, or a field of a struct."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'entry :inner count))

;; mae
(hel-define-command hel-mark-around-entry (count)
  "Select the entry at the cursor with the separator after it.
On the last entry, take the separator before it. With COUNT, select COUNT
entries."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'entry :around count))

;; ]e
(hel-define-command hel-mark-entry-forward (count &optional extend)
  "Select from the cursor to the end of the entry under the cursor.
When point is already there, select the whole next entry. With COUNT, go
on to the end of the COUNT-th entry."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'entry count :end t extend))

;; [e
(hel-define-command hel-mark-entry-backward (count &optional extend)
  "Select from the cursor to the start of the entry under the cursor.
When point is already there, select the whole previous entry. With COUNT,
go on to the start of the COUNT-th entry."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'entry (- count) :begin t extend))

;; mic
(hel-define-command hel-mark-comment (count)
  "Select the comment at the cursor, or COUNT comments.
A comment holds all the comment lines that have no blank line between
them."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'comment :inner count))

;; mac
(hel-define-command hel-mark-around-comment (count)
  "Select the comment at the cursor with the blank lines after it.
After code, the gap is the blanks on the line. With COUNT, select COUNT
comments."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-text-object 'comment :around count))

;; ]c
(hel-define-command hel-mark-comment-forward (count &optional extend)
  "Select from the cursor to the end of the comment under the cursor.
When point is already there, select the whole next comment. With COUNT,
go on to the end of the COUNT-th comment."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'comment count :end t extend))

;; [c
(hel-define-command hel-mark-comment-backward (count &optional extend)
  "Select from the cursor to the start of the comment under the cursor.
When point is already there, select the whole previous comment. With
COUNT, go on to the start of the COUNT-th comment."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (hel-mark-to-thing-edge 'comment (- count) :begin t extend))

;;;; Mark pairs

;; mi"
(hel-define-command hel-mark-inner-double-quoted (count)
  "Select the text between the double quotes at the cursor.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\" :inner count))

;; ma"
(hel-define-command hel-mark-a-double-quoted (count)
  "Select the text between the double quotes at the cursor, with the quotes.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\" :around count))

;; mi'
(hel-define-command hel-mark-inner-single-quoted (count)
  "Select the text between the single quotes at the cursor.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?' :inner count))

;; ma'
(hel-define-command hel-mark-a-single-quoted (count)
  "Select the text between the single quotes at the cursor, with the quotes.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?' :around count))

;; mi`
(hel-define-command hel-mark-inner-back-quoted (count)
  "Select the text between the backquotes at the cursor.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?` :inner count))

;; ma`
(hel-define-command hel-mark-a-back-quoted (count)
  "Select the text between the backquotes at the cursor, with the quotes.
COUNT is ignored: quotes do not nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?` :around count))

;; mi( mi)
(hel-define-command hel-mark-inner-paren (count)
  "Select the text inside the parentheses around the cursor.
The whitespace at the edges of the text stays out. A repeated key
selects inside the next parentheses outward, and COUNT goes COUNT - 1
pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\( :inner count))

;; ma( ma)
(hel-define-command hel-mark-a-paren (count)
  "Select the parentheses around the cursor, with the text inside.
A repeated key selects the next parentheses outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\( :around count))

;; mi[ mi]
(hel-define-command hel-mark-inner-bracket (count)
  "Select the text inside the square brackets around the cursor.
The whitespace at the edges of the text stays out. A repeated key
selects inside the next square brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\[ :inner count))

;; ma[ ma]
(hel-define-command hel-mark-a-bracket (count)
  "Select the square brackets around the cursor, with the text inside.
A repeated key selects the next square brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?\[ :around count))

;; mi{ mi}
(hel-define-command hel-mark-inner-curly (count)
  "Select the text inside the curly brackets around the cursor.
The whitespace at the edges of the text stays out. A repeated key
selects inside the next curly brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?{ :inner count))

;; ma{ ma}
(hel-define-command hel-mark-a-curly (count)
  "Select the curly brackets around the cursor, with the text inside.
A repeated key selects the next curly brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?{ :around count))

;; mi< mi>
(hel-define-command hel-mark-inner-angle (count)
  "Select the text inside the angle brackets around the cursor.
The whitespace at the edges of the text stays out. A repeated key
selects inside the next angle brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?< :inner count))

;; ma< ma>
(hel-define-command hel-mark-an-angle (count)
  "Select the angle brackets around the cursor, with the text inside.
A repeated key selects the next angle brackets outward, and COUNT goes
COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?< :around count))

;; mim mb
(hel-define-command hel-mark-inner-closest-pair (count)
  "Select the text inside the closest brackets around the cursor.
The brackets are parentheses, square brackets or curly brackets,
whichever lie closest. The whitespace at the edges of the text stays
out. A repeated key selects inside the next brackets outward, and COUNT
goes COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?m :inner count))

;; mam
(hel-define-command hel-mark-around-closest-pair (count)
  "Select the closest brackets around the cursor, with the text inside.
The brackets are parentheses, square brackets or curly brackets,
whichever lie closest. A repeated key selects the next brackets
outward, and COUNT goes COUNT - 1 pairs outward."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-pair ?m :around count))

(defun hel--last-command-char ()
  "Return the character of the last key that ran the command, or nil."
  (if (characterp last-command-event)
      last-command-event
    (get last-command-event 'ascii-character)))

;; mi* and the other characters
(hel-define-command hel-mark-inner-surround (count)
  "Select the text between two characters of the last key, as `*' in `mi*'.
The key finds its pair through `hel-pair-sources'. A character without
sources pairs with itself. COUNT goes COUNT - 1 pairs outward when the
pairs nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (when-let* ((char (hel--last-command-char)))
    (hel-mark-pair char :inner count)))

;; ma* and the other characters
(hel-define-command hel-mark-a-surround (count)
  "Select the text between two characters of the last key, with them.
The key is the last character typed, as `*' in `ma*', and it finds its
pair through `hel-pair-sources'. A character without sources pairs with
itself. COUNT goes COUNT - 1 pairs outward when the pairs nest."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (when-let* ((char (hel--last-command-char)))
    (hel-mark-pair char :around count)))

;;;; Expand selection

;; M-i or C-j
(hel-define-command hel-shrink-selection (count)
  "Restore the selection that preceded the last expand or text object.
Repeat COUNT times. A negative COUNT expands the selection. When the
selection differs from the last entry of the history, select the first
child of its node."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-expand producer (- count) -1)))

;; M-o, M-b or C-k
(hel-define-command hel-expand-selection-backward (count)
  "Select the smallest syntax region that encloses the selection.
Put point at its start. Without an active region, start from the
character under the cursor. Repeat COUNT times. A negative COUNT shrinks
the selection.

In a buffer with a tree-sitter parser, the regions are the syntax nodes.
In a Lisp buffer they are the sexps. In other buffers they come from the
syntax table: the word and the symbol at the selection start, the inside
and the whole of each enclosing string, comment and bracket pair, the
sentence in `text-mode' and the paragraph."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-expand producer count -1)))

;; M-e
(hel-define-command hel-expand-selection-forward (count)
  "Select the smallest syntax region that encloses the selection.
Put point at its end. Without an active region, start from the
character under the cursor. Repeat COUNT times. A negative COUNT shrinks
the selection.
The regions are those of `hel-expand-selection-backward'."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-expand producer count 1)))

;;;; Structural editing

;; M-n or C-l
(hel-define-command hel-select-next-sibling (count &optional extend)
  "Select the next sibling of the node under the selection.
At the last sibling, keep the selection. Repeat COUNT times. A negative
COUNT selects the previous siblings. When no node is under the selection,
as on a space between two nodes, first select the node after it."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-select-sibling producer count extend)))

;; M-p or C-h
(hel-define-command hel-select-previous-sibling (count &optional extend)
  "Select the previous sibling of the node under the selection.
At the first sibling, keep the selection. Repeat COUNT times. When no
node is under the selection, as on a space between two nodes, first
select the node before it."
  :multiple-cursors t
  :merge-selections t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     hel-extend-selection))
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-select-sibling producer (- count) extend)))

;; M-a
(hel-define-command hel-select-all-siblings ()
  "Select the node under the selection and all its siblings.
Put a cursor on each of them. When the node is an only child, use the
nearest parent with more than one child."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-select-all-siblings producer)))

;; M-I or M-j
(hel-define-command hel-select-all-children ()
  "Select all children of the node under the selection.
Put a cursor on each of them. When the node has no children, keep the
selection."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (when-let* ((producer (hel-node-producer)))
    (hel--tree-select-all-children producer)))

;;; Surround

;; ms
(hel-define-command hel-surround ()
  (format "Enclose the selection with the delimiters of a key.
Read the key, see `hel-surround-delimiters'. A bare cursor encloses the
character under it.

Left brackets — `(', `[', `{' — add a space between each bracket and the
region. With a linewise selection (made with \"x\"), they put each
bracket on a line of its own instead.

Right brackets — `)', `]', `}' — are inserted tightly, and leave out
the spaces and the line break at the edges of the region.

%s -- surround the region with an HTML tag: type “div class=\"box\"”​ and
the region is wrapped in <div class=\"box\"> and </div>. With a linewise
selection (made with %s), the tags go on lines of their own.

In a programming mode the new lines are indented by the rules of the
mode. In other modes the region keeps its indentation, and the new lines
take the least indentation of the region."
          (propertize "m s t" 'face 'help-key-binding)
          (propertize "x" 'face 'help-key-binding))
  :multiple-cursors t
  (interactive)
  (when-let* ((selection (hel-selection))
              ((-let [((begin . end)) selection] (< begin end)))
              (key (read-char "Surround: " t)))
    (-let* (((delimiters . padding) (hel-surround--delimiters key))
            (linewise? (and padding (hel-line-selection-p selection))))
      (unless (or linewise? (hel-bare-cursor-p selection))
        (hel-trim-whitespace-from-selection))
      (-let [(begin . end) (hel-selection-bounds)]
        (hel-save-selection
          (hel-surround-wrap begin end delimiters padding linewise?))))
    (hel-extend-selection -1)))

;; md
(hel-define-command hel-surround-delete ()
  (format "Delete the pair of delimiters around point.
Read the key of the pair, see `hel-pair-sources'. A delimiter alone on
its line goes with its line, and a programming mode then indents the
text again. A left bracket also deletes the spaces inside the brackets,
and a right bracket deletes only the brackets.

%s -- delete an HTML tag: type “div​” and the enclosing <div class=\"box\">
and </div> tags are deleted. An empty name deletes the nearest
enclosing tags."
          (propertize "m d t" 'face 'help-key-binding))
  :multiple-cursors t
  (interactive)
  (when-let* ((char (read-char "Delete pair: " t))
              (producer (hel-pair-producer char (point)))
              (pair (hel-pair-at producer (point))))
    (let* ((pair (hel-surround-widen pair (hel-surround--padding char)))
           (lines? (hel--pair-on-own-lines-p pair))
           (text-begin (copy-marker (hel-pair-open-end pair)))
           (text-end (copy-marker (hel-pair-close-begin pair)))
           (deactivate-mark nil))
      (hel-surround-replace pair '("" . ""))
      (when (and lines? (derived-mode-p 'prog-mode))
        (indent-region text-begin text-end))
      (set-marker text-begin nil)
      (set-marker text-end nil))))

;; mr
(hel-define-command hel-surround-change ()
  (format "Replace the pair of delimiters around point with another pair.
Read the key of the old pair, see `hel-pair-sources', then the key of the
new one, see `hel-surround-delimiters'. Only the delimiters change: the
spaces and the line breaks between them and the text stay.

%s -- replace one HTML tag with another. An empty tag name replaces
the nearest enclosing tags."
          (propertize "m r t" 'face 'help-key-binding))
  :multiple-cursors t
  (interactive)
  (when-let* ((char (read-char "Replace pair: " t))
              (producer (hel-pair-producer char (point)))
              (pair (hel-pair-at producer (point))))
    (let ((delimiters (if (eq char ?t)
                          (hel-surround-read-tag "With tag: ")
                        (car (hel-surround--delimiters
                              (read-char "With pair: " t))))))
      (hel-save-selection
        (hel-surround-replace pair delimiters)))))

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
`hel-widen-indirectly-narrowed' to undo it (incrementally).  A bare
cursor does nothing: a buffer narrowed to one character is useless."
  :multiple-cursors t
  (interactive)
  (-let [(selection &as (begin . end)) (hel-selection)]
    (unless (hel-bare-cursor-p selection)
      (let ((orig-buffer (current-buffer))
            (name (or buffer-file-name
                      list-buffers-directory)))
        (deactivate-mark)
        (hel-clone-indirect-buffer-same-window)
        (setq list-buffers-directory name
              hel--narrowed-base-buffer orig-buffer)
        (narrow-to-region begin end)))))

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
