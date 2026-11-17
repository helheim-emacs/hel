;;; hel-multiple-cursors-core.el --- Fake cursor engine -*- lexical-binding: t -*-
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
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The core functionality for multiple cursors. The code is inspired by
;; `multiple-cursors.el' package from Magnar Sveen.
;;
;; How multiple cursors works internally. Command is first executed for
;; real cursor by Emacs command loop. Then in `post-command-hook' it is executed
;; for all fake cursors. Fake cursor is an overlay that emulates cursor and
;; stores inside point, mark, kill-ring and some other variables (full list
;; is in `hel-fake-cursor-variables'). Executing command for fake cursor looks
;; as follows: set point and mark to positions saved in fake cursor overlay,
;; restore all variables from it, execute command in this environment, store
;; point, mark and new state into fake cursor overlay.
;;
;; How command will be executed is controlled by the `multiple-cursors' symbol
;; property with three cases:
;; - t         for all cursors
;; - nil       property explicitly set: only for the main cursor
;; - no value  property not present: prompt the user and permanently store
;;             the choice in `hel-whitelist-file'
;;
;; ID 0 always corresponds to the real cursor.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'subr-x)
(require 'rect)
(require 'hel-lib)

(defvar hel-insert-state)
(defvar hel-normal-state)

(declare-function hel-update-cursor "hel-core")

;;; Multiple cursors minor mode

(define-minor-mode hel-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need to activate it manually: it is activated automatically when you create
first fake cursor with `hel-create-fake-cursor', and disabled when you
delete last one with `hel-delete-fake-cursor'."
  :interactive nil
  :keymap (make-sparse-keymap)
  (if hel-multiple-cursors-mode
      (hel--disable-minor-modes-incompatible-with-multiple-cursors)
    ;; else
    (when (hel-any-fake-cursors-p)
      (setq hel--cursor-selections-history (hel-cursor-selections))
      (hel--delete-all-fake-cursors))
    (hel--enable-minor-modes-incompatible-with-multiple-cursors)))

(defun hel-auto-multiple-cursors-mode ()
  "Enable `hel-multiple-cursors' if there are multiple cursors,
disable if only one."
  (when (xor hel-multiple-cursors-mode
             (hel-any-fake-cursors-p))
    (hel-multiple-cursors-mode 'toggle)))

(defun hel-disable-multiple-cursors-mode ()
  "Remove all fake cursors from the current buffer.
You may restore them with `hel-restore-cursors'."
  (interactive)
  (when hel-multiple-cursors-mode
    (hel-multiple-cursors-mode -1)))

(defun hel--disable-minor-modes-incompatible-with-multiple-cursors ()
  "Disable incompatible minor modes while there are multiple cursors
in the buffer."
  (dolist (mode hel-minor-modes-incompatible-with-multiple-cursors)
    (when (and (boundp mode) (symbol-value mode))
      (push mode hel--temporarily-disabled-minor-modes)
      (funcall mode -1))))

(defun hel--enable-minor-modes-incompatible-with-multiple-cursors ()
  "Enable minor modes disabled by
`hel--disable-minor-modes-incompatible-with-multiple-cursors'."
  (when hel--temporarily-disabled-minor-modes
    (dolist (mode hel--temporarily-disabled-minor-modes)
      (funcall mode 1))
    (setq hel--temporarily-disabled-minor-modes nil)))

;;; Fake cursor object

(defun hel--new-fake-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf hel--fake-cursor-last-used-id))

(defvar hel--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `hel-create-fake-cursor' is called and `hel-max-cursors-number' is
overridden, this value serves as a backup so that `hel-max-cursors-number'
can take on a new value. When `hel--delete-all-fake-cursors' is called,
the values are reset.")

(defun hel-create-fake-cursor (point &optional mark id)
  "Create a fake cursor at POINT position.
If MARK is passed a fake active region overlay between POINT
and MARK will be created.
The ID, if specified, will be assigned to the new cursor.
Otherwise, the new unique ID will be created.
The current state is stored in the overlay for later retrieval."
  (unless hel--max-cursors-original
    (setq hel--max-cursors-original hel-max-cursors-number))
  (when hel-max-cursors-number
    (when-let* ((num (hel-number-of-cursors))
                ((<= hel-max-cursors-number num)))
      (if (yes-or-no-p (format "%d active cursors. Continue? " num))
          (setq hel-max-cursors-number (read-number "Enter a new, temporary maximum: "))
        (hel--delete-all-fake-cursors)
        (error "Aborted: too many cursors"))))
  (prog1 (hel--create-fake-cursor-1 id point mark)
    (unless hel-multiple-cursors-mode
      (hel-multiple-cursors-mode 1))))

(defun hel--create-fake-cursor-1 (id point mark)
  "Create a fake cursor with ID at POINT and fake region between POINT and MARK.
This function is the guts of the `hel-create-fake-cursor'."
  (or id (setq id (hel--new-fake-cursor-id)))
  (save-excursion
    (goto-char point)
    (let ((cursor (make-overlay point point nil t nil)))
      (overlay-put cursor 'id id)
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 101)
      (hel--store-cursor-state cursor point mark)
      (hel--set-cursor-overlay cursor)
      (hel--set-fake-selection-overlay cursor)
      (puthash id cursor hel--cursors-table)
      cursor)))

(defun hel--delete-all-fake-cursors ()
  "Remove all fake cursors overlays form current buffer.
It is likely that you need `hel-disable-multiple-cursors-mode', not this one."
  (when hel--max-cursors-original
    (setq hel-max-cursors-number hel--max-cursors-original
          hel--max-cursors-original nil))
  (maphash (lambda (_ cursor)
             (hel--delete-fake-cursor cursor))
           hel--cursors-table))

(defun hel-create-fake-cursor-from-point (&optional id)
  "Create a fake cursor with an optional fake region based on point and mark.
Assign the ID to the new fake cursor, if specified.
The current state is stored in it for later retrieval."
  (hel-create-fake-cursor (point) (mark t) id))

(defun hel-move-fake-cursor (cursor point &optional mark update)
  "Move fake CURSOR to new POINT.
If MARK is non-nil also set fake region.
Move fake CURSOR and its region according to new POINT and MARK.
Optionally UPDATE fake-cursors state."
  (set-marker (overlay-get cursor 'point) point)
  (set-marker (overlay-get cursor 'mark) (or mark point))
  (when update (hel-update-fake-cursor-state cursor))
  (hel--set-cursor-overlay cursor)
  (hel--set-fake-selection-overlay cursor)
  cursor)

(defun hel-delete-fake-cursor (cursor)
  "Delete fake CURSOR and disable `hel-multiple-cursors-mode' if no
more fake cursors are remaining."
  (hel--delete-fake-cursor cursor)
  (hel-auto-multiple-cursors-mode))

(defun hel--set-cursor-overlay (cursor)
  "Draw fake CURSOR from the point, the mark and the state that it stores.
In Normal state, when its region is not active or is empty, the fake
cursor covers its cursor character: the bounds that `hel-selection'
gives for its point and mark.  Otherwise it covers the character after
its point.

A newline, or the empty bounds at the end of the buffer, show as a cell
of one space after the last character of the line, because a face on
a newline paints to the right edge of the window.

With a bar cursor in a graphic frame, when `hel-match-fake-cursor-style'
is non-nil, the fake cursor is a bar at its point, and its cursor
character has the face `hel-cursor-char', as the main cursor has."
  (let* ((point (marker-position (overlay-get cursor 'point)))
         (mark (marker-position (overlay-get cursor 'mark)))
         (char? (and hel-normal-state
                     (not (and (overlay-get cursor 'mark-active)
                               (/= point mark)))))
         (bounds (if char?
                     (car (hel--selection-from point mark nil))
                   (cons point (min (1+ point) (point-max)))))
         (begin (car bounds))
         ;; A face on a newline paints to the right edge of the window.
         (end (if (eq (char-after begin) ?\n)
                  begin
                (cdr bounds)))
         (face (cond (hel-extend-selection
                      'hel-extend-selection-cursor)
                     (hel-insert-state
                      'hel-insert-state-fake-cursor)
                     (t
                      'hel-normal-state-fake-cursor))))
    (move-overlay cursor begin end)
    (if (and (display-graphic-p)
             hel-match-fake-cursor-style
             (hel-cursor-is-bar-p))
        (let ((bar (propertize hel-bar-fake-cursor
                               'face `(,face
                                       (:height ,(window-default-font-height)))))
              (char-face (if char? 'hel-cursor-char)))
          (overlay-put cursor 'face (if (/= begin end) char-face))
          (overlay-put cursor 'before-string (if (= point begin) bar))
          (overlay-put cursor 'after-string
                       (cond ((/= point begin) bar)
                             ((and char? (= begin end))
                              (propertize " " 'face 'hel-cursor-char)))))
      (overlay-put cursor 'face (if (/= begin end) face))
      (overlay-put cursor 'before-string nil)
      (overlay-put cursor 'after-string
                   (if (= begin end)
                       (propertize " " 'face face))))
    cursor))

(defun hel--set-fake-selection-overlay (cursor)
  "For fake CURSOR setup the overlay looking like active region when appropriate."
  (let ((beg (overlay-get cursor 'point))
        (end (overlay-get cursor 'mark)))
    (if (and (overlay-get cursor 'mark-active)
             (/= beg end))
        (if-let* ((region (overlay-get cursor 'fake-region)))
            (move-overlay region beg end)
          ;; else
          (setq region (-doto (make-overlay beg end nil nil t)
                         (overlay-put 'face 'region)
                         (overlay-put 'type 'fake-region)
                         (overlay-put 'id (overlay-get cursor 'id))
                         ;; The same as Emacs native region overlay.
                         (overlay-put 'priority 100)))
          (overlay-put cursor 'fake-region region))
      ;; else
      (hel--delete-fake-selection-overlay cursor))))

(defun hel--delete-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (remhash (overlay-get cursor 'id) hel--cursors-table)
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (hel--delete-fake-selection-overlay cursor)
  (delete-overlay cursor))

(defun hel--delete-fake-selection-overlay (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (-some-> (overlay-get cursor 'fake-region)
    (delete-overlay)))

(defun hel--store-cursor-state (overlay point mark)
  "Store POINT, MARK and variables relevant to fake cursor into OVERLAY."
  (or mark (setq mark point))
  (if-let* ((pnt (overlay-get overlay 'point)))
      (set-marker pnt point)
    (overlay-put overlay 'point (copy-marker point t)))
  (if-let* ((mrk (overlay-get overlay 'mark)))
      (set-marker mrk mark)
    (overlay-put overlay 'mark (copy-marker mark)))
  (dolist (var hel-fake-cursor-variables)
    (if (boundp var)
        (overlay-put overlay var (symbol-value var))))
  overlay)

(defun hel-update-fake-cursor-state (cursor)
  "Update variables stored in fake CURSOR."
  (dolist (var hel-fake-cursor-variables)
    (if (boundp var)
        (overlay-put cursor var (symbol-value var)))))

(defun hel-restore-point-from-fake-cursor (cursor)
  "Restore point, mark and variables from fake CURSOR overlay and delete it."
  (hel--restore-cursor-state cursor)
  (hel--delete-fake-cursor cursor))

(defun hel--restore-cursor-state (overlay)
  "Restore point, mark and cursor variables saved in OVERLAY."
  (goto-char (overlay-get overlay 'point))
  (set-marker (mark-marker) (overlay-get overlay 'mark))
  (dolist (var hel-fake-cursor-variables)
    (if (boundp var)
        (set var (overlay-get overlay var))))
  (hel--delete-fake-selection-overlay overlay)
  (delete-overlay overlay))

(defun hel-hide-fake-cursor (cursor)
  "Disable the fake CURSOR visibility in the buffer without deleting it."
  (hel--delete-fake-selection-overlay cursor)
  (delete-overlay cursor)
  cursor)

(defun hel-show-fake-cursor (cursor)
  "Restore fake CURSOR visibility if it was previously hidden with
`hel-hide-fake-cursor'."
  (hel--set-cursor-overlay cursor)
  (hel--set-fake-selection-overlay cursor))

(defun hel-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun hel-fake-selection-p (overlay)
  "Return t if an OVERLAY is a fake region."
  (eq (overlay-get overlay 'type) 'fake-region))

;;; Access fake cursors

(defun hel-all-fake-cursors (&optional sort)
  "Return list with all fake cursors in current buffer.
If SORT is non-nil sort cursors in order they are located in the buffer."
  ;; The table is nil in buffers without `hel-local-mode', for example the
  ;; minibuffer, but global advices such as `current-kill@hel' run there too.
  (when hel--cursors-table
    (if sort
        (sort (hash-table-values hel--cursors-table)
              (lambda (c1 c2)
                (< (overlay-get c1 'point)
                   (overlay-get c2 'point))))
      (hash-table-values hel--cursors-table))))

(defun hel-cursor-with-id (id)
  "Return the cursor with the given ID if it is stil alive."
  (if-let* ((cursor (gethash id hel--cursors-table))
            ((hel-overlay-live-p cursor)))
      cursor))

(defun hel-fake-cursor-at (position)
  "Return the fake cursor at POSITION, or nil if no one."
  (-some->> (overlays-in position (1+ position))
    (-filter #'hel-fake-cursor-p)
    (--find (= position (overlay-get it 'point)))))

(defun hel-next-fake-cursor (position)
  "Return the next fake cursor after the POSITION."
  (cl-loop for pos = (next-overlay-change position)
           then (next-overlay-change pos)
           until (= pos (point-max))
           thereis (hel-fake-cursor-at pos)))

(defun hel-previous-fake-cursor (position)
  "Return the first fake cursor before the POSITION."
  (cl-loop for pos = (previous-overlay-change position)
           then (previous-overlay-change pos)
           until (= pos (point-min))
           thereis (hel-fake-cursor-at pos)))

(defun hel-first-fake-cursor ()
  "Return the first fake cursor in the buffer."
  (-min-by (lambda (a b)
             (> (overlay-get a 'point)
                (overlay-get b 'point)))
           (hel-all-fake-cursors)))

(defun hel-last-fake-cursor ()
  "Return the last fake cursor in the buffer."
  (-max-by (lambda (a b)
             (> (overlay-get a 'point)
                (overlay-get b 'point)))
           (hel-all-fake-cursors)))

(defun hel-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (hash-table-count hel--cursors-table)))

(defun hel-any-fake-cursors-p ()
  "Return non-nil if there are fake cursors in the buffer."
  (not (hash-table-empty-p hel--cursors-table)))

;;; The cursors as values

;; A cursor value is a list (ID RANGE DIRECTION ACTIVE).  ID is 0 for the
;; main cursor, the key of the fake cursor in `hel--cursors-table'
;; otherwise, and nil for a cursor that does not exist yet.  RANGE and
;; DIRECTION are the selection of the cursor, as `hel-selection' returns
;; it.  ACTIVE is t when that selection is the active region of the
;; cursor, and nil when it is the cursor character.
;;
;; Code that works on all cursors at once reads them with
;; `hel-cursor-selections', computes a new list of values, and makes the
;; cursors match it with `hel-set-cursor-selections'.

(defun hel-cursor-selections ()
  "Return the values of all cursors, sorted by the beginning of their ranges.
Each value is (ID RANGE DIRECTION ACTIVE).  ID is 0 for the main cursor
and the key of the fake cursor in `hel--cursors-table' for the others.
RANGE and DIRECTION are the selection of the cursor, as `hel-selection'
returns it, and ACTIVE is t when that selection is the active region of
the cursor, nil when it is the cursor character.
`hel-set-cursor-selections' takes the returned list."
  (sort (cons (hel--main-cursor-value)
              (mapcar #'hel--fake-cursor-value (hel-all-fake-cursors)))
        (lambda (a b)
          (< (car (cadr a)) (car (cadr b))))))

(defun hel--main-cursor-value ()
  "Return the value (0 RANGE DIRECTION ACTIVE) of the main cursor."
  (hel--cursor-value 0 (point) (mark t) (region-active-p)))

(defun hel--fake-cursor-value (cursor)
  "Return the value (ID RANGE DIRECTION ACTIVE) of the fake CURSOR."
  (hel--cursor-value (overlay-get cursor 'id)
                     (marker-position (overlay-get cursor 'point))
                     (marker-position (overlay-get cursor 'mark))
                     (overlay-get cursor 'mark-active)))

(defun hel--cursor-value (id point mark active)
  "Return the value of the cursor with ID, POINT, MARK and ACTIVE.
ACTIVE is non-nil when the region of the cursor is active.  ACTIVE of
the value is t only when the selection is that region: an empty region
reads as no region, because the selection of the cursor is then its
cursor character, and the writer could not tell it from a selection of
one character."
  (let ((active (and active (/= point mark) t)))
    `(,id ,@(hel--selection-from point mark active) ,active)))

(defun hel-set-cursor-selections (cursors)
  "Make the cursors of the buffer match CURSORS, a list of cursor values.
Move the main cursor and every fake cursor whose ID is in CURSORS,
create a fake cursor for each value with ID nil, and delete the other
fake cursors.  A fake cursor keeps its kill ring and the other variables
of `hel-fake-cursor-variables'.  Each value is (ID RANGE DIRECTION
ACTIVE), as `hel-cursor-selections' returns it."
  (let ((ids (make-hash-table :size (length cursors))))
    (dolist (value cursors)
      (puthash (car value) t ids))
    (dolist (cursor (hel-all-fake-cursors))
      (unless (gethash (overlay-get cursor 'id) ids)
        (hel--delete-fake-cursor cursor))))
  (-each cursors #'hel--set-cursor-selection)
  (hel-auto-multiple-cursors-mode))

(defun hel--set-cursor-selection (value)
  "Make one cursor match VALUE, a cursor value (ID RANGE DIRECTION ACTIVE).
ID 0 is the cursor that runs the command: point and the mark.  A fake
cursor with ID moves; when ID is nil, or no fake cursor has it, a new
fake cursor is made.  A cursor that already has this value stays as it
is, so its mark keeps its place even when the selection does not need
it there."
  (-let* (((id range dir active) value)
          ((begin . end) range)
          ((head . anchor) (if (< 0 dir) (cons end begin) (cons begin end))))
    (if (eql id 0)
        (unless (equal value (hel--main-cursor-value))
          (if active
              (hel-select range dir)
            (hel--set-cursor-char head dir)))
      (let ((cursor (and id (gethash id hel--cursors-table))))
        (cond ((null cursor)
               ;; A new cursor stores `mark-active' and draws itself.
               (let ((mark-active active))
                 (if id
                     ;; A cursor of an undo step or of `g v' that was
                     ;; deleted since keeps its ID.
                     (hel--create-fake-cursor-1 id head anchor)
                   (hel-create-fake-cursor head anchor))))
              ((not (equal value (hel--fake-cursor-value cursor)))
               (overlay-put cursor 'mark-active active)
               (hel-move-fake-cursor cursor head anchor)))))))

(defun hel-select-ranges (regions direction)
  "Select the first of REGIONS with the current cursor, the others with new ones.
REGIONS is a list of (BEGIN . END) cons-cells.  DIRECTION is 1 or -1.
The other cursors stay as they are."
  (hel--set-cursor-selection (list 0 (car regions) direction t))
  (dolist (region (cdr regions))
    (hel--set-cursor-selection (list nil region direction t))))

;;; Executing commands for real and fake cursors

(defmacro hel-save-window-scroll (&rest body)
  "Save the window scroll position, evaluate BODY, restore it."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (win-start win-hscroll)
    `(let ((,win-start (copy-marker (window-start)))
           (,win-hscroll (window-hscroll)))
       ,@body
       (set-window-start nil ,win-start :noforce)
       (set-window-hscroll nil ,win-hscroll)
       (set-marker ,win-start nil))))

(defmacro hel-save-excursion (&rest body)
  "Like `save-excursion' but additionally save and restore all
the data needed for multiple cursors functionality."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (state)
    `(let ((,state (hel--conserve-main-cursor-state)))
       (save-excursion ,@body)
       (hel--restore-main-cursor-state ,state))))

(defun hel--conserve-main-cursor-state ()
  (let ((state (list :point (copy-marker (point) t)
                     :mark (copy-marker (mark-marker)))))
    (dolist (var hel-fake-cursor-variables)
      (if (boundp var)
          (cl-callf plist-put state var (symbol-value var))))
    state))

(defun hel--restore-main-cursor-state (state)
  (goto-char (let ((pnt (plist-get state :point)))
               (prog1 (marker-position pnt)
                 (set-marker pnt nil))))
  (set-marker (mark-marker)
              (let ((mrk (plist-get state :mark)))
                (prog1 (marker-position mrk)
                  (set-marker mrk nil))))
  (dolist (var hel-fake-cursor-variables)
    (if (boundp var)
        (set var (plist-get state var)))))

(defmacro hel-with-fake-cursor (cursor &rest body)
  "Move point to the fake CURSOR, restore the environment from it,
evaluate BODY, update fake CURSOR."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((hel-executing-command-for-fake-cursor t))
     (hel--restore-cursor-state ,cursor)
     (unwind-protect
         (progn ,@body)
       (hel-move-fake-cursor ,cursor (point) (mark t) :update))))

(defmacro hel-with-each-cursor (&rest body)
  "Evaluate BODY for all cursors: real and fake ones."
  (declare (indent 0) (debug t))
  ;; First collect fake cursors because BODY can create new cursors,
  ;; and we want it to be executed only for original ones.
  `(let ((cursors (if hel-multiple-cursors-mode
                      (hel-all-fake-cursors))))
     ;; Main cursor
     ,@body
     ;; Fake cursors
     (when cursors
       (hel-save-window-scroll
         (hel-save-excursion
           (dolist (cursor cursors)
             (hel-with-fake-cursor cursor
               ,@body)))))))

(defun hel-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for all cursors: real and fake ones."
  (hel--call-interactively command)
  (hel--execute-command-for-all-fake-cursors command)
  (when (hel--merge-cursors-p command)
    (hel-merge-overlapping-cursors))
  (setq hel--input-cache nil))

(defun hel--execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each fake cursor."
  (when hel-multiple-cursors-mode
    (cond ((and (symbolp command)
                (get command 'hel-unsupported))
           (message "%S is not supported with multiple cursors" command))
          ((or
            ;; If it's a lambda, we can't know if it's supported or not —
            ;; so go ahead and assume it's ok.
            (not (symbolp command))
            ;; If command has `multiple-cursors' property assigned — use it,
            ;; else promt user and permanently store the decision.
            (if-let* ((val (plist-member (symbol-plist command) 'multiple-cursors)))
                (cadr val)
              (hel--prompt-for-unknown-command command)))
           (hel-save-window-scroll
             (hel-save-excursion
               ;; `syntax-ppss' reuses an earlier parse only for a position
               ;; after it. In buffer order every cursor finds such a parse;
               ;; in reverse order none does, and each cursor parses the
               ;; buffer from its start.
               (dolist (cursor (hel-all-fake-cursors :sort))
                 (hel-with-fake-cursor cursor
                   (hel--call-interactively command)))))))))

(defun hel--call-interactively (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for fake cursor."
  (unless (eq command 'ignore)
    (let ((this-command command))
      (call-interactively command)))
  ;; (setq this-command command)
  ;; ;; (ignore-errors)
  ;; (run-hooks 'pre-command-hook)
  ;; (unless (eq this-command 'ignore)
  ;;   (call-interactively command))
  ;; (run-hooks 'post-command-hook)
  ;; (when deactivate-mark (deactivate-mark))
  )

(defmacro hel-with-real-cursor-as-fake (&rest body)
  "Temporarily convert real cursor into fake-cursor with ID 0.
Restore it after BODY evaluation if it is still alive."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (real-cursor)
    `(let ((,real-cursor (hel--create-fake-cursor-1 0 (point) (mark t))))
       (unwind-protect (progn ,@body)
         (cond ((hel-overlay-live-p ,real-cursor)
                (hel-restore-point-from-fake-cursor ,real-cursor))
               ((hel-any-fake-cursors-p)
                (hel-restore-point-from-fake-cursor (hel-first-fake-cursor))))
         (hel-auto-multiple-cursors-mode)
         (hel-update-cursor)))))

;;; Merge overlapping regions

(defun hel--merge-cursors-p (command)
  "Return non-nil if regions need to be merged after COMMAND."
  (and hel-multiple-cursors-mode
       (cond ((symbolp command)
              (let ((val (get command 'merge-selections)))
                (cond ((symbolp val)
                       (symbol-value val))
                      ((functionp val)
                       (funcall val)))))
             ((functionp command) ;; COMMAND is a lambda
              t))))

(defun hel-merge-overlapping-cursors ()
  "Merge the cursors whose selections overlap into one cursor each.
When a group of overlapping cursors holds the main cursor, the main
cursor survives with its own state.  Otherwise the first cursor of the
group survives.  The other cursors of the group are deleted."
  (let* ((cursors (hel-cursor-selections))
         (merged (hel--merge-cursors cursors)))
    ;; The merge only joins cursors, so a list of the same length is
    ;; the same list.
    (unless (length= cursors (length merged))
      (hel-set-cursor-selections merged))))

(defun hel--merge-cursors (cursors)
  "Merge the cursors whose selections overlap.
CURSORS is a list of cursor values (ID RANGE DIRECTION ACTIVE), sorted
by the beginning of their ranges.  Return the merged list in the same
order.  Each range of the result covers a group of overlapping ranges.
The main cursor survives in its group, and otherwise the first cursor
of the group does; the survivor keeps its ID and direction.  The region
of the result is active when the region of any cursor of the group is."
  (nreverse
   (-reduce-from (lambda (merged cursor)
                   (if (and merged
                            (hel--ranges-overlap-p (cadr (car merged))
                                                   (cadr cursor)))
                       (cons (hel--merge-two-cursors (car merged) cursor)
                             (cdr merged))
                     (cons cursor merged)))
                 nil cursors)))

(defun hel--ranges-overlap-p (a b)
  "Return non-nil if the range B overlaps the range A.
A begins at or before B.  Equal ranges overlap, even when empty, but
ranges that only touch do not."
  (or (< (car b) (cdr a))
      (equal a b)))

(defun hel--merge-two-cursors (a b)
  "Return the cursor that covers the selections of the cursors A and B.
The main cursor survives; otherwise A does.  The survivor keeps its ID
and direction."
  (-let* (((id-a (begin-a . end-a) dir-a active-a) a)
          ((id-b (begin-b . end-b) dir-b active-b) b)
          (b-survives? (eql id-b 0)))
    (list (if b-survives? id-b id-a)
          (cons (min begin-a begin-b) (max end-a end-b))
          (if b-survives? dir-b dir-a)
          (or active-a active-b))))

;;; Integration with other Emacs functionalities

(defmacro hel-cache-input (fn-name)
  "Advice function to cache users input to use it with all cursors.

This macro wraps functions in around advice that caches the user's
response so it can be reused across all cursors.

FN-NAME must be an interactive function that takes PROMPT as its first argument,
like `read-char' or `read-from-minibuffer'. The PROMPT argument will be used as
a hash key to distinguish between different calls to FN-NAME within the same
command. Calls with equal PROMPT or without it would be indistinguishable."
  `(hel-define-advice ,fn-name (:around (orig-fun &rest args) hel-cache-input)
     "Cache the users input to use it with multiple cursors."
     (if hel-multiple-cursors-mode
         (let* (;; Use PROMPT argument as a hash key to distinguish different
                ;; calls of `read-char' like functions within one command.
                (prompt (car-safe args))
                (key (list ,(symbol-name fn-name) prompt)))
           (with-memoization (alist-get key hel--input-cache nil nil #'equal)
             (apply orig-fun args)))
       ;; else
       (apply orig-fun args))))

(defun hel-maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry of each cursor to `killed-rectangle',
unless they all are equal. You can paste them later with `yank-rectangle'."
  (when hel-multiple-cursors-mode
    (let ((entries (-map (-lambda ((id))
                           (car-safe (if (eql id 0)
                                         kill-ring
                                       (-> (gethash id hel--cursors-table)
                                           (overlay-get 'kill-ring)))))
                         (hel-cursor-selections))))
      (unless (hel-all-elements-are-the-same-p entries)
        (setq killed-rectangle entries)))))

(defun hel-set-unsupported-command (symbol)
  "Prevent command from being executed while there are multiple cursors."
  (hel-advice-add symbol :around #'hel--unsupported-a)
  (put symbol 'hel-unsupported t))

(defun hel--unsupported-a (orig-fun &rest args)
  "Prevent command from being executed while there are multiple cursors."
  (unless (and hel-multiple-cursors-mode
               (called-interactively-p 'any))
    (apply orig-fun args)))

(defun hel-set-multiple-cursors-command (symbol)
  "Mark command to be executed for all cursors."
  (put symbol 'multiple-cursors t))

(defun hel-set-single-cursor-command (symbol)
  "Mark command to be executed only for main cursor."
  (put symbol 'multiple-cursors nil))

;; Execute following commands for ALL cursor.
(mapc #'hel-set-multiple-cursors-command
      '(append-next-kill
        back-to-indentation
        backward-char
        backward-delete-char-untabify
        backward-kill-word
        backward-list
        backward-paragraph
        backward-sexp
        backward-word
        beginning-of-line
        capitalize-word
        comment-dwim             ;; gc
        default-indent-new-line
        delete-backward-char
        delete-blank-lines
        delete-char
        delete-forward-char
        downcase-word
        end-of-line
        eval-defun
        eval-expression
        exchange-point-and-mark
        fill-region              ;; gq
        forward-char
        forward-list
        forward-paragraph
        forward-sexp
        forward-word
        hippie-expand
        indent-region            ;; =
        indent-rigidly-left      ;; >
        indent-rigidly-right     ;; <
        insert-char              ;; C-x 8 RET
        join-line
        just-one-space
        keyboard-quit            ;; C-g
        kill-line
        kill-region
        kill-ring-save
        kill-whole-line
        kill-word
        left-char
        left-word
        move-beginning-of-line
        move-end-of-line
        newline
        newline-and-indent
        next-line
        org-cycle
        org-delete-backward-char
        org-force-self-insert
        org-indent-region
        org-metaup
        org-return
        org-self-insert-command
        previous-line
        quoted-insert            ;; C-q
        repeat
        right-char
        right-word
        self-insert-command
        set-mark-command
        transpose-chars
        transpose-lines
        transpose-paragraphs
        transpose-regions
        transpose-sexps
        upcase-word
        yank
        yank-pop
        zap-to-char))

;; Execute following commands only for MAIN cursor.
(mapc #'hel-set-single-cursor-command
      '(browse-url-at-point      ;; gx
        delete-other-windows
        describe-bindings
        describe-function
        describe-mode
        describe-prefix-bindings
        digit-argument
        edebug-next-mode
        eval-buffer
        exit-minibuffer
        find-file-at-point       ;; gf
        hel-normal-state         ;; <escape>
        kill-buffer-and-window
        minibuffer-complete-and-exit
        mouse-drag-region
        mouse-set-point
        mwheel-scroll
        negative-argument
        other-window
        quit-window
        repeat-complex-command
        save-buffer
        scroll-down-command
        scroll-up-command
        split-window-below
        split-window-right
        tab-bar-mouse-down-1
        tab-next
        tab-previous
        toggle-frame-fullscreen
        toggle-input-method
        top-level
        undefined
        undo
        undo-redo
        undo-fu-only-redo
        undo-fu-only-undo
        undo-tree-redo
        undo-tree-undo
        universal-argument
        universal-argument-more
        view-echo-area-messages
        windmove-down
        windmove-left
        windmove-right
        windmove-up))

;;; Whitelists

(defun hel--prompt-for-unknown-command (command)
  "Ask the user whether the COMMAND should be executed for all cursors or not,
and remember the choice.

Return t if COMMMAND should be executed for all cursors."
  (let ((for-all? (ignore-error quit ;; treat "C-g" as answer "no"
                    (y-or-n-p (format "Do %S for all cursors?" command)))))
    (put command 'multiple-cursors for-all?)
    (if for-all?
        (push command hel-commands-to-run-for-all-cursors)
      (push command hel-commands-to-run-once))
    (hel-save-whitelists-into-file)
    for-all?))

(defun hel-load-whitelists ()
  "Load `hel-whitelist-file' file if not yet."
  (unless hel--whitelist-file-loaded
    (load hel-whitelist-file 'noerror 'nomessage)
    (setq hel--whitelist-file-loaded t)
    (-each hel-commands-to-run-for-all-cursors #'hel-set-multiple-cursors-command)
    (-each hel-commands-to-run-once #'hel-set-single-cursor-command)))

(defun hel--dump-whitelist (list-symbol)
  "Insert (setq \\='LIST-SYMBOL LIST-VALUE) into current buffer."
  (cl-symbol-macrolet ((value (symbol-value list-symbol)))
    (insert "(setq " (symbol-name list-symbol) "\n"
            "      '(")
    (newline-and-indent)
    (set list-symbol (-> value (sort (lambda (x y)
                                       (string< (symbol-name x)
                                                (symbol-name y))))))
    (-each value (lambda (cmd)
                   (insert (format "%S" cmd))
                   (newline-and-indent)))
    (insert "))")
    (newline)))

(defun hel-save-whitelists-into-file ()
  "Save users preferences which commands to execute for one cursor
and which for all to `hel-whitelist-file' file."
  (with-temp-file hel-whitelist-file
    (emacs-lisp-mode)
    (insert ";; -*- mode: emacs-lisp; lexical-binding: t -*-")
    (newline)
    (insert ";; This file is automatically generated by Hel.")
    (newline)
    (insert ";; It keeps track of your preferences for running commands with multiple cursors.")
    (newline)
    (newline)
    (hel--dump-whitelist 'hel-commands-to-run-for-all-cursors)
    (newline)
    (hel--dump-whitelist 'hel-commands-to-run-once)))

;;; .
(provide 'hel-multiple-cursors-core)
;;; hel-multiple-cursors-core.el ends here
