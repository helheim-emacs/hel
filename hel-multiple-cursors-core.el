;;; hel-multiple-cursors-core.el -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.10.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The core functionality for multiple cursors. The code is inspired by
;; `multiple-cursors.el' package from Magnar Sveen.
;;
;; How multiple cursors works internally. Command is firstly executed for
;; real cursor by Emacs command loop. Then in `post-command-hook' it executed
;; for all fake cursors. Fake cursor is an overlay that emulates cursor and
;; stores inside point, mark, kill-ring and some other variables (full list
;; is in `hel-fake-cursor-specific-vars'). Executing command for fake cursor
;; looks as follows: set point and mark to positions saved in fake cursor
;; overlay, restore all variables from it, execute command in this environment,
;; store point, mark and new state into fake cursor overlay.
;;
;; How command will be executed is controlled by the `multiple-cursors' symbol
;; property with three cases:
;; - t         for all cursors
;; - nil       property explicitly set: only for the main cursor
;; - no value  property not present: prompt the user and permanently store
;;             the choice in `hel-whitelist-file'
;;
;; ID 0 is always corresponding to real cursor.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'subr-x)
(require 'rect)
(require 'hel-common)

;;; Undo

(hel-defvar-local hel--in-single-undo-step nil
  "Non-nil while we are in the single undo step.")

(defun hel--single-undo-step-beginning ()
  "Initiate atomic undo step.
All following buffer modifications are grouped together as a single
action. The step is terminated with `hel--single-undo-step-end'."
  (unless (or hel--in-single-undo-step
              (hel-undo-command-p this-command)
              (eq buffer-undo-list t))
    (setq hel--in-single-undo-step t)
    (unless (null (car-safe buffer-undo-list))
      (undo-boundary))
    (setq hel--undo-list-pointer buffer-undo-list)
    (hel--push-undo-boundary-1)))

(defun hel--single-undo-step-end ()
  "Finalize atomic undo step started by `hel--single-undo-step-beginning'."
  (when hel--in-single-undo-step
    (hel--push-undo-boundary-2)
    (unless (eq buffer-undo-list hel--undo-list-pointer)
      (let ((undo-list buffer-undo-list))
        (while (and (consp undo-list)
                    (eq (car undo-list) nil))
          (setq undo-list (cdr undo-list)))
        (let ((equiv (gethash (car undo-list)
                              undo-equiv-table)))
          ;; Remove undo boundaries (nil elements) from `buffer-undo-list'
          ;; withing current undo step. Also remove number entries -- they
          ;; move point during undo, and we handle cursors positions manually
          ;; to synchronize real cursor with fake ones.
          (setq undo-list (hel-destructive-filter
                           (lambda (i) (or (numberp i) (null i)))
                           undo-list
                           hel--undo-list-pointer))
          ;; Restore "undo" status of the tip of `buffer-undo-list'.
          (when equiv
            (puthash (car undo-list) equiv
                     undo-equiv-table)))
        (setq buffer-undo-list undo-list)))
    (setq hel--in-single-undo-step nil
          hel--undo-list-pointer nil)))

(defun hel--push-undo-boundary-1 ()
  (setq hel--undo-boundary
        `(apply hel--undo-step-end ,(hel-cursors-positions)))
  (push hel--undo-boundary buffer-undo-list))

(defun hel--push-undo-boundary-2 ()
  (when hel--undo-boundary
    (let ((undo-list buffer-undo-list))
      (while (and (consp undo-list)
                  (eq (car undo-list) nil))
        (pop undo-list))
      (if (equal (car undo-list) hel--undo-boundary)
          (pop undo-list)
        ;; else
        (push `(apply hel--undo-step-start ,(hel-cursors-positions))
              undo-list))
      (setq hel--undo-boundary nil
            buffer-undo-list undo-list))))

(defun hel--undo-step-start (cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the first one from a pair of functions:
`hel--undo-step-start' and `hel--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors positions and regions after undo/redo step.

CURSORS-POSITIONS is an alist returned by `hel-cursors-positions' function."
  (push `(apply hel--undo-step-end ,cursors-positions)
        buffer-undo-list))

(defun hel--undo-step-end (cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the second one from a pair of functions:
`hel--undo-step-start' and `hel--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors positions and regions after undo/redo step.

CURSORS-POSITIONS is an alist returned by `hel-cursors-positions' function."
  (hel-place-cursors cursors-positions)
  (push `(apply hel--undo-step-start ,cursors-positions)
        buffer-undo-list))

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
    (let ((cursor (hel--set-cursor-overlay nil point)))
      (overlay-put cursor 'id id)
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (hel--store-cursor-state cursor point mark)
      (hel--set-fake-region-overlay cursor)
      (puthash id cursor hel--cursors-table)
      cursor)))

(defun hel--delete-all-fake-cursors ()
  "Remove all fake cursors overlays form current buffer.
It is likely that you need `hel-disable-multiple-cursors-mode', not this one."
  (when hel--max-cursors-original
    (setq hel-max-cursors-number hel--max-cursors-original
          hel--max-cursors-original nil))
  (-each (hel-all-fake-cursors) #'hel--delete-fake-cursor))

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
  (hel--set-cursor-overlay cursor point)
  (hel--set-fake-region-overlay cursor)
  cursor)

(defun hel-delete-fake-cursor (cursor)
  "Delete fake CURSOR and disable `hel-multiple-cursors-mode' if no
more fake cursors are remaining."
  (hel--delete-fake-cursor cursor)
  (hel-auto-multiple-cursors-mode))

(defun hel--set-cursor-overlay (cursor pos)
  "Move or create fake CURSOR overlay at position POS.
If CURSOR is nil — create new fake cursor overlay at POS.
Return CURSOR."
  (save-excursion
    (goto-char pos)
    ;; Special case for end of line, because overlay over
    ;; a newline highlights the entire width of the window.
    (setq cursor (cond ((and cursor (eolp))
                        (move-overlay cursor pos pos))
                       (cursor
                        (move-overlay cursor pos (1+ pos)))
                       ((eolp)
                        (make-overlay pos pos nil t nil))
                       (t
                        (make-overlay pos (1+ pos) nil t nil))))
    (let ((face (cond (hel--extend-selection
                       'hel-extend-selection-cursor)
                      (hel-insert-state
                       'hel-insert-state-fake-cursor)
                      (t
                       'hel-normal-state-fake-cursor))))
      (cond ((and (display-graphic-p)
                  hel-match-fake-cursor-style
                  (hel-cursor-is-bar-p))
             (overlay-put cursor 'face nil)
             (overlay-put cursor 'before-string
                          (propertize hel-bar-fake-cursor
                                      'face `(,face
                                              (:height ,(window-default-font-height)))))
             (overlay-put cursor 'after-string nil))
            ((eolp)
             (overlay-put cursor 'face nil)
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'after-string (propertize " " 'face face)))
            (t
             (overlay-put cursor 'face face)
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'after-string nil))))
    cursor))

(defun hel--set-fake-region-overlay (cursor)
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
                         (overlay-put 'priority 1)))
          (overlay-put cursor 'fake-region region))
      ;; else
      (hel--delete-fake-region-overlay cursor))))

(defun hel--delete-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (remhash (overlay-get cursor 'id) hel--cursors-table)
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (hel--delete-fake-region-overlay cursor)
  (delete-overlay cursor))

(defun hel--delete-fake-region-overlay (cursor)
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
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (overlay-put overlay var (symbol-value var))))
  overlay)

(defun hel-update-fake-cursor-state (cursor)
  "Update variables stored in fake CURSOR."
  (dolist (var hel-fake-cursor-specific-vars)
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
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (set var (overlay-get overlay var))))
  (hel--delete-fake-region-overlay overlay)
  (delete-overlay overlay))

(defun hel-hide-fake-cursor (cursor)
  "Disable the fake-CURSOR visibility in the buffer without deleting it."
  (hel--delete-fake-region-overlay cursor)
  (delete-overlay cursor)
  cursor)

(defun hel-show-fake-cursor (cursor)
  "Restore fake-CURSOR visibility if it was previously hidden with
`hel-hide-fake-cursor'."
  (let ((point (overlay-get cursor 'point))
        (mark (overlay-get cursor 'mark)))
    (hel--set-cursor-overlay cursor point)
    (hel--set-fake-region-overlay cursor)))

(defun hel-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun hel-fake-region-p (overlay)
  "Return t if an OVERLAY is a fake region."
  (eq (overlay-get overlay 'type) 'fake-region))

;;; Access fake cursors

(defun hel-all-fake-cursors (&optional sort)
  "Return list with all fake cursors in current buffer.
If SORT is non-nil sort cursors in order they are located in buffer."
  (let ((cursors (hash-table-values hel--cursors-table)))
    (if sort
        (sort cursors (lambda (c1 c2)
                        (< (overlay-get c1 'point)
                           (overlay-get c2 'point))))
      cursors)))

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

(defun hel-cursors-positions ()
  "Return alist with positions data of all cursors.
Alist containes cons cells:

    (ID . (POINT MARK))

MARK is nil if cursor has no region.

Real cursor has ID 0 and is the first element (`car') of the list."
  (let (alist)
    (when hel-multiple-cursors-mode
      (dolist (cursor (hel-all-fake-cursors))
        (push (list (overlay-get cursor 'id) ;; id
                    (marker-position (overlay-get cursor 'point)) ;; point
                    (if (overlay-get cursor 'mark-active)
                        (marker-position (overlay-get cursor 'mark)))) ;; mark
              alist)))
    ;; Real cursor
    (push (list 0 (point) (if mark-active (marker-position (mark-marker))))
          alist)
    alist))

(defun hel-place-cursors (cursors-positions)
  "Setup all cursors according to CURSORS-POSITIONS.
CURSORS-POSITIONS is an alist of the form that `hel-cursors-positions' returns."
  (maphash (lambda (id cursor)
             (unless (assoc id cursors-positions #'eql)
               (hel--delete-fake-cursor cursor)))
           hel--cursors-table)
  (cl-loop for (id point mark) in cursors-positions
           do (pcase id
                (0 (hel-set-region mark point))
                (_ (let ((mark-active (not (null mark))))
                     (if-let* ((cursor (gethash id hel--cursors-table)))
                         (hel-move-fake-cursor cursor point mark :update)
                       (hel--create-fake-cursor-1 id point mark))))))
  (hel-auto-multiple-cursors-mode))

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
    (dolist (var hel-fake-cursor-specific-vars)
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
  (dolist (var hel-fake-cursor-specific-vars)
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
               (dolist (cursor (hel-all-fake-cursors))
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

;;; Multiple cursors minor mode

(define-minor-mode hel-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need to activate it manually: it is activated automatically when you create
first fake cursor with `hel-create-fake-cursor', and disabled when you
delete last one with `hel-delete-fake-cursor'."
  :global nil
  :interactive nil
  :keymap (make-sparse-keymap)
  (if hel-multiple-cursors-mode
      (hel--disable-minor-modes-incompatible-with-multiple-cursors)
    ;; else
    (when (hel-any-fake-cursors-p)
      (setq hel--cursors-positions-history (hel-cursors-positions))
      (hel--delete-all-fake-cursors))
    (hel--enable-minor-modes-incompatible-with-multiple-cursors))
  (hel-update-active-keymaps))

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

(defun hel-multiple-cursors--indicator ()
  (when hel-multiple-cursors-mode
    (format hel-multiple-cursors-mode-line-indicator
            (hel-number-of-cursors))))

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
    (-each hel-commands-to-run-for-all-cursors
      (lambda (command)
        (put command 'multiple-cursors t)))
    (-each hel-commands-to-run-once
      (lambda (command)
        (put command 'multiple-cursors nil)))))

(defun hel--dump-whitelist (list-symbol)
  "Insert (setq \\='LIST-SYMBOL LIST-VALUE) into current buffer."
  (cl-symbol-macrolet ((value (symbol-value list-symbol)))
    (insert "(setq " (symbol-name list-symbol) "\n"
            "      '(")
    (newline-and-indent)
    (set list-symbol (-> value
                         (sort (lambda (x y)
                                 (string-lessp (symbol-name x)
                                               (symbol-name y))))))
    (mapc (lambda (cmd)
            (insert (format "%S" cmd))
            (newline-and-indent))
          value)
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
  "Merge overlapping cursors."
  (if (use-region-p)
      (hel--merge-overlapping-regions)
    (hel--merge-overlapping-points)))

(defun hel--merge-overlapping-points ()
  "Merge cursors at the same positions."
  (let* ((cursors (hel-all-fake-cursors t))
         (cursor (car cursors))
         (point (overlay-get cursor 'point))
         pos)
    (dolist (c (cdr cursors))
      (setq pos (overlay-get c 'point))
      (if (= point pos)
          (hel--delete-fake-cursor c)
        (setq cursor c
              point pos))))
  (-some-> (hel-fake-cursor-at (point))
    (hel--delete-fake-cursor))
  (hel-auto-multiple-cursors-mode))

(defun hel--merge-overlapping-regions ()
  "Merge overlapping regions."
  (let ((dir (hel-region-direction)))
    (dolist (group-or-overlapping-regions (hel--overlapping-regions))
      (-let (for-deletion
             real-cursor?
             ((id beg end) (car group-or-overlapping-regions)))
        ;; i - region ID
        ;; b - region beginning
        ;; e - region end
        (cl-loop for (i b e) in (cdr group-or-overlapping-regions) do
                 (when (< b beg)
                   (setq beg b)
                   (when (< dir 0)
                     (push id for-deletion)
                     (setq id i)))
                 (when (> e end)
                   (setq end e)
                   (when (< 0 dir)
                     (push id for-deletion)
                     (setq id i)))
                 (cond ((eql i 0)
                        (setq real-cursor? t))
                       ((/= id i)
                        (push i for-deletion))))
          (pcase id
            (0 (hel-set-region beg end dir))
            (_ (when-let* ((cursor (gethash id hel--cursors-table)))
                 (cond (real-cursor?
                        (hel-restore-point-from-fake-cursor cursor)
                        (hel-set-region beg end dir))
                       ((< dir 0)
                        (hel-move-fake-cursor cursor beg end))
                       (t
                        (hel-move-fake-cursor cursor end beg))))))
          (dolist (id for-deletion)
            (when-let* ((cursor (gethash id hel--cursors-table)))
              (hel--delete-fake-cursor cursor)))))
    (hel-auto-multiple-cursors-mode)))

(defun hel--overlapping-regions ()
  "Return the list of groups, where each group is a list of
cons cells (ID . (START END)) denoting fake cursor ID and its
region bounds. Inside each group, all regions are overlapping
and sorted by starting position. ID 0 coresponds to the real
cursor."
  (let ((alist (hel--regions-ranges))
        result
        current-group
        (current-end (point-min)))
    (dolist (item alist)
      (-let [(_ start end) item]
        (if (< start current-end)
            (push item current-group)
          ;; else
          (when (length> current-group 1)
            (push (nreverse current-group) result))
          (setq current-group (list item)))
        (setq current-end (max end current-end))))
    (when (length> current-group 1)
      (push (nreverse current-group) result))
    (nreverse result)))

(defun hel--regions-ranges ()
  "Return the alist with cons cells (ID . (START END)).
\(START END) are bounds of regions. Alist is sorted by START.
ID 0 coresponds to the real cursor."
  (let* ((alist (cons
                 ;; Append real cursor with ID 0
                 `(0 ,(region-beginning) ,(region-end))
                 (mapcar (lambda (cursor)
                           (let* ((id  (overlay-get cursor 'id))
                                  (pnt (overlay-get cursor 'point))
                                  (mrk (overlay-get cursor 'mark))
                                  (start (min pnt mrk))
                                  (end   (max pnt mrk)))
                             `(,id ,start ,end)))
                         (hel-all-fake-cursors)))))
    (sort alist (lambda (a b)
                  (< (-second-item a) (-second-item b))))))

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

(defmacro hel-unsupported-command (command)
  "Adds command to list of unsupported commands and prevents it
from being executed when `hel-multiple-cursors-mode' is active."
  `(progn
     (put ',command 'hel-unsupported t)
     (hel-define-advice ,command (:around (orig-fun &rest args)
                                          hel-unsupported)
       "Don't execute an unsupported command while multiple cursors are active."
       (unless (and hel-multiple-cursors-mode
                    (called-interactively-p 'any))
         (apply orig-fun args)))))

;; Execute following commands for ALL cursor.
(mapc (lambda (command)
        (put command 'multiple-cursors t))
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
(mapc (lambda (command)
        (put command 'multiple-cursors nil))
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

;;; .
(provide 'hel-multiple-cursors-core)
;;; hel-multiple-cursors-core.el ends here
