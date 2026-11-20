;;; hel-multiple-cursors-core.el --- Multiple cursors -*- lexical-binding: t -*-
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
;; How multiple cursors work.  A buffer has one main cursor, whose state
;; is point, the mark and the variables of `hel-cursor-variables'
;; in Emacs, and any number of secondary cursors, each an overlay that holds
;; the same state.  Emacs runs a command for the main cursor.  Then
;; `post-command-hook' runs it for each secondary cursor: it loads the
;; cursor into Emacs with `hel--load-cursor', runs the command, and saves
;; the cursor back with `hel--save-cursor'.
;;
;; How command will be executed is controlled by the `multiple-cursors' symbol
;; property with three cases:
;; - t         for all cursors
;; - nil       property explicitly set: only for the main cursor
;; - no value  property not present: prompt the user and permanently store
;;             the choice in `hel-whitelist-file'
;;
;; ID 0 always corresponds to the main cursor.
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

(declare-function hel-draw-cursor "hel-core")
(declare-function hel--cursor-look "hel-core")
(declare-function hel--cursor-bounds "hel-core")
(declare-function hel--cursor-display "hel-core")

;;; Multiple cursors minor mode

(define-minor-mode hel-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need to activate it manually: it is activated automatically when you create
first secondary cursor with `hel-create-secondary-cursor', and disabled when you
delete last one with `hel-delete-secondary-cursor'."
  :interactive nil
  :keymap (make-sparse-keymap)
  (if hel-multiple-cursors-mode
      (hel--disable-minor-modes-incompatible-with-multiple-cursors)
    ;; else
    (when (hel-any-secondary-cursors-p)
      (setq hel--selections-history (hel-selections))
      (hel--delete-all-secondary-cursors))
    (hel--enable-minor-modes-incompatible-with-multiple-cursors)))

(defun hel-auto-multiple-cursors-mode ()
  "Enable `hel-multiple-cursors' if there are multiple cursors,
disable if only one."
  (when (xor hel-multiple-cursors-mode
             (hel-any-secondary-cursors-p))
    (hel-multiple-cursors-mode 'toggle)))

(defun hel-disable-multiple-cursors-mode ()
  "Remove all secondary cursors from the current buffer.
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

;;; Secondary cursor object

(defun hel--new-secondary-cursor-id ()
  "Return a new ID for a secondary cursor.
The ID lets undo find the cursor again."
  (cl-incf hel--secondary-cursor-last-used-id))

(defun hel-create-secondary-cursor (&optional range dir active id)
  "Create a secondary cursor and return it.
Without RANGE the new cursor is a copy of the main cursor with its
selection.  With RANGE and ACTIVE, the cursor selects RANGE in the
direction DIR.  With RANGE alone, it is a bare cursor at the beginning of
RANGE.  The cursor takes its kill ring and the other variables of
`hel-cursor-variables' from the main cursor.

ID names the cursor, as in a cursor value of `hel-selections'.  Without
ID the cursor gets a new one.  Turn on `hel-multiple-cursors-mode' when
it is off."
  (let ((cursor (hel--save-cursor (make-overlay (point) (point) nil t nil)))
        (id (or id (hel--new-secondary-cursor-id))))
    (overlay-put cursor 'id id)
    (overlay-put cursor 'type 'secondary-cursor)
    ;; More then the `show-paren-mode' highlights.
    (overlay-put cursor 'priority 1001)
    (puthash id cursor hel--cursors-table)
    (if range
        (hel--set-secondary-selection cursor range dir active)
      (hel--draw-secondary-cursor cursor))
    (unless hel-multiple-cursors-mode
      (hel-multiple-cursors-mode 1))
    cursor))

(defun hel--delete-all-secondary-cursors ()
  "Delete all secondary cursors of the current buffer.
`hel-multiple-cursors-mode' stays on: to turn it off as well, use
`hel-disable-multiple-cursors-mode'."
  (maphash (lambda (_ cursor)
             (hel--delete-secondary-cursor cursor))
           hel--cursors-table))

(defun hel-delete-secondary-cursor (cursor)
  "Delete the secondary CURSOR.
When it is the last one, turn off `hel-multiple-cursors-mode'."
  (hel--delete-secondary-cursor cursor)
  (hel-auto-multiple-cursors-mode))

(defun hel--draw-secondary-cursor (cursor)
  "Draw the secondary CURSOR and its region, and return CURSOR.
In Normal state the cursor is a box over the head character of its
selection.  In Insert state it is a bar at its point, the string
`hel-bar-secondary-cursor', and in a text terminal a box over the
character after point.  A box on a newline or at the end of the buffer
is a cell of one space."
  (-let* ((point (hel--secondary-cursor-point cursor))
          ((begin . end)
           (-> (hel--selection-from point
                                    (hel--secondary-cursor-mark cursor)
                                    (and hel-normal-state
                                         (overlay-get cursor 'mark-active)))
               (hel--cursor-bounds)))
          ((shape face) (hel--cursor-look nil)))
    (pcase shape
      ('box
       (move-overlay cursor begin end)
       (overlay-put cursor 'face (if (/= begin end) face))
       (overlay-put cursor 'display (if (/= begin end)
                                        (hel--cursor-display begin face)))
       (overlay-put cursor 'before-string nil)
       (overlay-put cursor 'after-string (if (= begin end)
                                             (propertize " " 'face face))))
      ('bar
       (move-overlay cursor point point)
       (overlay-put cursor 'face nil)
       (overlay-put cursor 'display nil)
       (overlay-put cursor 'before-string
                    (propertize hel-bar-secondary-cursor
                                'face `(,face (:height
                                               ,(window-default-font-height)))))
       (overlay-put cursor 'after-string nil))
      (_
       (move-overlay cursor point point)
       (overlay-put cursor 'face nil)
       (overlay-put cursor 'display nil)
       (overlay-put cursor 'before-string nil)
       (overlay-put cursor 'after-string nil))))
  (hel--draw-secondary-region cursor)
  cursor)

(defun hel--draw-secondary-region (cursor)
  "Draw the active region of the secondary CURSOR, or delete its drawing."
  (let ((beg (hel--secondary-cursor-point cursor))
        (end (hel--secondary-cursor-mark cursor)))
    (if (and (overlay-get cursor 'mark-active)
             (/= beg end))
        (if-let* ((region (overlay-get cursor 'secondary-region)))
            (move-overlay region beg end)
          ;; else
          (setq region (-doto (make-overlay beg end nil nil t)
                         (overlay-put 'face 'region)
                         (overlay-put 'type 'secondary-region)
                         ;; The same as Emacs native region overlay.
                         (overlay-put 'priority 100)))
          (overlay-put cursor 'secondary-region region))
      ;; else
      (hel--delete-secondary-region cursor))))

(defun hel--delete-secondary-cursor (cursor)
  "Delete the secondary CURSOR and its region."
  (remhash (overlay-get cursor 'id) hel--cursors-table)
  (hel--delete-secondary-region cursor)
  (hel--free-cursor cursor))

(defun hel--delete-secondary-region (cursor)
  "Delete the drawing of the region of the secondary CURSOR."
  (-some-> (overlay-get cursor 'secondary-region)
    (delete-overlay)))

(defun hel--save-cursor (overlay)
  "Copy the state of the cursor in Emacs into OVERLAY.
The state is point, the mark and the variables of
`hel-cursor-variables'.  Return OVERLAY."
  (let ((point (point))
        (mark (or (mark t) (point))))
    ;; Point and the mark are empty overlays, which follow the edits as
    ;; markers do.  Emacs walks every marker of the buffer on each edit,
    ;; but keeps the overlays in a tree.  Text inserted at point goes
    ;; before it, and text inserted at the mark goes after it.
    (if-let* ((position (overlay-get overlay 'point)))
        (move-overlay position point point)
      (overlay-put overlay 'point (make-overlay point point nil t t)))
    (if-let* ((position (overlay-get overlay 'mark)))
        (move-overlay position mark mark)
      (overlay-put overlay 'mark (make-overlay mark mark nil nil nil))))
  (dolist (var hel-cursor-variables)
    (if (boundp var)
        (overlay-put overlay var (symbol-value var))))
  overlay)

(defun hel--load-cursor (overlay)
  "Copy the state of the cursor in OVERLAY into Emacs."
  (goto-char (hel--secondary-cursor-point overlay))
  (set-marker (mark-marker) (hel--secondary-cursor-mark overlay))
  (dolist (var hel-cursor-variables)
    (if (boundp var)
        (set var (overlay-get overlay var)))))

(defun hel--free-cursor (overlay)
  "Delete OVERLAY, which holds the state of a cursor, and the positions in it."
  (-some-> (overlay-get overlay 'point) (delete-overlay))
  (-some-> (overlay-get overlay 'mark) (delete-overlay))
  (delete-overlay overlay))

(cl-defun hel--set-secondary-selection (cursor (begin . end) dir active)
  "Give the secondary CURSOR a new selection and draw it.
With ACTIVE, it is a selection between BEGIN and END in the direction
DIR: 1 or -1, or nil for at most one character, as in `hel-select'.
Without ACTIVE, it is a bare cursor at BEGIN."
  (when (and active (null dir) (< 1 (- end begin)))
    (error "No direction for a selection of %d characters" (- end begin)))
  (-let [(point . mark) (if (and active dir (< 0 dir))
                            (cons end begin)
                          (cons begin end))]
    (move-overlay (overlay-get cursor 'point) point point)
    (when active
      (move-overlay (overlay-get cursor 'mark) mark mark))
    (overlay-put cursor 'mark-active (and active t))
    (hel--draw-secondary-cursor cursor)))

(defun hel--secondary-cursor-point (cursor)
  "Return the position of point of the secondary CURSOR."
  (overlay-start (overlay-get cursor 'point)))

(defun hel--secondary-cursor-mark (cursor)
  "Return the position of the mark of the secondary CURSOR."
  (overlay-start (overlay-get cursor 'mark)))

(defun hel-make-cursor-main (cursor)
  "Make the secondary CURSOR the main cursor.
Load its state into Emacs and delete it.  The state of the main cursor
before the call is lost, so save it first to keep it as a secondary cursor."
  (hel--load-cursor cursor)
  (hel--delete-secondary-cursor cursor))

(defmacro hel-save-main-cursor (&rest body)
  "Evaluate BODY, then give the main cursor back its state.
The state is point, the mark, the variables of `hel-cursor-variables'
and the current buffer."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (main)
    `(let ((,main (hel--save-cursor (make-overlay (point) (point) nil t nil))))
       (unwind-protect
           (save-excursion ,@body)
         (hel--load-cursor ,main)
         (hel--free-cursor ,main)))))

(defmacro hel-with-secondary-cursor (cursor &rest body)
  "Evaluate BODY at the secondary CURSOR, and save the result into CURSOR.
In BODY, point, the mark and the variables of `hel-cursor-variables' are
those of CURSOR, so a command acts at CURSOR.  BODY replaces the state of
the main cursor, so call this macro inside `hel-save-main-cursor'."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((hel-executing-command-for-secondary-cursor t))
     (hel--load-cursor ,cursor)
     (hel--hide-secondary-cursor ,cursor)
     (unwind-protect
         (progn ,@body)
       (hel--save-cursor ,cursor)
       (hel--draw-secondary-cursor ,cursor))))

(defun hel--hide-secondary-cursor (cursor)
  "Delete the drawing of the secondary CURSOR and keep the cursor."
  (hel--delete-secondary-region cursor)
  (delete-overlay cursor)
  cursor)

(defun hel-secondary-cursor-p (overlay)
  "Return non-nil when OVERLAY is a secondary cursor."
  (eq (overlay-get overlay 'type) 'secondary-cursor))

(defun hel-secondary-region-p (overlay)
  "Return non-nil when OVERLAY is the region of a secondary cursor."
  (eq (overlay-get overlay 'type) 'secondary-region))

;;; Access secondary cursors

(defun hel-all-secondary-cursors (&optional sort)
  "Return all secondary cursors of the current buffer.
With SORT, return them in buffer order."
  ;; The table is nil in buffers without `hel-local-mode', for example the
  ;; minibuffer, but global advices such as `current-kill@hel' run there too.
  (when hel--cursors-table
    (if sort
        (sort (hash-table-values hel--cursors-table)
              (lambda (c1 c2)
                (< (hel--secondary-cursor-point c1)
                   (hel--secondary-cursor-point c2))))
      (hash-table-values hel--cursors-table))))

(defun hel-cursor-with-id (id)
  "Return the cursor with the given ID if it is stil alive."
  (if-let* ((cursor (gethash id hel--cursors-table))
            ((hel-overlay-live-p cursor)))
      cursor))

(defun hel-secondary-cursor-at (position)
  "Return the secondary cursor whose point is at POSITION, or nil."
  ;; The overlay of a cursor is its box over the head character.  The box
  ;; of a forward selection ends at point, on the character before it.
  (-some->> (overlays-in (max (point-min) (1- position)) (1+ position))
    (-filter #'hel-secondary-cursor-p)
    (--find (= position (hel--secondary-cursor-point it)))))

(defun hel-next-secondary-cursor (position)
  "Return the nearest secondary cursor after POSITION, or nil."
  (cl-loop for pos = (next-overlay-change position)
           then (next-overlay-change pos)
           thereis (hel-secondary-cursor-at pos)
           until (= pos (point-max))))

(defun hel-previous-secondary-cursor (position)
  "Return the nearest secondary cursor before POSITION, or nil."
  (cl-loop for pos = (previous-overlay-change position)
           then (previous-overlay-change pos)
           thereis (hel-secondary-cursor-at pos)
           until (= pos (point-min))))

(defun hel-first-secondary-cursor ()
  "Return the first secondary cursor in the buffer."
  (-min-by (lambda (a b)
             (> (hel--secondary-cursor-point a)
                (hel--secondary-cursor-point b)))
           (hel-all-secondary-cursors)))

(defun hel-last-secondary-cursor ()
  "Return the last secondary cursor in the buffer."
  (-max-by (lambda (a b)
             (> (hel--secondary-cursor-point a)
                (hel--secondary-cursor-point b)))
           (hel-all-secondary-cursors)))

(defun hel-number-of-cursors ()
  "Return the number of cursors in the buffer, the main cursor included."
  (1+ (hash-table-count hel--cursors-table)))

(defun hel-any-secondary-cursors-p ()
  "Return non-nil if there are secondary cursors in the buffer."
  (not (hash-table-empty-p hel--cursors-table)))

;;; The cursors as values

;; A cursor value is a list (ID RANGE DIRECTION ACTIVE).  ID is 0 for the
;; main cursor, the key of the secondary cursor in `hel--cursors-table'
;; otherwise, and nil for a cursor that does not exist yet.  RANGE and
;; DIRECTION are the selection of the cursor, as `hel-selection' returns
;; it.  ACTIVE is t when that selection is the active region of the
;; cursor, and nil when it is the cursor character.
;;
;; Code that works on all cursors at once reads them with
;; `hel-selections', computes a new list of values, and makes the
;; cursors match it with `hel-set-selections'.

(defun hel-selections ()
  "Return the selections of all cursors as values, in buffer order.
A cursor value is (ID RANGE DIRECTION ACTIVE).  ID names the cursor: 0 is
the main cursor, and each secondary cursor has a number of its own.
RANGE and DIRECTION are the selection of the cursor, as `hel-selection'
returns it.  ACTIVE is t when the selection is an active region, and nil
when it is the cursor character.  `hel-set-selections' accepts this
list."
  (-sort (-on #'< (-lambda ((_id (begin . _end))) begin))
         (cons (hel--main-cursor-value)
               (-map #'hel--secondary-cursor-value
                     (hel-all-secondary-cursors)))))

(defun hel--main-cursor-value ()
  "Return the value (0 RANGE DIRECTION ACTIVE) of the main cursor."
  (hel--cursor-value 0 (point) (mark t) (region-active-p)))

(defun hel--secondary-cursor-value (cursor)
  "Return the value (ID RANGE DIRECTION ACTIVE) of the secondary CURSOR."
  (hel--cursor-value (overlay-get cursor 'id)
                     (hel--secondary-cursor-point cursor)
                     (hel--secondary-cursor-mark cursor)
                     (overlay-get cursor 'mark-active)))

(defun hel--cursor-value (id point mark active)
  "Return the value of the cursor with ID, POINT, MARK and ACTIVE.
ACTIVE is non-nil when the region of the cursor is active.  An empty
region counts as no region: the selection is then the cursor character,
and a value with ACTIVE t would turn it into an active selection of one
character."
  (let ((active (and active (/= point mark) t)))
    `(,id ,@(hel--selection-from point mark active) ,active)))

(defun hel-set-selections (cursors)
  "Make the cursors of the buffer match CURSORS, a list of cursor values.
Each cursor whose ID is in CURSORS gets its new selection.  A value whose
cursor does not exist becomes a new secondary cursor.  The other secondary
cursors are deleted.  A cursor keeps its kill ring and the other variables
of `hel-cursor-variables'.  A cursor whose value is the same is not
touched, so its mark keeps its place even where the selection does not
need it.  CURSORS has the form that `hel-selections' returns."
  (let ((ids (make-hash-table :size (length cursors)))
        main)
    (dolist (value cursors)
      (puthash (car value) t ids))
    (dolist (cursor (hel-all-secondary-cursors))
      (unless (gethash (overlay-get cursor 'id) ids)
        (hel--delete-secondary-cursor cursor)))
    (dolist (value cursors)
      (-let* (((id . selection) value)
              (cursor (and id (gethash id hel--cursors-table))))
        (cond ((eql id 0)
               (setq main value))
              ((null cursor)
               ;; A cursor of an undo step or of `g v' that was deleted
               ;; since keeps its ID.
               (-let [(range dir active) selection]
                 (hel-create-secondary-cursor range dir active id)))
              ((not (equal value (hel--secondary-cursor-value cursor)))
               (apply #'hel--set-secondary-selection cursor selection)))))
    ;; The main cursor goes last: the new secondary cursors copy its
    ;; variables, and `hel-select' may change them.
    (when (and main (not (equal main (hel--main-cursor-value))))
      (-let [(_ range dir active) main]
        (if active
            (hel-select range dir)
          (hel-set-cursor (car range))))))
  (hel-auto-multiple-cursors-mode))

(defun hel-select-ranges (ranges direction)
  "Create selection for all the RANGES with a cursor of its own.
The cursor selects the first of RANGES, and secondary cursors select each of the
others. RANGES is a list of (BEGIN . END) pairs, and DIRECTION, 1 forward or -1
backward, is the direction of every selection."
  (dolist (range (cdr ranges))
    (hel-create-secondary-cursor range direction t))
  (hel-select (car ranges) direction))

;;; Executing commands for main and secondary cursors

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

(defmacro hel-with-each-cursor (&rest body)
  "Evaluate BODY for each cursor: first the main one, then the secondary ones.
BODY does not run for the cursors that it creates."
  (declare (indent 0) (debug t))
  ;; First collect secondary cursors because BODY can create new cursors,
  ;; and we want it to be executed only for original ones.
  `(let ((cursors (if hel-multiple-cursors-mode
                      (hel-all-secondary-cursors))))
     ;; Main cursor
     ,@body
     ;; Secondary cursors
     (when cursors
       (hel-save-window-scroll
         (hel-save-main-cursor
           (dolist (cursor cursors)
             (hel-with-secondary-cursor cursor
               ,@body)))))))

(defun hel-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for each cursor, the main one first.
Then merge the cursors whose selections overlap, when the
`merge-selections' property of COMMAND asks for it."
  (hel--call-interactively command)
  (hel--execute-command-for-all-secondary-cursors command)
  (when (hel--merge-cursors-p command)
    (hel-merge-overlapping-cursors))
  (setq hel--input-cache nil))

(defun hel--execute-command-for-all-secondary-cursors (command)
  "Call COMMAND interactively for each secondary cursor, in buffer order.
The `multiple-cursors' property of COMMAND decides whether it runs for
them.  For a command without the property, ask the user and remember
the answer."
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
                (-second-item val)
              (hel--prompt-for-unknown-command command)))
           (hel-save-window-scroll
             (hel-save-main-cursor
               ;; `syntax-ppss' reuses an earlier parse only for a position
               ;; after it. In buffer order every cursor finds such a parse;
               ;; in reverse order none does, and each cursor parses the
               ;; buffer from its start.
               (dolist (cursor (hel-all-secondary-cursors :sort))
                 (hel-with-secondary-cursor cursor
                   (hel--call-interactively command)))))))))

(defun hel--call-interactively (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for secondary cursor."
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

(defmacro hel-with-main-cursor-as-secondary (&rest body)
  "Evaluate BODY with the main cursor turned into a secondary cursor.
Then BODY can treat all cursors in the same way.  Afterwards that cursor
becomes the main cursor again, or the first secondary cursor does when
BODY deleted it."
  (declare (indent 0) (debug t))
  (cl-with-gensyms (main-cursor)
    `(let ((,main-cursor (hel-create-secondary-cursor nil nil nil 0)))
       (unwind-protect (progn ,@body)
         (-some-> (if (eq ,main-cursor (gethash 0 hel--cursors-table))
                      ,main-cursor
                    (hel-first-secondary-cursor))
           (hel-make-cursor-main))
         (hel-auto-multiple-cursors-mode)
         (hel-draw-cursor)))))

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
  (let* ((cursors (hel-selections))
         (merged (hel--merge-cursors cursors)))
    ;; The merge only joins cursors, so a list of the same length is
    ;; the same list.
    (unless (length= cursors (length merged))
      (hel-set-selections merged))))

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
                            (hel--ranges-overlap-p (-second-item (car merged))
                                                   (-second-item cursor)))
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
and direction.  A bare survivor has no direction, so a merged selection
longer than one character is forward then."
  (-let* (((_ (begin-a . end-a) _ active-a) a)
          ((id-b (begin-b . end-b) _ active-b) b)
          ((id . survivor) (if (eql id-b 0) b a))
          (range (cons (min begin-a begin-b) (max end-a end-b))))
    (list id
          range
          (cond ((hel-bare-cursor-p (list range)) nil)
                ((hel-bare-cursor-p survivor) 1)
                (t (cadr survivor)))
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
                         (hel-selections))))
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
