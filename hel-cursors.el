;;; hel-cursors.el --- Cursors -*- lexical-binding: t -*-
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
;; How multiple cursors work. A buffer has one main cursor, whose state is
;; point, the mark and the variables of `hel-cursor-variables' in Emacs, and
;; any number of secondary cursors, each an overlay that holds the same state.
;; Emacs runs a command for the main cursor. Then `post-command-hook' runs it
;; for each secondary cursor: it loads the cursor into Emacs with
;; `hel--load-cursor', runs the command, and saves the cursor back with
;; `hel--save-cursor'.
;;
;; ID 0 always corresponds to the main cursor.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'subr-x)
(require 'hel-lib)

(defvar hel-insert-state)
(defvar hel-normal-state)
(defvar hel-local-mode)

(declare-function hel-state-property "hel-core")

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

(defun hel-create-secondary-cursor (&optional range dir line? id)
  "Create a secondary cursor and return it.
Without RANGE the new cursor is a copy of the main cursor with its selection.
With RANGE, the cursor selects RANGE as `hel-select' does with RANGE, DIR and
LINE?. The cursor takes its kill ring and the other variables of
`hel-cursor-variables' from the main cursor.

ID names the cursor, as in a cursor value of `hel-selections'. Without ID the
cursor gets a new one. Turn on `hel-multiple-cursors-mode' when it is off."
  (let ((cursor (hel--save-cursor (make-overlay (point) (point) nil t nil)))
        (id (or id (hel--new-secondary-cursor-id))))
    (overlay-put cursor 'id id)
    (overlay-put cursor 'type 'secondary-cursor)
    ;; More then the `show-paren-mode' highlights.
    (overlay-put cursor 'priority 1001)
    (puthash id cursor hel--cursors-table)
    (if range
        (hel--set-secondary-selection cursor range dir line?)
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
In Normal state the cursor is a box over the head character of its selection.
In Insert state it is a bar at its point, the string `hel-bar-secondary-cursor',
and in a text terminal a box over the character after point. A box on a newline
or at the end of the buffer is a cell of one space."
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
  "Draw the active region of the secondary CURSOR, or delete its drawing.
The line selection of an empty line gets the face of the region to the
edge of the window, so it looks like a selected line, and a bare cursor
on the same line does not."
  (let ((beg (hel--secondary-cursor-point cursor))
        (end (hel--secondary-cursor-mark cursor)))
    (if (and (overlay-get cursor 'mark-active)
             (/= beg end))
        (let ((region (or (overlay-get cursor 'secondary-region)
                          (-doto (make-overlay beg end nil nil t)
                            (overlay-put 'type 'secondary-region)
                            ;; The same as Emacs native region overlay.
                            (overlay-put 'priority 100)))))
          (move-overlay region beg end)
          (overlay-put region 'face
                       (if (hel--empty-line-range-p (hel-range beg end))
                           '(:inherit region :extend t)
                         'region))
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
The state is point, the mark and the variables of `hel-cursor-variables'.
Return OVERLAY."
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

(cl-defun hel--set-secondary-selection (cursor range dir &optional line?)
  "Give the secondary CURSOR the selection of RANGE and draw it.
RANGE, DIR and LINE? are as in `hel-select'."
  (-let* (((begin . end) range)
          (bare-cursor? (<= (hel-range-length range) 1))
          (active? (or line? (not bare-cursor?)))
          ((point . mark) (if (and (not bare-cursor?) dir (< 0 dir))
                              (cons end begin)
                            (cons begin end))))
    (when (and (not bare-cursor?) (null dir))
      (error "No direction for a selection of %d characters" (- end begin)))
    (move-overlay (overlay-get cursor 'point) point point)
    (when active?
      (move-overlay (overlay-get cursor 'mark) mark mark))
    (overlay-put cursor 'mark-active active?)
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
those of CURSOR, so a command acts at CURSOR. BODY replaces the state of
the main cursor, so call this macro inside `hel-save-main-cursor'."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((hel-executing-command-for-secondary-cursor t))
     (hel--load-cursor ,cursor)
     (hel--hide-secondary-cursor ,cursor)
     (unwind-protect
         (progn ,@body)
       (hel--save-cursor ,cursor)
       (hel--draw-secondary-cursor ,cursor))))

(defmacro hel-with-main-cursor-as-secondary (&rest body)
  "Evaluate BODY with the main cursor turned into a secondary cursor.
Then BODY can treat all cursors in the same way. Afterwards that cursor becomes
the main cursor again, or the first secondary cursor does when BODY deleted it."
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

;; A cursor value is a list (ID . SELECTION). ID is 0 for the main cursor, the
;; key of the secondary cursor in `hel--cursors-table' otherwise, and nil for
;; a cursor that does not exist yet. SELECTION is the selection of the cursor,
;; as `hel-selection' returns it, so two cursors that look the same have
;; equal values.
;;
;; Code that works on all cursors at once reads them with `hel-selections',
;; computes a new list of values, and makes the cursors match it with
;; `hel-set-selections'.

(defun hel-selections ()
  "Return the selections of all cursors as values, in buffer order.
A cursor value is (ID . SELECTION). ID names the cursor: 0 is the main cursor,
and each secondary cursor has a number of its own. SELECTION is the selection
of the cursor, as `hel-selection' returns it.
`hel-set-selections' accepts this list."
  (-sort (-on #'< (-lambda ((_id (begin . _end))) begin))
         (cons (hel--main-cursor-value)
               (-map #'hel--secondary-cursor-value
                     (hel-all-secondary-cursors)))))

(defun hel--main-cursor-value ()
  "Return the value (0 . SELECTION) of the main cursor."
  (cons 0 (hel-selection)))

(defun hel--secondary-cursor-value (cursor)
  "Return the value (ID . SELECTION) of the secondary CURSOR."
  (cons (overlay-get cursor 'id)
        (hel--selection-from (hel--secondary-cursor-point cursor)
                             (hel--secondary-cursor-mark cursor)
                             (overlay-get cursor 'mark-active))))

(defun hel-set-selections (cursors)
  "Make the cursors of the buffer match CURSORS, a list of cursor values.
Each cursor whose ID is in CURSORS gets its new selection. A value whose
cursor does not exist becomes a new secondary cursor. The other secondary
cursors are deleted. A cursor keeps its kill ring and the other variables
of `hel-cursor-variables'. A cursor whose value is the same is not touched,
so its mark keeps its place even where the selection does not need it.
CURSORS has the form that `hel-selections' returns."
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
               (-let [(range dir line?) selection]
                 (hel-create-secondary-cursor range dir line? id)))
              ((not (equal value (hel--secondary-cursor-value cursor)))
               (apply #'hel--set-secondary-selection cursor selection)))))
    ;; The main cursor goes last: the new secondary cursors copy its
    ;; variables, and `hel-select' may change them.
    (when (and main (not (equal main (hel--main-cursor-value))))
      (apply #'hel-select (cdr main))))
  (hel-auto-multiple-cursors-mode))

(defun hel-select-ranges (ranges direction)
  "Create selection for all the RANGES with a cursor of its own.
The cursor selects the first of RANGES, and secondary cursors select each of the
others. RANGES is a list of (BEGIN . END) pairs, and DIRECTION, 1 forward or -1
backward, is the direction of every selection."
  (dolist (range (cdr ranges))
    (hel-create-secondary-cursor range direction))
  (hel-select (car ranges) direction))

;;; Evaluating code for each cursor

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

;;; Merge overlapping regions

(defun hel-merge-overlapping-cursors ()
  "Merge the cursors whose selections overlap into one cursor each.
When a group of overlapping cursors holds the main cursor, the main cursor
survives with its own state. Otherwise the first cursor of the group survives.
The other cursors of the group are deleted."
  (let* ((cursors (hel-selections))
         (merged (hel--merge-cursors cursors)))
    ;; The merge only joins cursors, so a list of the same length is
    ;; the same list.
    (unless (length= cursors (length merged))
      (hel-set-selections merged))))

(defun hel--merge-cursors (cursors)
  "Merge the cursors whose selections overlap.
CURSORS is a list of cursor values (ID . SELECTION), sorted by the beginning
of their ranges. Return the merged list in the same order. Each range of the
result covers a group of overlapping ranges. The main cursor survives in its
group, and otherwise the first cursor of the group does; the survivor keeps
its ID and direction. A merged selection of one empty line is a line selection
when the selection of any cursor of the group is."
  (nreverse
   (-reduce-from (lambda (merged cursor)
                   (if (and merged
                            (hel--ranges-overlap-p
                             (hel-selection-bounds (cdr (car merged)))
                             (hel-selection-bounds (cdr cursor))))
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
The main cursor survives; otherwise A does. The survivor keeps its ID
and direction. A bare survivor has no direction, so a merged selection
longer than one character is forward then. A merged selection of at most
one character is the line selection of an empty line when A or B is not
a bare cursor: both cursors then stand on the same empty line, and one of
them selects it."
  (-let* (((_ . selection-a) a)
          ((id-b . selection-b) b)
          ((id . survivor) (if (eql id-b 0) b a))
          ((begin-a . end-a) (hel-selection-bounds selection-a))
          ((begin-b . end-b) (hel-selection-bounds selection-b))
          (range (cons (min begin-a begin-b) (max end-a end-b))))
    (cond ((< 1 (hel-range-length range))
           (list id range (or (hel-selection-direction survivor) 1)))
          ((and (hel-bare-cursor-p selection-a)
                (hel-bare-cursor-p selection-b))
           (list id range nil))
          (t
           (list id range nil t)))))

;;; Drawing the cursors

(defvar-local hel--cursor-hidden? nil
  "Whether the major mode of the current buffer hides the cursor.")

(defun hel--check-if-cursor-is-hidden ()
  "Remember whether the major mode of the current buffer hides the cursor."
  (setq hel--cursor-hidden? (and (local-variable-p 'cursor-type)
                                 (null cursor-type))))

(defun hel--cursor-look (main?)
  "Return how a cursor looks in the current state: (SHAPE FACE BOX).
SHAPE is the shape of the cursor: `bar', `box', `hbar' or nil for none.
FACE colours it.  BOX is the face of the box that Hel draws over the
cursor character in Normal state, and nil in other states.  MAIN? is
non-nil for the main cursor and nil for a secondary one.

In Normal state every cursor is a box that Hel draws.  With
`hel-cursor-type' set to `hybrid', the main cursor is a bar at point and
a faint box.  In Insert state every cursor is a bar at point.  In Emacs
state the main cursor is an underscore.  While motions extend the
selection, every cursor has the face `hel-extend-selection-cursor'.

In a text terminal every bar is a box over the character after point.
The terminal shows its own cursor as a box, and a bar drawn as a string
would move the text to the right."
  (let ((face (if hel-extend-selection
                  'hel-extend-selection-cursor
                (nth (if main? 0 1) (hel-state-property hel-state :cursor)))))
    (pcase hel-state
      ('normal (list (cond ((not main?) 'box)
                           ((not (display-graphic-p)) 'box)
                           ((eq hel-cursor-type 'hybrid) 'bar))
                     face
                     (if (and main? (eq hel-cursor-type 'hybrid))
                         'hel-cursor-highlight
                       face)))
      ('insert (list (if (display-graphic-p) 'bar 'box) face nil))
      ('emacs (list (if main? 'hbar) face nil)))))

(defun hel-draw-cursor ()
  "Give the cursor of Emacs the shape and the colour of the current state.
The colour also shows whether motions extend the selection."
  (when (and (eq (window-buffer) (current-buffer))
             hel-local-mode
             (not hel--cursor-hidden?))
    (-let [(shape face) (hel--cursor-look t)]
      (setq cursor-type (if (eq shape 'hbar) '(hbar . 4) shape))
      (when face
        (hel--set-cursor-color (face-background face nil t))))))

;;;; The overlay of the main cursor

(defvar hel--main-line-overlay nil
  "The overlay that paints the line selection of an empty line.
Emacs draws the region of the main cursor, and its face stops at the
newline, so this overlay paints the selected empty line to the edge of
the window, in the selected window only. `hel--draw-main-cursor-overlay'
moves it, and it deletes it when the main cursor has no such selection.")

(defun hel--cursor-bounds (selection)
  "Return the bounds of the cursor of SELECTION, a (BEGIN . END) pair.
The cursor covers the head character of SELECTION: the cursor character,
or the character at the head end of an active region.  When the head
character is a newline, or at the end of the buffer, BEGIN equals END:
the cursor is drawn there as a cell of one space, because a face on
a newline would paint to the edge of the window.  SELECTION is (RANGE
DIRECTION), as `hel-selection' returns it."
  (let ((head (hel-selection-head selection)))
    (cons head (if (memq (char-after head) '(?\n nil))
                   head
                 (1+ head)))))

(defun hel--cursor-display (begin face)
  "Return a string that draws the box of a cursor on trailing whitespace.
With `show-trailing-whitespace' on, Emacs draws trailing whitespace with
the face `trailing-whitespace' only, so the box disappears.  Emacs does
not use that face for a display string.  So the box over a trailing space
or tab is a display string of that character with FACE.  A tab keeps its
width.  BEGIN is the position of the cursor character.  Return nil when
the character is not trailing whitespace or the option is off."
  (when (and show-trailing-whitespace
             (save-excursion
               (goto-char begin)
               (looking-at-p "[ \t]+$")))
    (propertize (string (char-after begin)) 'face face)))

(defun hel--draw-main-cursor-overlay (windows)
  "Draw the box of the main cursor in each of WINDOWS.
Redisplay calls this function before it redraws WINDOWS, or t for all
windows, so the box follows point after every command, including the
commands outside Hel.  The selected window draws the box over the head
character of the selection.  Another window on the buffer draws it at its
own point, with the face `hel-other-window-cursor'.  The selected window
of a frame without focus uses that face too.  On a newline or at
the end of the buffer the box is a cell of one space.  Outside Normal
state there is no box.

In the selected window, the line selection of an empty line gets the
face of the region to the edge of the window, see
`hel--main-line-overlay'."
  (dolist (window (if (consp windows) windows (window-list-1 nil t t)))
    (when (window-live-p window)
      (let ((overlay (window-parameter window 'hel--main-cursor-overlay))
            (selected? (eq window (selected-window))))
        (with-current-buffer (window-buffer window)
          (if (or hel--cursor-hidden? (not hel-normal-state))
              (progn
                (-some-> overlay (delete-overlay))
                (when selected?
                  (-some-> hel--main-line-overlay (delete-overlay))))
            ;; else
            (-let* ((selection (if selected?
                                   (hel-selection)
                                 (hel--selection-from (window-point window) nil nil)))
                    ((begin . end) (hel--cursor-bounds selection)))
              (unless overlay
                (setq overlay (-doto (make-overlay begin end)
                                (overlay-put 'type 'main-cursor)
                                (overlay-put 'window window)))
                (set-window-parameter window 'hel--main-cursor-overlay overlay))
              ;; In the selected window the box lies above the region of Emacs,
              ;; which has (nil . 100), and the bracket that `show-paren-mode'
              ;; highlights, which has `show-paren-priority', 1000 by default.
              ;; In another window it lies under every highlight with
              ;; a priority, such as the matches that Consult previews there
              ;; from the minibuffer.
              (overlay-put overlay 'priority (if selected? 1001 0))
              (unless (and (eq (overlay-buffer overlay) (current-buffer))
                           (eql (overlay-start overlay) begin)
                           (eql (overlay-end overlay) end))
                (move-overlay overlay begin end (current-buffer)))
              ;; A frame without focus draws its selected window with
              ;; the dimmer box, as Emacs draws a hollow cursor there.
              (let ((face (if (and selected?
                                   (frame-focus-state (window-frame window)))
                              (nth 2 (hel--cursor-look t))
                            'hel-other-window-cursor)))
                (if (= begin end)
                    (-doto overlay
                      (overlay-put 'face nil)
                      (overlay-put 'display nil)
                      (overlay-put 'after-string
                                   (propertize " " 'face face 'cursor t)))
                  (-doto overlay
                    (overlay-put 'face face)
                    (overlay-put 'display (hel--cursor-display begin face))
                    (overlay-put 'after-string nil))))
              ;; The line selection of an empty line looks like a selected
              ;; line, and a bare cursor on the same line does not.
              (when selected?
                (-let* (((range &as line-begin . line-end) (hel-selection-bounds selection))
                        (empty-line? (and (hel--empty-line-range-p range)
                                          (not (hel-bare-cursor-p selection)))))
                  (cond ((not empty-line?)
                         (-some-> hel--main-line-overlay (delete-overlay)))
                        (hel--main-line-overlay
                         (move-overlay hel--main-line-overlay
                                       line-begin line-end (current-buffer)))
                        (t
                         (setq hel--main-line-overlay
                               (-doto (make-overlay line-begin line-end)
                                 (overlay-put 'face '(:inherit region
                                                      :extend t))
                                 ;; The same as the region of Emacs.
                                 (overlay-put 'priority 100)))))
                  (when empty-line?
                    (overlay-put hel--main-line-overlay 'window window)))))))))))

(defun hel--delete-dead-main-cursor-overlays ()
  "Delete the boxes that deleted windows leave in their buffers.
Emacs runs this function when a window of the frame is added or deleted."
  (dolist (buffer (->> (window-list)
                       (-map #'window-buffer)
                       (-uniq)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (->> (overlays-in (point-min) (point-max))
             (-filter (lambda (ov)
                        (and (eq (overlay-get ov 'type) 'main-cursor)
                             (not (window-live-p (overlay-get ov 'window))))))
             (mapc #'delete-overlay))))))

(defun hel--delete-main-cursor-overlays ()
  "Delete the box of the main cursor in every window.
Delete also the paint of the line selection of an empty line."
  (-some-> hel--main-line-overlay (delete-overlay))
  (walk-windows (lambda (window)
                  (-some-> (window-parameter window 'hel--main-cursor-overlay)
                    (delete-overlay)))
                t t))

;;;; Update cursor color on theme change

(defun hel--set-cursor-color (color)
  ;; Cursor color can only be set for each frame but not for each buffer, also
  ;; `modify-frame-parameters' forces a redisplay, so only call it when the
  ;; color actually changes.
  (unless (equal color (frame-parameter nil 'cursor-color))
    (modify-frame-parameters (selected-frame) `((cursor-color . ,color)))))

(defun hel--update-main-cursor-color (color)
  (set-face-attribute 'hel-normal-state-main-cursor nil
                      :background color
                      :foreground (face-background 'default))
  (hel-draw-cursor))

(hel-advice-add 'set-cursor-color :after #'hel--update-main-cursor-color)

(defun hel--on-theme-change (_theme)
  (hel--update-main-cursor-color (face-background 'cursor)))

;;;; Pulse selections

(defun hel-pulse-main-selection (&optional face)
  "Flash the selection of the main cursor with FACE."
  (-let [(begin . end) (hel-selection-bounds)]
    (pulse-momentary-highlight-region begin end face)))

;;; .
(provide 'hel-cursors)
;;; hel-cursors.el ends here
