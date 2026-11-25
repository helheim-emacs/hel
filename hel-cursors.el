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
;; How multiple cursors work. A buffer has one main cursor, whose state
;; is point, the mark and the variables of `hel-cursor-variables' in Emacs,
;; and any number of secondary cursors, each an overlay that holds the same
;; state. Emacs runs a command for the main cursor. Then `post-command-hook'
;; runs it for each secondary cursor: it loads the cursor into Emacs with
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
the first secondary selection with `hel-create-secondary-selection', and
disabled when you delete the last one with `hel-delete-secondary-cursor'."
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

(defun hel-create-secondary-selection (&optional range dir line? id)
  "Create a secondary selection and return the cursor that holds it.
Without RANGE the new selection is a copy of the main selection. With RANGE it
selects RANGE as `hel-select' does with RANGE, DIR and LINE?. The cursor takes
its kill ring and the other variables of `hel-cursor-variables' from the main
cursor, so the new selection starts with the state of the main one.

ID names the cursor, as in a value of `hel-selections'. Without ID the cursor
gets a new one. Turn on `hel-multiple-cursors-mode' when it is off."
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
          ((point . mark) (if (and (not bare-cursor?)
                                   dir (< 0 dir))
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
    `(let ((,main-cursor (hel-create-secondary-selection nil nil nil 0)))
       (unwind-protect (progn ,@body)
         (-some-> (if (eq ,main-cursor (gethash 0 hel--cursors-table))
                      ,main-cursor
                    (hel-first-secondary-cursor))
           (hel-make-cursor-main))
         (hel-auto-multiple-cursors-mode)
         (hel-set-emacs-cursor)))))

(defun hel--hide-secondary-cursor (cursor)
  "Delete the drawing of the secondary CURSOR and keep the cursor."
  (hel--delete-secondary-region cursor)
  (delete-overlay cursor)
  cursor)

(defun hel-secondary-cursor-p (overlay)
  "Return non-nil when OVERLAY is a secondary cursor."
  (eq (overlay-get overlay 'type) 'secondary-cursor))

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
                 (hel-create-secondary-selection range dir line? id)))
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
    (hel-create-secondary-selection range direction))
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

(defun hel-merge-overlapping-selections ()
  "Merge each group of overlapping selections into one selection.
When a group holds the main selection, the main cursor survives with its own
state. Otherwise the cursor of the first selection of the group survives. The
cursors of the other selections of the group are deleted."
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
  (-let* (((_id . selection-a) a)
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
;;
;; A cursor has two layers. The cursor of Emacs shows point of the main
;; cursor in the selected window. Only the main cursor has it. Hel draws
;; everything else with overlays: the box over a character in Normal state,
;; and the bar of a secondary cursor in Insert state. `hel--cursor-look'
;; decides both layers of a cursor, and `hel--paint-cursor' puts the shape
;; that Hel draws on an overlay. The look never depends on the text at the
;; cursor, so it is the same for a cursor anywhere in the buffer.
;; `hel--paint-cursor' fits the shape to the text, where a face over the
;; character cannot show: on a newline, on trailing whitespace, and on
;; a character in the fringe.

(defvar-local hel--cursor-hidden? nil
  "Whether the major mode of the current buffer hides the cursor.")

(defun hel--check-if-cursor-is-hidden ()
  "Remember whether the major mode of the current buffer hides the cursor.
A major mode hides the cursor when it sets `cursor-type' to nil in its buffer."
  (setq hel--cursor-hidden? (and (local-variable-p 'cursor-type)
                                 (null cursor-type))))

(defun hel--cursor-look (role &optional bare?)
  "Return how a cursor of ROLE looks in the current buffer.
The look says which shape Emacs gives its own cursor, which shape Hel draws
over the text, and their faces. It depends on the state, on `hel-cursor-type',
and on whether motions extend the selection.

In Normal state every cursor is a box over a character. With `hel-cursor-type'
set to `hybrid', the main cursor is the bar of Emacs, and it has a box with the
face `hel-cursor-highlight' only when BARE? is non-nil: a longer selection shows
as the region, but a bare cursor has no region, so without the box its character
would not look selected.

In Insert state the main cursor is the bar of Emacs, and a secondary cursor
is a bar that Hel draws, because Emacs draws only one cursor. In Emacs state
the main cursor is an underscore, and a secondary cursor has no shape. While
motions extend the selection, the main and the secondary cursors have the face
`hel-extend-selection-cursor'. When the major mode hides the cursor, no cursor
has a shape, because the mode shows the position in its own way.

In a text terminal every bar is a box: the terminal draws its own cursor as
a box, and a bar drawn as a string would push the text to the right.

ROLE is:
  `main'       the main cursor in the selected window
  `other'      the main cursor anywhere else
  `secondary'  a secondary cursor

BARE? is non-nil when the selection of the cursor is a bare cursor.

Returns plist (:cursor-type TYPE :color COLOR :shape SHAPE :face FACE),
and a missing key is nil. TYPE is the value for `cursor-type', and COLOR is
the face whose background colours the cursor of Emacs; only the role `main'
has them. SHAPE is `box', `bar' or nil, the shape that Hel draws, and FACE
is its face."
  (unless hel--cursor-hidden?
    (let ((face (cond ((eq role 'other)
                       'hel-other-window-cursor)
                      (hel-extend-selection 'hel-extend-selection-cursor)
                      ((eq role 'main)
                       (hel-state-property hel-state :main-cursor))
                      (t
                       (hel-state-property hel-state :secondary-cursor))))
          (graphic? (display-graphic-p))
          (hybrid? (eq hel-cursor-type 'hybrid)))
      (pcase (list hel-state role)
        ('(normal main)
         (list :cursor-type (cond ((not graphic?) 'box)
                                  (hybrid? 'bar))
               :color face
               :shape (if (or (not hybrid?) bare?) 'box)
               :face (if hybrid? 'hel-cursor-highlight face)))
        (`(normal ,_)
         (list :shape 'box :face face))
        ('(insert main)
         (list :cursor-type (if graphic? 'bar 'box)
               :color face))
        ('(insert secondary)
         (list :shape (if graphic? 'bar 'box)
               :face face))
        ('(emacs main)
         (list :cursor-type '(hbar . 4)
               :color face))))))

(defun hel-set-emacs-cursor ()
  "Give the cursor of Emacs the shape and the colour of the main cursor.
`hel--cursor-look' decides them from the state, so the colour also shows
whether motions extend the selection.  The colour belongs to the frame,
so only the buffer of the selected window sets it, and only when Hel is
on in that buffer."
  (when (and (eq (window-buffer) (current-buffer))
             hel-local-mode)
    (-let [(&plist :cursor-type type :color face) (hel--cursor-look 'main)]
      (setq cursor-type type)
      (when face
        (let ((color (face-background face nil t)))
          ;; `modify-frame-parameters' forces a redisplay, so write the
          ;; colour only when it changes.
          (unless (equal color (frame-parameter nil 'cursor-color))
            (modify-frame-parameters nil `((cursor-color . ,color)))))))))

(defvar-local hel--default-font-height nil
  "The height of the default font for the bars of the current command, or nil.
`window-default-font-height' asks the display, and that is slow when every
secondary cursor draws a bar. So the first bar of a command computes the
height, and the others reuse it. `hel--pre-command-hook' sets this variable
to nil, because the font can change between commands.")

(defun hel--paint-cursor (overlay look selection)
  "Paint the OVERLAY of the cursor.
A box covers the head character of SELECTION. When that character is
hidden, the first character after it that is not hidden gets a hollow box
instead, so the cursor stays visible. Point stays on the hidden character,
and the hollow box says that the character under it is not selected. See
`hel--char-hidden-p'.

LOOK is a value of `hel--cursor-look'.
SELECTION has the form ((BEGIN . END) DIRECTION), as `hel-selection' returns."
  (-let* (((&plist :shape :face) look)
          (head (hel-selection-head selection))
          (hidden? (and (eq shape 'box) (hel--char-hidden-p head)))
          ((begin . end)
           (if (eq shape 'box)
               ;; A newline and the end of the buffer give an empty range,
               ;; and `after-string' then draws the box as a space.
               (let ((char (if hidden? (hel--next-shown-char head) head)))
                 (if (eq (char-after char) ?\n)
                     (cons char char)
                   (hel-char-range char)))
             (let ((point (-let [((begin . end) dir) selection]
                            (if (eql dir 1) end begin))))
               (cons point point))))
          (box-on-char? (and (eq shape 'box) (/= begin end)))
          (box-as-space? (and (eq shape 'box) (= begin end)))
          (box-face (if hidden?
                        `(:box (:line-width (-1 . -1)
                                :color ,(face-background face nil t)))
                      face)))
    (hel--move-overlay overlay begin end)
    (hel--overlay-put overlay 'face (if box-on-char? box-face))
    (hel--overlay-put overlay 'display
                      ;; With `show-trailing-whitespace' on, Emacs paints
                      ;; trailing whitespace only with the face `trailing-whitespace',
                      ;; and the box disappears. Emacs does not use that face
                      ;; for a display string, so a box over a trailing space or
                      ;; tab shows the same character as a display string with
                      ;; FACE. A tab keeps its width.
                      (when (and box-on-char?
                                 show-trailing-whitespace
                                 (save-excursion
                                   (goto-char begin)
                                   (looking-at-p "[ \t]+$")))
                        (propertize (string (char-after begin)) 'face box-face)))
    (hel--overlay-put overlay 'before-string
                      (when (eq shape 'bar)
                        (unless hel--default-font-height
                          (setq hel--default-font-height (window-default-font-height)))
                        (propertize hel-bar-secondary-cursor
                                    'face `(,face (:height ,hel--default-font-height)))))
    (hel--overlay-put overlay 'after-string
                      (if box-as-space? (propertize " " 'face box-face 'cursor t)))
    overlay))

(defun hel--char-hidden-p (pos)
  "Return non-nil if the character at POS is hidden.
A hidden character has no cell in the text where a face could show. Text
is hidden when it is invisible, such as a folded section in Org mode, or
when a `display' spec (left-fringe BITMAP FACE) or (right-fringe BITMAP
FACE) draws it in the fringe, such as the prefix of a line in `diff-mode'."
  (or (invisible-p pos)
      (memq (car-safe (get-char-property pos 'display))
            '(left-fringe right-fringe))))

(defun hel--next-shown-char (pos)
  "Return the position of the first character after POS that is not hidden.
The end of the buffer counts as such a position, because the box there is
a cell of one space. See `hel--char-hidden-p'."
  (let ((pos (next-char-property-change pos)))
    (while (and (< pos (point-max))
                (hel--char-hidden-p pos))
      (setq pos (next-char-property-change pos)))
    pos))

(defun hel--paint-region (overlay range)
  "Make OVERLAY paint RANGE as the region, and return OVERLAY.
The line selection of an empty line covers only the newline, so there the
face extends to the edge of the window, and the user sees a selected line.
RANGE is a (BEGIN . END) pair."
  (hel--move-overlay overlay (car range) (cdr range))
  ;; The same priority as the region of Emacs.
  (overlay-put overlay 'priority 100)
  (overlay-put overlay 'face (if (hel--empty-line-range-p range)
                                 '(:inherit region :extend t)
                               'region))
  overlay)

(defun hel--move-overlay (overlay begin end)
  "Move OVERLAY to BEGIN and END in the current buffer, unless it is there.
`move-overlay' redraws OVERLAY even when its bounds stay, and Hel redraws every
cursor again after each command."
  (unless (and (eq (overlay-buffer overlay) (current-buffer))
               (eql (overlay-start overlay) begin)
               (eql (overlay-end overlay) end))
    (move-overlay overlay begin end (current-buffer))))

(defun hel--overlay-put (overlay property value)
  "Set PROPERTY of OVERLAY to VALUE, unless it already has an equal value.
`overlay-put' redraws OVERLAY whenever VALUE is not `eq' to the old value,
while Hel redraw every cursor after each command."
  (unless (equal-including-properties value (overlay-get overlay property))
    (overlay-put overlay property value)))

;;;; The overlay of the main cursor

(defvar hel--main-cursor-overlays nil
  "The overlays of the main cursor, as an alist of (WINDOW . OVERLAY).
Each window has its own overlay in the buffer that it shows. A deleted
window cannot delete its overlay, so `hel--delete-dead-main-cursor-overlays'
deletes the overlays of the windows that are not live.")

(defvar hel--main-line-overlay nil
  "The overlay that paints the line selection of an empty line.
On an empty line the line selection covers only the newline, and the
region that Emacs draws there looks like a bare cursor.  This overlay
paints the line to the edge of the window, so the user sees a selected
line.  It lives in the selected window, and `hel--draw-main-line-overlay'
moves it or deletes it.")

(defun hel--draw-main-cursor (windows)
  "Draw the main cursor in each of WINDOWS.
Redisplay calls this function before it redraws WINDOWS, so the main cursor
follows point after every command, including the commands that are not part of
Hel. WINDOWS is a list of windows, or t for all windows."
  (->> (if (consp windows)
           windows
         (window-list-1 nil t t))
       (-filter #'window-live-p)
       (mapc #'hel--draw-main-cursor-in-window)))

(defun hel--draw-main-cursor-in-window (window)
  "Draw the main cursor in WINDOW.
In the selected window of a frame with focus, the overlay of WINDOW draws the
shape that `hel--cursor-look' gives the role `main'. In every other window, and
in the selected window of a frame without focus, it draws the shape of the role
`other', with the face `hel-other-window-cursor', like the hollow cursor that
Emacs draws there. A window that is not selected has its own point and no
region, so its box covers the character at its point. When the look has no
shape, or Hel is off in the buffer, WINDOW has no overlay of the main cursor.

The selected window also paints the line selection of an empty line, see
`hel--draw-main-line-overlay'."
  (with-current-buffer (window-buffer window)
    (let* ((overlay (alist-get window hel--main-cursor-overlays))
           (selected? (eq window (selected-window)))
           (selection (cond ((not hel-local-mode) nil)
                            (selected? (hel-selection))
                            (t (hel--selection-from (window-point window) nil nil))))
           (look (and selection
                      (hel--cursor-look
                       (if (and selected? (frame-focus-state (window-frame window)))
                           'main
                         'other)
                       (hel-bare-cursor-p selection)))))
      (if (null (plist-get look :shape))
          (-some-> overlay (delete-overlay))
        (unless overlay
          (setq overlay (-doto (make-overlay 1 1)
                          (overlay-put 'type 'main-cursor)
                          (overlay-put 'window window)))
          (push (cons window overlay) hel--main-cursor-overlays))
        ;; In the selected window the box lies above the region of Emacs, which
        ;; has (nil . 100), and the bracket that `show-paren-mode' highlights,
        ;; which has `show-paren-priority', 1000 by default. In another window
        ;; it lies under every highlight with a priority, such as the matches
        ;; that Consult previews there from the minibuffer.
        (overlay-put overlay 'priority (if selected? 1001 0))
        (hel--paint-cursor overlay look selection))
      (when selected?
        (if selection
            (hel--draw-main-line-overlay window selection)
          (-some-> hel--main-line-overlay (delete-overlay)))))))

(defun hel--draw-main-line-overlay (window selection)
  "Paint the line selection of an empty line in WINDOW, or delete the paint.
In Normal state, when SELECTION is the line selection of an empty line,
`hel--main-line-overlay' paints that line with the face of the region to the
edge of WINDOW, so the user sees a selected line. A bare cursor on an empty line
covers the same newline, but it is a cursor, not a selected line, so it gets no
paint. In every other case the function deletes the paint. The paint shows only
in WINDOW, because the selection belongs to the selected window. SELECTION is
\((BEGIN . END) DIRECTION), as `hel-selection' returns it."
  (let* ((range (hel-selection-bounds selection))
         (empty-line? (and hel-normal-state
                           (hel--empty-line-range-p range)
                           (not (hel-bare-cursor-p selection)))))
    (if (not empty-line?)
        (-some-> hel--main-line-overlay (delete-overlay))
      ;; else
      (setq hel--main-line-overlay
            (hel--paint-region (or hel--main-line-overlay (make-overlay 1 1))
                               range))
      (overlay-put hel--main-line-overlay 'window window))))

(defun hel--delete-dead-main-cursor-overlays ()
  "Delete the overlays of the main cursor whose windows are deleted.
Emacs runs this function when a window is added or deleted."
  (-let [(live dead) (->> hel--main-cursor-overlays
                          (-separate (-lambda ((window))
                                       (window-live-p window))))]
    (->> dead
         (-map #'cdr)
         (mapc #'delete-overlay))
    (setq hel--main-cursor-overlays live)))

(defun hel--delete-all-main-cursor-overlays ()
  "Delete the overlay of the main cursor in every window.
Delete also the paint of the line selection of an empty line."
  (-some-> hel--main-line-overlay (delete-overlay))
  (->> hel--main-cursor-overlays
       (-map #'cdr)
       (mapc #'delete-overlay))
  (setq hel--main-cursor-overlays nil))

;;;; The overlays of the secondary cursors

(defun hel--draw-secondary-cursor (cursor)
  "Draw the secondary CURSOR and its region, and return CURSOR.
CURSOR is itself the overlay that draws the shape, which `hel--cursor-look'
gives the role `secondary'. The region is a separate overlay, see
`hel--draw-secondary-region'."
  (hel--paint-cursor cursor
                     (hel--cursor-look 'secondary)
                     (hel--selection-from (hel--secondary-cursor-point cursor)
                                          (hel--secondary-cursor-mark cursor)
                                          (and hel-normal-state
                                               (overlay-get cursor 'mark-active))))
  (hel--draw-secondary-region cursor)
  cursor)

(defun hel--draw-secondary-region (cursor)
  "Draw the region of the secondary CURSOR, or delete the drawing.
The region is drawn when the mark of CURSOR is active and differs from its
point. It is an overlay of its own, stored in the property `secondary-region'
of CURSOR. The line selection of an empty line covers only the newline, so its
face extends to the edge of the window, and the user sees a selected line."
  (let ((point (hel--secondary-cursor-point cursor))
        (mark  (hel--secondary-cursor-mark cursor)))
    (if (and (overlay-get cursor 'mark-active)
             (/= point mark))
        (overlay-put cursor 'secondary-region
                     (hel--paint-region
                      (or (overlay-get cursor 'secondary-region)
                          (-doto (make-overlay 1 1 nil nil t)
                            ;; No code reads the type.  It finds a region
                            ;; overlay that a bug has left without its cursor.
                            (overlay-put 'type 'secondary-region)))
                      (hel-range point mark)))
      ;; else
      (hel--delete-secondary-region cursor))))

;;;; Update cursor color on theme change

(defun hel--update-main-cursor-color (color)
  (set-face-attribute 'hel-normal-state-main-cursor nil
                      :background color
                      :foreground (face-background 'default))
  (hel-set-emacs-cursor))

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
