;;; hel-core.el --- Core functionality -*- lexical-binding: t -*-
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
;; Hel states are similar to Emacs minor modes, but they are not minor modes
;; in the sense that they are not created with `define-minor-mode' macro.
;;
;; The internal mechanism in general terms is as follows: `hel-mode-map-alist'
;; symbol is stored in `emulation-mode-map-alists' list, and keymap bound to it
;; is changed on every Hel state change.
;;
;; Every state has general globally shared keymap, and "nested" keymaps that are
;; stored in other keymaps (typical example are major-mode maps) under special
;; keys like "<normal-state>" or "<insert-state>", that are associated with
;; particular Hel states and can not be produced by a keyboard. On every Hel
;; state change, the algorithm traverses all currently active keymaps looking
;; for these keys, and activates nested keymaps associated with them.
;;
;;; Code:

(eval-when-compile (require 'hel-macros))
(require 'cl-lib)
(require 'map)
(require 'dash)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-multiple-cursors-core)

;; Declarations
(defvar edebug-mode)
(defvar edebug-mode-map)

;;; Hel mode

(define-minor-mode hel-local-mode
  "Minor mode for setting up Hel in a current buffer."
  :global nil
  (if hel-local-mode
      (progn
        ;; Just push the symbol into `emulation-mode-map-alists'.
        ;; We will update its content on every Hel state change.
        (cl-pushnew 'hel-mode-map-alist emulation-mode-map-alists)
        (setq-local hel--cursors-table (make-hash-table :test 'eql :weakness t))
        (hel-load-whitelists)
        (add-hook 'pre-command-hook  #'hel--pre-command-hook 90 t)
        (add-hook 'post-command-hook #'hel--post-command-hook 90 t)
        (add-hook 'after-revert-hook #'hel-disable-multiple-cursors-mode 90 t)
        (setq hel-input-method current-input-method)
        (add-hook 'input-method-activate-hook #'hel-activate-input-method 90 t)
        (add-hook 'input-method-deactivate-hook #'hel-deactivate-input-method 90 t)
        (hel--check-if-cursor-is-hidden)
        (hel-switch-state (hel-initial-state)))
    ;; else
    (remove-hook 'post-command-hook #'hel--post-command-hook t)
    (remove-hook 'pre-command-hook  #'hel--pre-command-hook t)
    (remove-hook 'after-revert-hook #'hel-disable-multiple-cursors-mode t)
    (remove-hook 'input-method-activate-hook #'hel-activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'hel-deactivate-input-method t)
    (hel--single-undo-step-end)
    (hel-disable-multiple-cursors-mode)
    (setq hel-this-command nil
          hel--input-cache nil)
    (hel-disable-current-state)
    (activate-input-method hel-input-method)))

(put 'hel-local-mode 'permanent-local t)

;; On a major-mode change Emacs executes `kill-all-local-variables', which
;; drops this function from the buffer-local `after-revert-hook'. Then it runs
;; the TURN-ON function of every globalized minor mode — `hel--initialize' in
;; our case. But `hel-local-mode' is permanent-local (see above), so it is still
;; non-nil, `hel--initialize' takes the `hel-switch-state' branch, the body
;; of `hel-local-mode' does not re-run, and nothing restores the entry.
;; The `permanent-local-hook' property makes `kill-all-local-variables' keep it.
(put 'hel-disable-multiple-cursors-mode 'permanent-local-hook t)

;;;###autoload (autoload 'hel-mode "hel" nil t)
(define-globalized-minor-mode hel-mode hel-local-mode hel--initialize
  :group 'hel
  (if hel-mode
      (progn
        (setq scroll-conservatively 101
              scroll-margin 0)
        (dolist (fun-how-advice hel--advices)
          (apply #'advice-add fun-how-advice))
        (when hel-want-minibuffer
          (add-hook 'minibuffer-setup-hook #'hel-local-mode))
        (add-hook 'window-buffer-change-functions #'hel--fundamental-mode-hack)
        (add-hook 'change-major-mode-after-body-hook #'hel--check-if-cursor-is-hidden)
        (add-hook 'window-configuration-change-hook #'hel-update-cursor)
        (add-hook 'enable-theme-functions  #'hel--on-theme-change)
        (add-hook 'disable-theme-functions #'hel--on-theme-change)
        ;; Setup ESC, C-i and C-m keys
        (-each (frame-list) #'hel-setup-terminal-keys)
        (add-hook 'after-make-frame-functions #'hel-setup-terminal-keys)
        (hel-setup-mode-line))
    ;; else
    (setq scroll-conservatively (custom--standard-value 'scroll-conservatively)
          scroll-margin (custom--standard-value 'scroll-margin))
    (cl-loop for (fun _how advice) in hel--advices
             do (advice-remove fun advice))
    (remove-hook 'minibuffer-setup-hook #'hel-local-mode)
    (remove-hook 'window-buffer-change-functions #'hel--fundamental-mode-hack)
    (remove-hook 'change-major-mode-after-body-hook #'hel--check-if-cursor-is-hidden)
    (remove-hook 'window-configuration-change-hook #'hel-update-cursor)
    (remove-hook 'enable-theme-functions  #'hel--on-theme-change)
    (remove-hook 'disable-theme-functions #'hel--on-theme-change)
    (hel-setup-mode-line :remove)))

(defun hel--initialize ()
  "Turn on `hel-local-mode' in current buffer if appropriate."
  (cond (hel-local-mode
         ;; Set Hel state according to new major-mode.
         (hel-switch-state (hel-initial-state)))
        ((not (minibufferp))
         (hel-local-mode 1))))

(defun hel--fundamental-mode-hack (_)
  "Activate `hel-local-mode' in current buffer if it is in `fundamental-mode'.
Emacs sometimes creates random empty buffers in `fundamental-mode'.
For these buffers `after-change-major-mode-hook' is not called, so
they remain invisible to `define-globalized-minor-mode'. This function
ensures `hel-local-mode' is activated in such cases."
  (when (and (eq major-mode 'fundamental-mode)
             (null hel-local-mode))
    (hel-local-mode 1)))

(hel-define-advice select-window (:after (&rest _))
  (hel-update-cursor))

(hel-advice-add 'use-global-map :after #'hel-update-active-keymaps-a)
(hel-advice-add 'use-local-map  :after #'hel-update-active-keymaps-a)

;;; ESC, C-i and C-m keys

(defun hel-esc (map)
  "Translate `\\e' to `escape' if no further event arrives."
  (if (and (or (not (display-graphic-p))
               hel-want-gui-esc-translation)
           (not hel-inhibit-esc)
           (or hel-local-mode
               (active-minibuffer-window))
           (let ((keys (this-single-command-keys)))
             (and (length> keys 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for hel-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(defun hel-setup-terminal-keys (frame)
  "Make Emacs correctly handle ESC in terminal, and distinguish TAB from
C-i and RET from C-m.
In GUI frames, also translate ESC when `hel-want-gui-esc-translation'
is non-nil."
  (with-selected-frame frame
    (if (eq t (terminal-live-p (frame-terminal frame)))
        ;; Text terminal.
        ;; Guard to run only once per terminal: `input-decode-map' is
        ;; terminal-local, but this function runs once per frame.
        (unless (terminal-parameter nil 'hel--terminal-keys-set-up)
          (set-terminal-parameter nil 'hel--terminal-keys-set-up t)
          ;; Kitty keyboard protocol:
          ;; https://sw.kovidgoyal.net/kitty/keyboard-protocol/
          (define-key input-decode-map "\e[105;5u" [C-i])
          (define-key input-decode-map "\e[109;5u" [C-m])
          (keymap-set input-decode-map
                      "ESC" `( menu-item ""
                               ,(keymap-lookup input-decode-map "ESC")
                               :filter hel-esc)))
      ;; GUI Emacs
      (keymap-set input-decode-map "C-i" [C-i])
      (keymap-set input-decode-map "C-m" [C-m])
      (unless (terminal-parameter nil 'hel--terminal-keys-set-up)
        (set-terminal-parameter nil 'hel--terminal-keys-set-up t)
        (keymap-set input-decode-map
                    "ESC" `( menu-item ""
                             ,(keymap-lookup input-decode-map "ESC")
                             :filter hel-esc))))))

;;; Hel states

(defmacro hel-define-state (state doc &rest body)
  "Define new Hel STATE.
DOC is a general description and shows up in all docstrings.
BODY is executed each time the state is enabled or disabled.

Optional KEY keyword arguments:

`:keymap'        Keymap that will be active while Hel is in STATE.
               Can be accessed later via `hel-STATE-state-map' variable
               or `hel-state-property' funciton.

`:cursor'        Cursor apperance when Hel is in STATE.
               Can be a cursor type as per `cursor-type', a color string
               as passed to `set-cursor-color', a list of them, or a
               zero-argument function for changing the cursor appearence.
               Can be accessed later via `hel-state-property' function.

`:input-method'  When non-nil Hell will activate the enabled input method
               on switching to STATE.

`:modes'         A list of major and minor modes for which Hel’s initial
               state is STATE. Use `hel-set-initial-state' to register
               additional modes later.

`:tag'           Mode-line tag shown while Hel is in STATE.  Either a string,
               or a zero-argument function returning a string. Can be accessed
               later via `hel-state-property' function.

Also two hooks are defined which are run each time Hel enter or exit STATE:
- `hel-STATE-state-enter-hook'
- `hel-STATE-state-exit-hook'

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug ( &define name
                    [&optional stringp]
                    [&rest [keywordp sexp]]
                    def-body)))
  (-let* ((state-name (concat (capitalize (symbol-name state)) " state"))
          (symbol     (intern (format "hel-%s-state" state)))
          (variable   symbol)
          (keymap     (intern (format "%s-map" symbol)))
          (enter-hook (intern (format "%s-enter-hook" symbol)))
          (exit-hook  (intern (format "%s-exit-hook" symbol)))
          ;; collect keywords
          ((kwargs . body) (hel-split-keyword-args body))
          ((&plist :keymap keymap-value
                   :cursor :input-method :modes :tag) kwargs))
    ;; macro expansion
    `(progn
       ;; State variable
       (hel-defvar-permanent-local ,variable nil ,(format "Non nil if Hel is in %s." state-name))
       ;; Hooks
       (defvar ,enter-hook nil ,(format "Hooks to run on entry %s." state-name))
       (defvar ,exit-hook  nil ,(format "Hooks to run on exit %s." state-name))
       ;; Keymap
       (defvar ,keymap ,(or keymap-value '(make-sparse-keymap))
         ,(format "Global keymap for Hel %s." state-name))
       ;; Save state properties in `hel-state-properties' for runtime lookup.
       (setf (alist-get ',state hel-state-properties)
             (list :name         ,state-name
                   :variable     ',variable
                   :function     ',symbol
                   :keymap       ,keymap
                   :cursor       ,cursor
                   :input-method ,input-method
                   :modes        ,modes
                   :tag          ,tag))
       ;; State function
       (defun ,symbol (&optional arg)
         ,(format "Switch Hel to %s.
When ARG is non-positive integer and Hel is in %s — disable it.\n\n%s"
                  state-name state-name doc)
         (interactive)
         (if (and (numberp arg) (< arg 1))
             ;; disable STATE
             (when (eq hel-state ',state)
               (setq hel-state nil
                     hel-previous-state ',state
                     ,variable nil)
               ,@body
               (run-hooks ',exit-hook))
           ;; enable STATE
           (unless hel-local-mode (hel-local-mode))
           (hel-disable-current-state)
           (setq hel-state ',state
                 ,variable t)
           (let ((input-method-activate-hook nil)
                 (input-method-deactivate-hook nil))
             ,(if input-method
                  '(activate-input-method hel-input-method)
                '(deactivate-input-method)))
           ,@body
           ;; Switch color and shape of all cursors.
           ;; main cursor
           (setq hel--extend-selection nil)
           (hel-update-cursor)
           ;; fake cursors
           (when hel-multiple-cursors-mode
             (hel-save-window-scroll
               (hel-save-excursion
                 (dolist (cursor (hel-all-fake-cursors))
                   (hel-with-fake-cursor cursor
                     (setq hel--extend-selection nil))))))
           (run-hooks ',enter-hook))
         (hel-update-active-keymaps)
         (force-mode-line-update)))))

(defun hel-state-p (symbol)
  "Return non-nil if SYMBOL corresponds to Hel state."
  (assq symbol hel-state-properties))

(defun hel-switch-state (state)
  "Switch Hel into STATE."
  (if (eq state hel-state)
      (progn
        (hel-update-active-keymaps)
        (hel-update-cursor))
    ;; else
    (-> (hel-state-property state :function)
        (funcall 1))))

(defun hel-switch-to-initial-state ()
  (hel-switch-state (hel-initial-state)))

(defun hel-disable-current-state ()
  "Disable current Hel state."
  (when hel-state
    (-> (hel-state-property hel-state :function)
        (funcall -1))))

(defun hel-state-property (state property)
  "Return the value of PROPERTY for STATE.
PROPERTY is a keyword as used by `hel-define-state'.
STATE is the state's symbolic name.

`hel-state-property' is a generalized variable, i.e. and you can use it as
PLACE in `setf'."
  (-> (alist-get state hel-state-properties)
      (plist-get property)))

(gv-define-setter hel-state-property (value state property)
  `(setf (-> (alist-get ,state hel-state-properties)
             (plist-get ,property))
         ,value))

(defun hel-initial-state (&optional buffer)
  "Return the state in which Hel should start in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (if (minibufferp) 'insert)
        ;; Check minor modes
        (cl-loop for (mode) in minor-mode-map-alist
                 when (and (boundp mode)
                           (symbol-value mode))
                 thereis (hel-initial-state-for-mode mode))
        ;; Check major mode
        (hel-initial-state-for-mode major-mode t)
        ;; Temporarily strip Hel's emulation keymaps to inspects the major
        ;; mode's own bindings.
        (let ((hel-mode-map-alist nil))
          (if (hel-letters-are-self-insert-p) 'normal 'emacs)))))

(defun hel-initial-state-for-mode (mode &optional follow-parent checked-modes)
  "Return the Hel state to use for MODE or its alias.
The initial state for MODE should be set beforehand by the
`hel-set-initial-state' function.

If FOLLOW-PARENT is non-nil, also check parent modes of MODE and its alias.

CHECKED-MODES is used internally and should not be set initially."
  (when (memq mode checked-modes)
    (error "Circular reference detected in ancestors of `%s'\n%s"
           major-mode checked-modes))
  (let ((mode-alias (if-let* ((func (symbol-function mode))
                              ((symbolp func)))
                        func)))
    (or (->> hel-state-properties
             (-any (-lambda ((state . properties))
                     (if-let* ((modes (plist-get properties :modes))
                               ((or (memq mode modes)
                                    (if mode-alias
                                        (memq mode-alias modes)))))
                         state))))
        (if-let* ((follow-parent)
                  (parent (get mode 'derived-mode-parent)))
            (hel-initial-state-for-mode parent t (cons mode checked-modes)))
        (if-let* ((follow-parent)
                  (mode-alias)
                  (parent (get mode-alias 'derived-mode-parent)))
            (hel-initial-state-for-mode parent t
                                        (cons mode-alias checked-modes))))))

(defun hel-set-initial-state (mode state)
  "Set the Hel initial STATE for the major MODE.
MODE and STATE should be symbols."
  ;; Remove current settings.
  (-each hel-state-properties
    (-lambda ((_ . plist))
      (cl-symbol-macrolet ((modes (plist-get plist :modes)))
        (setf modes (delq mode modes)))))
  ;; Add new settings.
  (cl-pushnew mode (-> hel-state-properties
                       (map-elt state)
                       (map-elt :modes))))

(defun hel-setup-mode-line (&optional remove?)
  ;; state tag
  (cl-symbol-macrolet
      ((section '(hel-local-mode (:eval (hel-state-mode-line-tag))))
       (modeline (default-value 'mode-line-format)))
    (cond (remove?
           (setq-default mode-line-format (remove section modeline)))
          ((not (member section modeline))
           (setq-default mode-line-format (cons section modeline)))))
  ;; number of cursors, search info
  (let ((section '(hel-local-mode hel-mode-line-info)))
    (if remove?
        (cl-callf2 remove section mode-line-misc-info)
      (add-to-list 'mode-line-misc-info section))))

(cl-defun hel-state-mode-line-tag (&optional (state hel-state))
  "Return the STATE tag for modeline."
  (when state
    (let ((tag (pcase (hel-state-property state :tag)
                 ((and (pred functionp) fun)
                  (funcall fun))
                 (tag tag)))
          (face (let ((face (intern (format "hel-%s-state-tag" state))))
                  (if (facep face)
                      face
                    'mode-line-emphasis))))
      (when (stringp tag)
        (propertize tag
                    'face face
                    'help-echo (hel-state-property state :name)
                    'mouse-face 'mode-line-highlight)))))

;;; Normal, Insert and Emacs states

(hel-define-state normal
  "Normal state."
  :tag " NORMAL"
  :keymap (define-keymap :full t :suppress t)
  :cursor (list hel-normal-state-cursor-type
                (lambda ()
                  (if hel--extend-selection
                      'hel-extend-selection-cursor
                    'hel-normal-state-main-cursor))))

(hel-define-state insert
  "Insert state."
  :tag " INSERT"
  :cursor (list hel-insert-state-cursor-type
                'hel-insert-state-main-cursor) ; face
  :input-method t
  (if hel-insert-state
      (progn
        (setq hel-undo--previous-command-kind 'other
              hel--region-was-active-on-insert
              (and hel-reactivate-selection-after-insert-state
                   (region-active-p)))
        (hel-with-each-cursor
          (deactivate-mark)))
    ;; else
    (hel-push-point)
    (when hel--region-was-active-on-insert
      (hel-with-each-cursor
        (activate-mark)))))

(hel-define-state emacs
  "Emacs state."
  :cursor (list hel-emacs-state-cursor-type
                'hel-emacs-state-main-cursor) ; face
  (setq hel--extend-selection nil)
  (deactivate-mark))

;;; Keymaps

(defun hel-update-active-keymaps ()
  "Rebuild `hel-mode-map-alist' for the current Hel state.
It is likely that you need `hel-maybe-update-active-keymaps' instead."
  (setq hel-mode-map-alist
        (if-let* ((state hel-state))
            ;; Order matters: the first found binding will be accepted,
            ;; so earlier keymaps has higher priority.
            `(
              ;; Edebug takes precedence over all other keymaps
              ,@(if (bound-and-true-p edebug-mode)
                    (list `(edebug-mode . ,edebug-mode-map)))
              ;; Multiple cursors related keys should take precedence over
              ;; all others when `hel-multiple-cursors-mode' is active.
              ,@(if-let* ((hel-multiple-cursors-mode)
                          (map (hel-get-nested-hel-keymap
                                hel-multiple-cursors-mode-map state)))
                    (list `(hel-multiple-cursors-mode . ,map)))
              ;; Hel buffer local overriding map
              ,@(if-let* ((map (hel-get-nested-hel-keymap
                                hel-overriding-local-map state)))
                    (list `(:hel-overriding-local-map . ,map)))
              ;; Hel keymaps nested in other keymaps
              ,@(-keep (lambda (keymap)
                         ;; Unless already collected above.
                         (unless (eq keymap hel-multiple-cursors-mode-map)
                           (if-let* ((hel-map (hel-get-nested-hel-keymap keymap state)))
                               (cons (hel-minor-mode-for-keymap keymap) hel-map))))
                       (current-active-maps))
              ;; Main state keymap
              ,(cons (hel-state-property state :variable)
                     (hel-state-property state :keymap))))))

(defun hel--keymap-fingerprint-changed-p ()
  "Return non-nil if `hel-mode-map-alist' must be rebuilt."
  (let* ((needed (+ 10 (* 2 (+ (length minor-mode-overriding-map-alist)
                               (length minor-mode-map-alist)))))
         (vec (if (and hel--keymap-fingerprint
                       (<= needed (length hel--keymap-fingerprint)))
                  hel--keymap-fingerprint
                (setq hel--keymap-fingerprint (make-vector needed nil))))
         (i 0)
         (changed nil))
    ;; `record' must be a macro. If we make it a lambda that captures and
    ;; assigns to VEC, I and CHANGED, none of the three can stay in a stack
    ;; slot: the byte compiler moves each one into a cons cell, so that the
    ;; lambda and the code around it share a value. That is three cons cell
    ;; allocations per call, and this runs after every command. A macro
    ;; expands inline with no extra allocations.
    (cl-macrolet ((record (form)
                    `(let ((object ,form))
                       (unless (eq (aref vec i) object)
                         (aset vec i object)
                         (setq changed t))
                       (cl-incf i))))
      ;; Every source `current-active-maps' is built from, plus some extra
      ;; that `hel-update-active-keymaps' reads.
      (record hel-state)
      (record hel-multiple-cursors-mode)
      (record (bound-and-true-p edebug-mode))
      (record hel-overriding-local-map)
      (record overriding-terminal-local-map)
      (record overriding-local-map)
      (record (get-char-property (point) 'keymap))
      (record (get-char-property (point) 'local-map))
      (record (current-local-map))
      (record (current-global-map))
      (cl-loop for (mode . keymap) in minor-mode-overriding-map-alist
               when (and (boundp mode)
                         (symbol-value mode))
               do (record mode)
                  (record keymap))
      (cl-loop for (mode . keymap) in minor-mode-map-alist
               when (and (boundp mode)
                         (symbol-value mode))
               do (record mode)
                  (record keymap)))
    ;; If a minor mode was turned off, `i' will be smaller than the recorded
    ;; count. Comparing the count catches this.
    (unless (= i hel--keymap-fingerprint-length)
      (setq hel--keymap-fingerprint-length i
            changed t))
    changed))

(defun hel-maybe-update-active-keymaps ()
  "Rebuild `hel-mode-map-alist' if anything it is built from has changed."
  (when (hel--keymap-fingerprint-changed-p)
    (hel-update-active-keymaps)))

(defun hel-update-active-keymaps-a (&rest _)
  "Rebuild `hel-mode-map-alist', ignoring the advised function's arguments."
  (hel-update-active-keymaps))

(defun hel-minor-mode-for-keymap (keymap)
  "Return the minor mode associated with KEYMAP or t if it doesn't have one."
  (when (symbolp keymap)
    (cl-callf symbol-value keymap))
  (or (car (rassq keymap minor-mode-overriding-map-alist))
      (car (rassq keymap minor-mode-map-alist))
      t))

(defun hel-get-nested-hel-keymap (keymap state &optional ignore-parent)
  "Get from KEYMAP the nested keymap associated with Hel STATE.
If IGNORE-PARENT is non-nil then Hel STATE keymap nested in KEYMAPs parent
keymap will be ignored."
  (when (and keymap state)
    (let* ((key (vector (intern (format "%s-state" state))))
           (hel-map (lookup-key keymap key)))
      (if (and hel-map
               (hel-nested-keymap-p hel-map)
               (not (and-let* ((ignore-parent)
                               (parent (keymap-parent keymap))
                               ((eq (lookup-key parent key)
                                    hel-map))))))
          hel-map))))

(defun hel-create-nested-hel-keymap (keymap state)
  "Create a nested keymap for Hel STATE inside the given KEYMAP."
  (let ((hel-map (make-sparse-keymap))
        (key (vector (intern (format "%s-state" state))))
        (prompt (format "Hel keymap for %s"
                        (or (hel-state-property state :name)
                            (format "%s state" state)))))
    (hel-set-keymap-prompt hel-map prompt)
    (define-key keymap key hel-map)
    hel-map))

(defun hel-set-keymap-prompt (keymap prompt)
  "Set the prompt-string of the KEYMAP to PROMPT."
  (delq (keymap-prompt keymap) keymap)
  (when prompt
    (setcdr keymap (cons prompt (cdr keymap)))))

(defun hel-nested-keymap-p (keymap)
  "Return non-nil if KEYMAP is a Hel nested keymap."
  (and-let* ((prompt (keymap-prompt keymap))
             ((string-prefix-p "Hel keymap" prompt)))))

;;;###autoload (autoload 'hel-keymap-set "hel" nil t)
(defun hel-keymap-set (keymap &rest args)
  "Bind KEY to DEFINITION in KEYMAP.

STATE is an optional keyword argument that restricts the binding to
a Hel modal state. Can be a symbol or list of symbols.

KEY and DEFINITION arguments are like those in `keymap-set'.
If DEFINITION is nil, the corresponding key binding will be removed from KEYMAP.
Any number of KEY / DEFINITION pairs can be provided.

Without STATE, this function works like `keymap-set' except that multiple
keybindings can be set at once.

Example:

   (hel-keymap-set keymap :state \\='(normal emacs)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn KEYMAP [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (-let* ((((&plist :state) . args) (hel-split-keyword-args args))
          (maps (if state
                    (-map (lambda (state)
                            (cl-assert (hel-state-p state) t "Unknown Hel state")
                            (or (hel-get-nested-hel-keymap keymap state t)
                                (hel-create-nested-hel-keymap keymap state)))
                          (ensure-list state))
                  (list keymap)))
          (_ (cl-assert (cl-evenp (length args)) nil
                        "The number of [KEY DEFINITION] pairs is not even"))
          ((bind unbind) (->> args
                              (-partition 2)
                              (-separate #'-second-item)))
          (unbind (-flatten unbind)))
    (dolist (map maps)
      (-each unbind (lambda (key)
                      (keymap-unset map key t)))
      (-each bind (-lambda ((key definition))
                    (keymap-set map key definition)))))
  keymap)

;;;###autoload (autoload 'hel-keymap-global-set "hel" nil t)
(defun hel-keymap-global-set (&rest args)
  "Create keybinding from KEY to DEFINITION in `global-map'.

STATE is an optional keyword argument that restricts the binding to
a Hel modal state. Can be a symbol or list of symbols.

KEY, DEFINITION arguments are like those of `keymap-global-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

Without STATE, this function works like `keymap-global-set' except that
multiple keybindings can be set at once.

Example:

   (hel-keymap-global-set :state \\='(normal emacs)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (-let* ((((&plist :state) . args) (hel-split-keyword-args args))
          (maps (if state
                    (-map (lambda (state)
                            (cl-assert (hel-state-p state) t "Unknown Hel state")
                            (hel-state-property state :keymap))
                          (ensure-list state))
                  (list (current-global-map)))))
    (cl-assert (cl-evenp (length args)) nil
               "The number of [KEY DEFINITION] pairs is not even")
    (dolist (map maps)
      (cl-loop for (key definition) on args by #'cddr
               do (if definition
                      (keymap-set map key definition)
                    (keymap-unset map key t))))))

(defun hel-keymap-local-set (&rest args)
  "Create keybinding from KEY to DEFINITION in current buffer local keymap.
It is the one that is set with `use-local-map' and in most cases it is the
major-mode keymap — i.e. it is shared with all other buffers in the same
major mode.

STATE is an optional keyword argument that restricts the binding to
a Hel modal state. Can be a symbol or list of symbols.

KEY, DEFINITION arguments are like those of `keymap-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((local-map (or (current-local-map)
                       (-doto (make-sparse-keymap)
                         (use-local-map)))))
    (apply #'hel-keymap-set local-map args)))

(defun hel-keymap-overriding-set (&rest args)
  "Create buffer-local keybindings from KEY to DEFINITION for Hel STATE which
take precedence over all others.

STATE is an optional keyword argument that restricts the binding to
a Hel modal state. Can be a symbol or list of symbols.

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless hel-overriding-local-map
    (setq hel-overriding-local-map (make-sparse-keymap)))
  (apply #'hel-keymap-set hel-overriding-local-map args)
  (hel-update-active-keymaps))

;;; Command loop hooks

(defun hel--pre-command-hook ()
  "Hook run before each command is executed. See `pre-command-hook'."
  (when (and hel--extend-selection (not mark-active))
    (set-mark (point)))
  (unless hel-executing-command-for-fake-cursor
    (setq hel-this-command this-command)
    (cond (hel-normal-state
           (hel--single-undo-step-beginning))
          (hel-insert-state
           (hel--maybe-split-undo-step)))))

(defun hel--post-command-hook ()
  "Hook run after each command is executed. See `post-command-hook'."
  (unless hel-executing-command-for-fake-cursor
    (when (and hel-multiple-cursors-mode
               (not (eq hel-this-command #'ignore))
               ;; TODO: This condition skips keyboard macros. We need to handle
               ;; them! They will generate actual commands that are also run in
               ;; the command loop.
               (functionp hel-this-command))
      ;; Wrap in `condition-case' to protect this function from being removed
      ;; from `post-command-hook', because the function throwing the error is
      ;; unconditionally removed from it.
      (condition-case err
          (progn
            (hel--execute-command-for-all-fake-cursors hel-this-command)
            (when (hel--merge-cursors-p hel-this-command)
              (hel-merge-overlapping-cursors)))
        (error
         (message "[Hel] error while executing command for fake cursor: %s"
                  (error-message-string err)))
        (quit))) ;; "C-g" during multistage command.
    (when hel-normal-state
      (condition-case err
          (hel--single-undo-step-end)
        (error
         (message "[Hel] error while closing the undo step: %s"
                  (error-message-string err)))))
    (setq hel-this-command nil
          hel--input-cache nil)
    (hel-maybe-update-active-keymaps)))

(put 'hel--pre-command-hook 'permanent-local-hook t)
(put 'hel--post-command-hook 'permanent-local-hook t)

;;; Undo

(hel-defvar-permanent-local hel--in-single-undo-step nil
  "Non-nil while we are in the single undo step.")

(defun hel--single-undo-step-beginning ()
  "Open an undo step.
All following buffer modifications are grouped together as a single
action. The step is terminated with `hel--single-undo-step-end'."
  (unless (or hel--in-single-undo-step
              (eq buffer-undo-list t))
    (setq hel--in-single-undo-step t)
    (when (car-safe buffer-undo-list)
      (undo-boundary))
    (setq hel--buffer-undo-list-pointer buffer-undo-list
          hel-undo--cursors-positions (hel-cursors-positions))))

(defun hel--single-undo-step-end ()
  "Finalize undo step started by `hel--single-undo-step-beginning'."
  (when hel--in-single-undo-step
    (unwind-protect
        (unless (or (eq buffer-undo-list t)
                    (eq buffer-undo-list hel--buffer-undo-list-pointer)
                    ;; Do not record undo step when the command replayed
                    ;; the undo history instead of editing the buffer.
                    ;;
                    ;; Recognises a replay done by `undo'
                    (hel--buffer-undo-list-tip-is-redo-record-p)
                    ;; Recognises a replay done by `undo-redo'
                    (not (hel--buffer-undo-list-pointer-reachable-p)))
          (hel--merge-undo-step))
      (setq hel--in-single-undo-step nil
            hel--buffer-undo-list-pointer nil
            hel-undo--cursors-positions nil))))

(defun hel--buffer-undo-list-tip-is-redo-record-p ()
  "Return non-nil if the tip of `buffer-undo-list' was produced by an undo.
Emacs marks such records in `undo-equiv-table'."
  (let ((undo-list buffer-undo-list))
    (while (and (consp undo-list) (null (car undo-list)))
      (setq undo-list (cdr undo-list)))
    (gethash undo-list undo-equiv-table)))

(defun hel--buffer-undo-list-pointer-reachable-p ()
  "Return non-nil if `hel--buffer-undo-list-pointer' is still part of
`buffer-undo-list'."
  (let ((tail buffer-undo-list))
    (while (and (consp tail)
                (not (eq tail hel--buffer-undo-list-pointer)))
      (setq tail (cdr tail)))
    (eq tail hel--buffer-undo-list-pointer)))

(defun hel--merge-undo-step ()
  "Merge everything up to `hel--buffer-undo-list-pointer' in single undo step."
  ;; Remove undo boundaries (nil elements) from `buffer-undo-list' withing
  ;; current undo step. Also remove number entries -- they move point during
  ;; undo, and we handle cursors positions manually to synchronize real cursor
  ;; with fake ones.
  (let ((undo-list (hel-destructive-filter
                    (lambda (i) (or (numberp i) (null i)))
                    buffer-undo-list
                    hel--buffer-undo-list-pointer)))
    (if (eq undo-list hel--buffer-undo-list-pointer)
        ;; The command recorded nothing in undo list.
        (setq buffer-undo-list undo-list)
      ;; Else put on the both ends of the undo step records that hold the
      ;; positions of every cursor, so that undo can put the cursors back.
      (setq buffer-undo-list
            (cons `(apply hel--undo-step-start ,(hel-cursors-positions))
                  undo-list))
      (let ((tail undo-list))
        (while (not (eq (cdr tail) hel--buffer-undo-list-pointer))
          (setq tail (cdr tail)))
        (setcdr tail (cons `(apply hel--undo-step-end ,hel-undo--cursors-positions)
                           hel--buffer-undo-list-pointer))))))

(defun hel-commit-undo-checkpoint ()
  "Finish current undo step and starts the new one.
What was edited so far becomes one undo step, and what follows starts
a new one."
  (if hel--in-single-undo-step
      (progn
        (hel--single-undo-step-end)
        (hel--single-undo-step-beginning))
    (undo-boundary)))

(defun hel-commit-undo-checkpoint-a (&rest _)
  "Finish current undo step and starts the new one.
For use as `:before' advice on a function that edits the buffer on its
own, so that its edit becomes an undo step separate from the typing
around it."
  (hel-commit-undo-checkpoint))

(defun hel--undo-split-between-p (previous current)
  "Return non-nil if the undo step must be split between two operations.
PREVIOUS and CURRENT are kinds as returned by `hel-undo--command-kind'."
  (let ((spaces '(typing-first-space typing-consecutive-space))
        (typing '(typing-other typing-first-space typing-consecutive-space)))
    (cond
     ;; Anything that is not typing starts a step of its own.
     ((and (memq previous typing)
           (not (memq current typing)))
      t)
     ;; A single space belongs to the word that follows it, so that one undo
     ;; takes back a whole word:  "abc |d" does not split, "abc  |d" does.
     ((eq previous 'typing-first-space)
      nil)
     ;; Typing, spacing and everything else are three different kinds.
     (t
      (let ((p (if (memq previous spaces) 'space previous))
            (c (if (memq current spaces) 'space current)))
        (not (eq p c)))))))

(defun hel--maybe-split-undo-step ()
  "Split the undo step when the kind of editing changes.
Does nothing when `hel-want-fine-undo' is nil."
  (when hel-want-fine-undo
    (let ((previous hel-undo--previous-command-kind)
          (current (hel-undo--command-kind)))
      (setq hel-undo--previous-command-kind current)
      (when (hel--undo-split-between-p previous current)
        (hel-commit-undo-checkpoint)))))

(defun hel-undo--command-kind ()
  "Classify the command that is about to run, for `hel-want-fine-undo'.
Return one of the symbols:
- `typing-other'
- `typing-first-space'
- `typing-consecutive-space'
- `other'."
  (if (hel-self-insert-command-p this-command)
      (if (eq last-command-event ?\s)
          (if (memq hel-undo--previous-command-kind '(typing-first-space
                                                      typing-consecutive-space))
              'typing-consecutive-space
            'typing-first-space)
        'typing-other)
    'other))

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

;;; Input-method

(defun hel-activate-input-method ()
  "Enable input method in Hel states with `:input-method' property set."
  (when (and hel-local-mode hel-state)
    (setq hel-input-method current-input-method)
    (unless (hel-state-property hel-state :input-method)
      (let ((input-method-activate-hook nil)
            (input-method-deactivate-hook nil))
        (deactivate-input-method)))))

(defun hel-deactivate-input-method ()
  "Disable input method in all states."
  (setq hel-input-method nil))

(put 'hel-activate-input-method 'permanent-local-hook t)
(put 'hel-deactivate-input-method 'permanent-local-hook t)

(defmacro hel-with-input-method (&rest body)
  "Execute body with current input method active."
  (declare (indent defun))
  `(if hel-input-method
       (unwind-protect
           (progn
             (remove-hook 'input-method-activate-hook #'hel-activate-input-method t)
             (remove-hook 'input-method-deactivate-hook #'hel-deactivate-input-method t)
             (prog2
                 (activate-input-method hel-input-method)
                 (progn ,@body)
               (deactivate-input-method)))
         (add-hook 'input-method-activate-hook #'hel-activate-input-method 90 t)
         (add-hook 'input-method-deactivate-hook #'hel-deactivate-input-method 90 t))
     ;; else
     ,@body))

(defun hel--with-input-method-a (orig-fun &rest args)
  (hel-with-input-method
    (apply orig-fun args)))

(hel-advice-add 'read-char :around #'hel--with-input-method-a)
;; (hel-advice-add 'read-char-from-minibuffer :around #'hel--with-input-method-a)

(defun hel--refresh-input-method-a (orig-fun &rest args)
  "Refresh `hel-input-method'."
  (cond ((not hel-local-mode)
         (apply orig-fun args))
        ((hel-state-property hel-state :input-method)
         (apply orig-fun args))
        (t
         (let ((current-input-method hel-input-method))
           (apply orig-fun args)))))

(hel-advice-add 'toggle-input-method :around #'hel--refresh-input-method-a)

;;; Cursor shape and color

(defvar-local hel--cursor-hidden? nil
  "Whether the major mode of the current buffer hides the cursor.")

(defun hel--check-if-cursor-is-hidden ()
  "Remember whether the major mode of the current buffer hides the cursor."
  (setq hel--cursor-hidden? (and (local-variable-p 'cursor-type)
                                 (null cursor-type))))

(defun hel-update-cursor ()
  "Update the main cursor appearance in the selected window according to
current Hel state."
  (when (and (eq (window-buffer) (current-buffer))
             hel-local-mode
             (not hel--cursor-hidden?))
    (when-let* ((x (hel-state-property hel-state :cursor)))
      (if (proper-list-p x)
          (mapc #'hel-set-cursor x)
        (funcall #'hel-set-cursor x)))))

(defun hel-set-cursor (arg)
  "Set the main cursor's apperance.
ARG may be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a face the `:background' attribute of which will be used,
or a function with no arguments that returns any of above."
  (cond ((facep arg)
         (hel--set-cursor-color (face-background arg nil t)))
        ((stringp arg)
         (hel--set-cursor-color arg))
        ((functionp arg)
         (-some-> (ignore-errors (funcall arg))
           (hel-set-cursor)))
        (t
         (setq cursor-type arg))))

;;;; Update cursor color on theme change

(defun hel--set-cursor-color (color)
  ;; Cursor color can only be set for each frame but not for each buffer, also
  ;; `modify-frame-parameters' forces a redisplay, so only call it when the
  ;; color actually changes.
  (unless (equal color (frame-parameter nil 'cursor-color))
    (modify-frame-parameters (selected-frame) `((cursor-color . ,color)))))

(defun hel--update-main-cursor-color (color)
  (set-face-attribute 'hel-normal-state-main-cursor nil :background color)
  (hel-update-cursor))

(hel-advice-add 'set-cursor-color :after #'hel--update-main-cursor-color)

(defun hel--on-theme-change (_theme)
  (hel--update-main-cursor-color (face-background 'cursor)))

;;; .
(provide 'hel-core)
;;; hel-core.el ends here
