;;; hel-core.el --- Core functionality -*- lexical-binding: t -*-
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

(defun hel--pre-command-hook ()
  "Hook run before each command is executed. See `pre-command-hook'."
  (when (and hel--extend-selection (not mark-active))
    (set-mark (point)))
  (unless hel-executing-command-for-fake-cursor
    (setq hel-this-command this-command)
    ;; Use our undo mechanism only in Normal state. This will
    ;; - merge all changes in Insert state into one undo step;
    ;; - ignore buffers in Emacs state that use undo, like Dired.
    (when hel-normal-state
      (hel--single-undo-step-beginning))))

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
      (hel--single-undo-step-end))
    (setq hel-this-command nil
          hel--input-cache nil)))

(put 'hel--pre-command-hook 'permanent-local-hook t)
(put 'hel--post-command-hook 'permanent-local-hook t)

(define-minor-mode hel-local-mode
  "Minor mode for setting up Hel in a current buffer."
  :global nil
  (if hel-local-mode
      (progn
        ;; Just push the symbol into `emulation-mode-map-alists'.
        ;; We will update its content on every Hel state change.
        (cl-pushnew 'hel-mode-map-alist emulation-mode-map-alists)
        (hel-load-whitelists)
        ;; Multiple cursors related keys should take precedence over
        ;; all others when `hel-multiple-cursors-mode' is active.
        (setf (alist-get 'hel-multiple-cursors-mode minor-mode-overriding-map-alist)
              hel-multiple-cursors-mode-map)
        (add-hook 'pre-command-hook  #'hel--pre-command-hook 90 t)
        (add-hook 'post-command-hook #'hel--post-command-hook 90 t)
        (add-hook 'after-revert-hook #'hel-disable-multiple-cursors-mode 90 t)
        (setq hel-input-method current-input-method)
        (add-hook 'input-method-activate-hook #'hel-activate-input-method 90 t)
        (add-hook 'input-method-deactivate-hook #'hel-deactivate-input-method 90 t)
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
    (cl-callf map-delete minor-mode-overriding-map-alist 'hel-multiple-cursors-mode)
    (hel-disable-current-state)
    (activate-input-method hel-input-method)))

(put 'hel-local-mode 'permanent-local t)

;;;###autoload (autoload 'hel-mode "hel" nil t)
(define-globalized-minor-mode hel-mode hel-local-mode hel--initialize
  :group 'hel
  (if hel-mode
      (progn
        (dolist (fun-how-advice hel--advices)
          (apply #'advice-add fun-how-advice))
        (when hel-want-minibuffer
          (add-hook 'minibuffer-setup-hook #'hel-local-mode))
        (add-hook 'window-buffer-change-functions #'hel--fundamental-mode-hack)
        (add-hook 'window-configuration-change-hook #'hel-update-cursor)
        (add-to-list 'mode-line-misc-info 'hel-mode-line-info))
    ;; else
    (cl-loop for (fun _how advice) in hel--advices
             do (advice-remove fun advice))
    (remove-hook 'minibuffer-setup-hook #'hel-local-mode)
    (remove-hook 'window-buffer-change-functions #'hel--fundamental-mode-hack)
    (remove-hook 'window-configuration-change-hook #'hel-update-cursor)))

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
                   :cursor :input-method :modes) kwargs))
    ;; macro expansion
    `(progn
       ;; State variable
       (hel-defvar-local ,variable nil ,(format "Non nil if Hel is in %s." state-name))
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
                   :modes        ,modes))
       ;; State function
       (defun ,symbol (&optional arg)
         ,(format "Switch Hel into %s.
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
      ;; When state is unchanged only rescan keymaps.
      (hel-update-active-keymaps)
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
STATE is the state's symbolic name."
  (-> (alist-get state hel-state-properties)
      (plist-get property)))

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
      (setf (map-elt plist :modes)
            (delq mode (map-elt plist :modes)))))
  ;; Add new settings.
  (cl-pushnew mode (-> hel-state-properties
                       (map-elt state)
                       (map-elt :modes))))

;;; Normal, Insert and Emacs states

(hel-define-state normal
  "Normal state."
  :keymap (define-keymap :full t :suppress t)
  :cursor (list hel-normal-state-cursor-type
                (lambda ()
                  (if hel--extend-selection
                      'hel-extend-selection-cursor
                    'hel-normal-state-main-cursor))))

(hel-define-state insert
  "Insert state."
  :cursor (list hel-insert-state-cursor-type
                'hel-insert-state-main-cursor) ; face
  :input-method t
  (if hel-insert-state
      (progn
        (setq hel--region-was-active-on-insert
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

;;; Keymaps

(defun hel-update-active-keymaps ()
  "Reset keymaps for current Hel state."
  (hel-activate-state-keymaps hel-state))

(defun hel-update-active-keymaps-a (&rest _)
  "Refresh Hel keymaps."
  (hel-activate-state-keymaps hel-state))

(defun hel-activate-state-keymaps (state)
  "Set the value of the `hel-mode-map-alist' in the current buffer
according to the Hel STATE."
  (setq hel-mode-map-alist
        (if state
            ;; Order matters: the first found binding will be accepted,
            ;; so earlier keymaps has higher priority.
            `(
              ;; Edebug takes precedence over all other keymaps
              ,@(if (bound-and-true-p edebug-mode)
                    (list `(edebug-mode . ,edebug-mode-map)))
              ;; Hel buffer local overriding map
              ,@(if-let* ((map (hel-get-nested-hel-keymap hel-overriding-local-map state)))
                    (list `(:hel-overriding-local-map . ,map)))
              ;; Hel keymaps nested in other keymaps
              ,@(cl-loop for keymap in (current-active-maps)
                         for hel-map = (hel-get-nested-hel-keymap keymap state)
                         when hel-map
                         collect (cons (hel-minor-mode-for-keymap keymap) hel-map))
              ;; Main state keymap
              ,(cons (hel-state-property state :variable)
                     (hel-state-property state :keymap))))))

(defun hel-minor-mode-for-keymap (keymap)
  "Return the minor mode associated with KEYMAP or t if it doesn't have one."
  (when (symbolp keymap)
    (cl-callf symbol-value keymap))
  (or (car (rassq keymap minor-mode-map-alist))
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
  "Create keybinding from KEY to DEFINITION in KEYMAP.

STATE is an optional keyword argument that specifies the Hel state in
which the keybindings will be active. Can be a symbol or list of symbols.
It must appear before any KEY / DEFINITION pairs.

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

STATE is an optional keyword argument. If provided, keybindings are set in
the main keymap for specified Hel state. Can be a symbol or list of symbols.
It must appear before any KEY / DEFINITION pairs.

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

STATE is an optional keyword argument that specifies the Hel state
in which the keybindings will be active. It must appear before any
KEY / DEFINITION pairs.

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

STATE is an optional keyword argument that specifies the Hel state
in which the keybindings will be active. It must appear before any
KEY / DEFINITION pairs.

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless hel-overriding-local-map
    (setq hel-overriding-local-map (make-sparse-keymap)))
  (apply #'hel-keymap-set hel-overriding-local-map args)
  (hel-update-active-keymaps))

;;; Cursor shape and color

;; set-window-cursor-type
;; window-cursor-type

(defun hel-update-cursor ()
  "Update the main cursor appearence in current buffer according to
current Hel state."
  (when (eq (window-buffer) (current-buffer))
    (when-let* ((hel-local-mode)
                (x (hel-state-property hel-state :cursor)))
      (if (proper-list-p x)
          (-each x #'hel-set-cursor)
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

(defun hel--set-cursor-color (color)
  ;; Cursor color can only be set for each frame but not for each buffer, also
  ;; `modify-frame-parameters' forces a redisplay, so only call it when the
  ;; color actually changes.
  (unless (equal color (frame-parameter nil 'cursor-color))
    (modify-frame-parameters (selected-frame) `((cursor-color . ,color)))))

;;; .
(provide 'hel-core)
;;; hel-core.el ends here
