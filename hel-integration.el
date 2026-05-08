;;; hel-integration.el --- Integration with other packages -*- lexical-binding: t -*-
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
;;; Commentary:
;;
;;  Hel integration with other Emacs packages.
;;
;;; Code:

(eval-when-compile (require 'hel-macros))
(require 'hel-vars)
(require 'hel-multiple-cursors-core)
(require 'hel-common)
(require 'hel-core)
(require 'hel-commands)

;;; Integration multiple cursors with Emacs functionality

;; M-x
(hel-advice-add 'execute-extended-command :after #'hel--execute-for-all-cursors-a)
(put 'execute-extended-command 'multiple-cursors nil)

;; M-X
(hel-advice-add 'execute-extended-command-for-buffer :after #'hel--execute-for-all-cursors-a)
(put 'execute-extended-command-for-buffer 'multiple-cursors nil)

(hel-define-advice current-kill (:before (n &optional _do-not-move) hel)
  "Make sure pastes from other programs are added to `kill-ring's
of all cursors when yanking."
  (when-let* ((interprogram-paste (and (= n 0)
                                       interprogram-paste-function
                                       (funcall interprogram-paste-function))))
    (when (listp interprogram-paste)
      ;; Use `reverse' to avoid modifying external data.
      (cl-callf reverse interprogram-paste))
    ;; Add `interprogram-paste' to `kill-ring's of all cursors real and
    ;; fake. This is what `current-kill' do internally, but we have to do
    ;; it ourselves, because `interprogram-paste-function' is not a pure
    ;; function — it returns something only once.
    (let ((interprogram-cut-function nil)
          (interprogram-paste-function nil))
      ;; real cursor
      (if (listp interprogram-paste)
          (mapc 'kill-new interprogram-paste)
        (kill-new interprogram-paste))
      ;; fake cursors
      (dolist (cursor (hel-all-fake-cursors))
        (let ((kill-ring (overlay-get cursor 'kill-ring))
              (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
          (if (listp interprogram-paste)
              (mapc 'kill-new interprogram-paste)
            (kill-new interprogram-paste))
          (overlay-put cursor 'kill-ring kill-ring)
          (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer))))))

(hel-define-advice execute-kbd-macro (:around (orig-fun &rest args))
  "`execute-kbd-macro' should never be run for fake cursors.
The real cursor will execute the keyboard macro, resulting in new commands
in the command loop, and the fake cursors can pick up on those instead."
  (unless hel-executing-command-for-fake-cursor
    (apply orig-fun args)))

(hel-cache-input read-char)
(hel-cache-input read-quoted-char)
(hel-cache-input read-from-kill-ring)
(hel-cache-input read-char-from-minibuffer)
(hel-cache-input read-char-by-name) ; is used by `insert-char'
(hel-cache-input register-read-with-preview)  ; is used by `read-string'

;;; Commands that don't work with multiple-cursors

(hel-unsupported-command isearch-forward)
(hel-unsupported-command isearch-backward)

;; Between invocations, `cycle-spacing' stores internal data in the
;; `cycle-spacing--context' variable. The original position is stored
;; as a number rather than a marker, and invalidates when other cursors
;; modify the buffer content.
(hel-unsupported-command cycle-spacing)

;; Replace it with `just-one-space' while multiple-cursors are active.
(hel-keymap-set hel-multiple-cursors-mode-map
  "<remap> <cycle-spacing>" #'just-one-space)

;;; Update cursor color on theme change

(defun hel--update-main-cursor-color (color)
  (set-face-attribute 'hel-normal-state-main-cursor nil :background color)
  (hel-update-cursor))

(hel-advice-add 'set-cursor-color :after #'hel--update-main-cursor-color)

(defun hel--handle-theme-change (_theme)
  (hel--update-main-cursor-color (face-background 'cursor)))

(defun hel--handle-theme-change-h ()
  (if hel-mode
      (progn
        (add-hook 'enable-theme-functions  #'hel--handle-theme-change)
        (add-hook 'disable-theme-functions #'hel--handle-theme-change))
    (remove-hook 'enable-theme-functions  #'hel--handle-theme-change)
    (remove-hook 'disable-theme-functions #'hel--handle-theme-change)))

(add-hook 'hel-mode-hook #'hel--handle-theme-change-h)

;;; Advices for built-in commands

(dolist (cmd '(fill-region    ; gq
               indent-region  ; =
               comment-dwim)) ; gc
  (hel-advice-add cmd :around #'hel-keep-selection-a))

(hel-advice-add 'clone-indirect-buffer :before #'hel-deactivate-mark-a)

;;; Distinguish `TAB' from `C-i' and `RET' from `C-m'

(defun hel-make-C-i-and-C-m-available ()
  "Make Emacs distinguish `TAB' from `C-i' and `RET' from `C-m'."
  (when (display-graphic-p) ;; do translation only in gui
    (keymap-set input-decode-map "C-i" [C-i])
    (keymap-set input-decode-map "C-m" [C-m])))

(hel-make-C-i-and-C-m-available)

;; For daemon mode
(add-hook 'after-make-frame-functions
          (defun hel--after-make-frame-hook (frame)
            (with-selected-frame frame
              (hel-make-C-i-and-C-m-available))))

;; (single-key-description 'C-i)
;; (key-valid-p "<C-i>")
;; (key-valid-p "C-<i>")

;;; emacs-lisp-mode (elisp)

;; Fontification for Hel macros.
(font-lock-add-keywords
 'emacs-lisp-mode (eval-when-compile
                    `((,(concat "^\\s-*("
                                (regexp-opt '("hel-define-command") t)
                                "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
                       (1 'font-lock-keyword-face)
                       (2 'font-lock-function-name-face nil t))
                      (,(concat "^\\s-*("
                                (regexp-opt '("hel-defvar-local") t)
                                "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
                       (1 'font-lock-keyword-face)
                       (2 'font-lock-variable-name-face nil t)))))

;; `emacs-lisp-mode' is inherited from `lisp-data-mode'.
(add-hook 'lisp-data-mode-hook 'hel--emacs-lisp-mode-h)

(defun hel--emacs-lisp-mode-h ()
  ;; Add legacy quotes marks to Hel surround functionality.
  (let ((spec '(:insert ("`" . "'") :remove ("`" . "'"))))
    (setf (alist-get ?` hel-surround-alist) spec)
    (setf (alist-get ?' hel-surround-alist) spec))
  ;;
  ;; Teach `imenu' about Hel macros.
  (dolist (i (eval-when-compile
               `(("Variables"
                  ,(concat "^\\s-*("
                           (regexp-opt '("hel-defvar-local") t)
                           "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
                  2)
                 (nil ;; top level
                  ,(concat "^\\s-*("
                           (regexp-opt '("hel-define-command") t)
                           "\\s-+'?\\(" (rx lisp-mode-symbol) "\\)")
                  2))))
    (cl-pushnew i imenu-generic-expression :test #'equal)))

(dolist (keymap (list emacs-lisp-mode-map
                      lisp-data-mode-map))
  (hel-keymap-set keymap :state 'normal
    "m `"   #'hel-mark-inner-legacy-quoted
    "m '"   #'hel-mark-inner-legacy-quoted
    "m i `" #'hel-mark-inner-legacy-quoted
    "m i '" #'hel-mark-inner-legacy-quoted
    "m a `" #'hel-mark-a-legacy-quoted
    "m a '" #'hel-mark-a-legacy-quoted))

(hel-keymap-set emacs-lisp-compilation-mode-map
  "g"   nil
  ", r" #'emacs-lisp-compilation-recompile)

(hel-define-command hel-mark-inner-legacy-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (hel-surround-4-bounds-at-point "`" "'"))
    (hel-set-region beg end)))

(hel-define-command hel-mark-a-legacy-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg _ _ end) (hel-surround-4-bounds-at-point "`" "'"))
    (hel-set-region beg end)))

;;; Built-in packages
;;;; Button

(hel-advice-add 'forward-button  :before #'hel-deactivate-mark-a)
(hel-advice-add 'backward-button :before #'hel-deactivate-mark-a)

;;;; Backtrace and Debug

(with-eval-after-load 'backtrace
  (hel-keymap-set backtrace-mode-map :state 'motion
    "j"   'backtrace-forward-frame   ;; "n"
    "k"   'backtrace-backward-frame) ;; "p"
  ;; <local leader>
  (hel-keymap-set backtrace-mode-map
    ", v" 'backtrace-toggle-locals
    ", #" 'backtrace-toggle-print-circle
    ", :" 'backtrace-toggle-print-gensym
    ", +" 'backtrace-multi-line
    ", -" 'backtrace-single-line
    ", ." 'backtrace-expand-ellipses))

(with-eval-after-load 'debug
  ;; `debugger-mode-map' is inherited from `backtrace-mode-map'
  (hel-keymap-set debugger-mode-map
    "J"   'debugger-jump
    ;; <local leader>
    ", b" 'debugger-frame
    ", c" 'debugger-continue
    ", j" 'debugger-jump
    ", r" 'debugger-return-value
    ", u" 'debugger-frame-clear
    ", d" 'debugger-step-through
    ", l" 'debugger-list-functions
    ", e" 'debugger-eval-expression
    ", R" 'debugger-record-expression))

;;;; Edebug

(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook #'hel-update-active-keymaps)
  (hel-keymap-set edebug-mode-map
    "SPC"   nil ; unding `edebug-step-mode'
    "h"     nil ; unding `edebug-goto-here'
    "s"     #'edebug-step-mode
    "H"     #'edebug-goto-here
    "C-c h" #'edebug-goto-here) ; <leader> h
  (hel-keymap-set edebug-mode-map :state 'normal
    "<escape>" #'hel-normal-state-escape))

;;;; Eldoc

(with-eval-after-load 'eldoc
  ;; Add motion commands to the `eldoc-message-commands' obarray.
  (eldoc-add-command 'hel-backward-char        ; h
                     'hel-forward-char         ; l
                     'hel-next-line            ; j
                     'hel-previous-line        ; k
                     'hel-forward-word-start   ; w
                     'hel-forward-WORD-start   ; W
                     'hel-backward-word-start  ; b
                     'hel-backward-WORD-start  ; B
                     'hel-forward-word-end     ; e
                     'hel-forward-WORD-end     ; E
                     'hel-first-non-blank      ; gh
                     'hel-end-of-line-command  ; gl
                     'hel-beginning-of-buffer  ; gg
                     'hel-end-of-buffer        ; G
                     'hel-search-forward       ; /
                     'hel-search-backward      ; ?
                     'hel-search-next          ; n
                     'hel-search-previous      ; N
                     'hel-find-char-forward    ; f
                     'hel-find-char-backward   ; F
                     'hel-till-char-forward    ; t
                     'hel-till-char-backward)) ; T

;;;; Help

(with-eval-after-load 'help-mode
  (hel-set-initial-state 'help-mode 'normal))

(with-eval-after-load 'helpful
  (hel-set-initial-state 'helpful-mode 'normal)
  (put 'helpful-at-point 'multiple-cursors nil))

;;;; Comint

(with-eval-after-load 'comint
  (hel-keymap-set comint-mode-map :state 'normal
    "i" 'hel-comint-insert
    "a" 'hel-comint-append
    "I" 'hel-comint-insert-line
    "A" 'hel-comint-append-line
    "c" 'hel-comint-change))

(defun hel-comint--goto-process-mark ()
  "Move point to the beginning of writable input start."
  (-> (current-buffer)
      (get-buffer-process)
      (process-mark)
      (goto-char)))

;; i
(hel-define-command hel-comint-insert ()
  "Switch to Insert state, jumping to prompt if cursor is in read-only area."
  :multiple-cursors nil
  (interactive)
  (unless (comint-after-pmark-p)
    (hel-disable-multiple-cursors-mode)
    (deactivate-mark)
    (hel-comint--goto-process-mark))
  (hel-insert))

;; a
(hel-define-command hel-comint-append ()
  "Switch to Insert state after selection, jumping to prompt if in read-only."
  :multiple-cursors nil
  (interactive)
  (unless (comint-after-pmark-p)
    (hel-disable-multiple-cursors-mode)
    (deactivate-mark)
    (hel-comint--goto-process-mark))
  (hel-append))

;; I
(hel-define-command hel-comint-insert-line ()
  "Go to beginning of writable input and switch to Insert state."
  :multiple-cursors nil
  (interactive)
  (unless (comint-after-pmark-p)
    (hel-disable-multiple-cursors-mode)
    (deactivate-mark)
    (hel-comint--goto-process-mark))
  (hel-insert-line))

;; A
(hel-define-command hel-comint-append-line ()
  "Go to end of input line and switch to Insert state."
  :multiple-cursors nil
  (interactive)
  (unless (comint-after-pmark-p)
    (hel-disable-multiple-cursors-mode)
    (deactivate-mark)
    (hel-comint--goto-process-mark))
  (hel-append-line))

;; c
(hel-define-command hel-comint-change ()
  "Delete selection and switch to Insert state.
If cursor is in read-only area, jump to prompt instead of deleting."
  :multiple-cursors nil
  (interactive)
  (if (comint-after-pmark-p)
      (hel-change)
    (hel-disable-multiple-cursors-mode)
    (deactivate-mark)
    (hel-comint--goto-process-mark)
    (hel-insert-state 1)))

;;;; Compilation

(hel-advice-add 'next-error     :around #'hel-jump-command-a)
(hel-advice-add 'previous-error :around #'hel-jump-command-a)

(with-eval-after-load 'compile
  (dolist (keymap (list compilation-minor-mode-map
                        compilation-mode-map))
    (hel-keymap-set keymap
      "o"   #'compilation-display-error

      "g"   nil ; unbind `recompile'
      "g o" #'compile-goto-error
      "g r" #'recompile ; revert

      "n"   #'next-error-no-select
      "N"   #'previous-error-no-select
      "C-j" #'next-error-no-select
      "C-k" #'previous-error-no-select

      "}"   #'compilation-next-file
      "{"   #'compilation-previous-file
      "] p" #'compilation-next-file
      "[ p" #'compilation-previous-file
      "z j" #'compilation-next-file
      "z k" #'compilation-previous-file))

  (hel-keymap-set compilation-mode-map
    "g f" #'next-error-follow-minor-mode
    "Z Q" #'kill-compilation)

  (hel-advice-add 'compile-goto-error :around #'hel-jump-command-a))

;;;; grep-mode

(with-eval-after-load 'grep
  ;; `grep-mode-map' is inherited from `compilation-minor-mode-map'
  (hel-keymap-set grep-mode-map
    "i"   #'wgrep-change-to-wgrep-mode
    "g f" #'next-error-follow-minor-mode))

;;;;; wgrep

(with-eval-after-load 'wgrep
  (hel-advice-add 'wgrep-change-to-wgrep-mode :after #'hel-switch-to-initial-state)

  (hel-keymap-set wgrep-mode-map :state 'normal
    "<escape>" 'wgrep-exit
    "Z Z"      'wgrep-finish-edit
    "Z Q"      'wgrep-abort-changes)

  (hel-keymap-set wgrep-mode-map
    "<remap> <save-buffer>" 'wgrep-finish-edit)

  (hel-advice-add 'wgrep-to-original-mode :before #'hel-deactivate-mark-a)
  (hel-advice-add 'wgrep-to-original-mode :before #'hel-delete-all-fake-cursors)
  (hel-advice-add 'wgrep-to-original-mode :after  #'hel-switch-to-initial-state))

;;;; occur-mode

(with-eval-after-load 'replace
  (hel-keymap-set occur-mode-map
    "i"   #'occur-edit-mode
    "o"   #'occur-mode-display-occurrence           ; default `C-o'
    "g o" #'occur-mode-goto-occurrence-other-window ; default `o'
    "g f" #'next-error-follow-minor-mode

    "n"   #'next-error-no-select
    "N"   #'previous-error-no-select
    "C-j" #'next-error-no-select
    "C-k" #'previous-error-no-select)

  (hel-keymap-set occur-edit-mode-map :state 'normal
    "g o"      #'occur-mode-goto-occurrence-other-window
    "<escape>" #'occur-cease-edit
    "Z Z"      #'occur-cease-edit
    "Z Q"      #'occur-cease-edit)

  (hel-advice-add 'occur-mode-goto-occurrence    :around #'hel-jump-command-a)
  (hel-advice-add 'occur-mode-display-occurrence :around #'hel-jump-command-a))

;;;; dired
;;;;; wdired

(with-eval-after-load 'wdired
  (hel-advice-add 'wdired-change-to-wdired-mode :after #'hel-switch-to-initial-state)
  (hel-advice-add 'wdired-change-to-dired-mode  :after #'hel-switch-to-initial-state)

  (hel-keymap-set wdired-mode-map :state 'normal
    "<escape>" 'helheim-wdired-exit
    "Z Z"      'wdired-finish-edit
    "Z Q"      'wdired-abort-changes
    ;;
    "j"        'wdired-next-line
    "k"        'wdired-previous-line
    "<up>"     'wdired-next-line
    "<down>"   'wdired-previous-line
    ;; Commands bound to these keys have no sense for wdired.
    "o" 'undefined
    "O" 'undefined
    "J" 'undefined)

  (hel-keymap-set wdired-mode-map
    "<remap> <save-buffer>" #'wdired-finish-edit
    "C-g" 'wdired-abort-changes)

  (put 'wdired--self-insert  'multiple-cursors t)
  (put 'wdired-next-line     'multiple-cursors t)
  (put 'wdired-previous-line 'multiple-cursors t)
  (put 'wdired-finish-edit   'multiple-cursors nil)
  (put 'wdired-abort-changes 'multiple-cursors nil)
  (put 'wdired-exit          'multiple-cursors nil)

  (hel-advice-add 'wdired-change-to-dired-mode :before #'hel-deactivate-mark-a)
  (hel-advice-add 'wdired-change-to-dired-mode :before #'hel-delete-all-fake-cursors)

  (hel-advice-add 'wdired-next-line     :before #'hel-deactivate-mark-a)
  (hel-advice-add 'wdired-previous-line :before #'hel-deactivate-mark-a))

(hel-define-command helheim-wdired-exit ()
  :multiple-cursors t
  (interactive)
  (cond (hel--extend-selection
         (hel-extend-selection -1))
        ((use-region-p)
         (deactivate-mark))
        (t
         (wdired-exit))))

;;;; image-mode

(with-eval-after-load 'image-mode
  (hel-keymap-set image-mode-map :state 'motion
    "C-y" 'image-previous-line
    "C-e" 'image-next-line
    "C-b" 'image-scroll-right
    "C-f" 'image-scroll-left
    "C-d" 'image-scroll-up
    "C-u" 'image-scroll-down
    "z h" 'image-scroll-right
    "z l" 'image-scroll-left))

;;;; Messages buffer

(hel-set-initial-state 'messages-buffer-mode 'normal)

;;;; Minibuffer

(hel-keymap-set minibuffer-mode-map :state 'normal
  ;; "ESC" #'abort-minibuffers
  "<escape>" #'abort-recursive-edit
  "<down>"   #'next-line-or-history-element
  "<up>"     #'previous-line-or-history-element
  "j"        #'next-line-or-history-element
  "k"        #'previous-line-or-history-element)

(hel-keymap-set minibuffer-mode-map
  "C-j" #'next-history-element
  "C-k" #'previous-history-element
  "M-j" #'next-history-element
  "M-k" #'previous-history-element)

;; `C-j' in `read--expression-map' is bound to `read--expression-try-read'
;; which is also bound to `RET'. Remove it, to make the binding from the
;; parent `read-expression-map' keymap available.
(keymap-unset read--expression-map "C-j" :remove)

;;;; outline

;; For when we manually enable `outline-minor-mode' in an existing buffer.
(hel-advice-add 'outline-minor-mode :after #'hel-update-active-keymaps-a)

(hel-advice-add 'outline-insert-heading :after #'hel-switch-to-insert-state-a)

(dolist (cmd '(outline-up-heading
               outline-next-visible-heading
               outline-previous-visible-heading
               outline-forward-same-level
               outline-backward-same-level))
  (hel-advice-add cmd :before #'hel-maybe-deactivate-mark-a))

(dolist (cmd '(outline-promote
               outline-demote))
  (hel-advice-add cmd :around #'hel-keep-selection-a))

;;;; repeat-mode

(setopt repeat-exit-key "<escape>")

(put 'undo 'repeat-map nil) ; Do not repeat `undo'.

(hel-keymap-set buffer-navigation-repeat-map
  "]" #'next-buffer
  "[" #'previous-buffer)

;;;; shortdoc

(with-eval-after-load 'shortdoc
  (keymap-set shortdoc-mode-map "y" #'shortdoc-copy-function-as-kill))

;;;; special-mode

(hel-keymap-set special-mode-map
  "h"   #'left-char
  "j"   #'next-line
  "k"   #'previous-line
  "l"   #'right-char
  ;;
  "g"   nil ; unbind `revert-buffer', we have it on "C-w r"
  "g a" #'describe-char
  "g r" #'revert-buffer          ; also "C-w r"
  "g h" #'move-beginning-of-line ; native "C-a"
  "g l" #'move-end-of-line       ; native "C-e"
  "g g" #'beginning-of-buffer    ; native "<"
  "G"   #'end-of-buffer          ; native ">"
  ;;
  ;; Switch to Normal state. This allows you to select and copy arbitrary text
  ;; in special modes.
  ;;   Bind it in base keymap instead of Motion state keymap, to not override
  ;; the "i" key for major modes that inherit from special-mode.
  "i"   #'hel-normal-state)

(hel-keymap-set special-mode-map :state 'normal
  "<escape>" #'hel-motion-state
  ;; Use "zx" or "C-x C-s" to switch back to motion state.
  ;; Saving special buffer has little sense, so we can use it.
  "<remap> <save-buffer>" #'hel-motion-state)

;;;; prog-mode

(hel-keymap-set prog-mode-map :state 'normal
  "g q" #'prog-fill-reindent-defun
  ")"   #'hel-mark-function-forward
  "("   #'hel-mark-function-backward)

;;;; text-mode

(hel-keymap-set text-mode-map :state 'normal
  ")" #'hel-mark-sentence-forward
  "(" #'hel-mark-sentence-backward)

;;;; winner-mode & tab-bar-history-mode

(add-hook 'winner-mode-hook
          (defun hel-setup-winner-mode-keys ()
            (when winner-mode
              (hel-keymap-set hel-window-map
                "u" #'winner-undo
                "U" #'winner-redo))))

(add-hook 'tab-bar-history-mode-hook
          (defun hel-setup-tab-bar-history-mode-keys ()
            (when tab-bar-history-mode
              (hel-keymap-set hel-window-map
                "u" #'tab-bar-history-back
                "U" #'tab-bar-history-forward))))

;;;; VC

(with-eval-after-load 'bug-reference
  (hel-keymap-set bug-reference-map :state 'normal
    "RET" #'bug-reference-push-button))

(with-eval-after-load 'log-view
  (hel-keymap-set log-view-mode-map
    "j" #'log-view-msg-next
    "k" #'log-view-msg-prev))

;;;; widget

(defun hel-switch-to-normal-state-in-field-widget ()
  "This function intended to be called from mode hook of major modes that use
field widgets (like `Custom-mode' or `notmuch-hello-mode')."
  (add-hook 'post-command-hook #'hel--widget-field-h nil t))

(defun hel--widget-field-h ()
  (cond ((widget-field-at (point))
         (when (eq hel-state 'motion)
           (hel-normal-state)
           (hel-update-active-keymaps)))
        ((not (eq hel-state 'motion))
         (when hel-multiple-cursors-mode (hel-multiple-cursors-mode -1))
         (hel-motion-state))))

(with-eval-after-load 'wid-edit
  (hel-keymap-set widget-field-keymap :state 'normal
    "g h" 'hel-beginning-of-line-command
    "g l" 'hel-widget-end-of-line)
  ;;
  (widget-put (get 'editable-field 'widget-type) :keymap widget-field-keymap))

;; gl
(hel-define-command hel-widget-end-of-line ()
  "Move point to end of field or end of line, whichever is first."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn (widget-end-of-line) (point))))

;;;; Xref

(with-eval-after-load 'xref
  (dolist (cmd '(xref-find-definitions
                 xref-find-references
                 xref-go-back
                 xref-go-forward
                 xref-goto-xref
                 xref--show-xrefs
                 xref--show-defs))
    (hel-advice-add cmd :around #'hel-jump-command-a))

  (hel-keymap-set xref--xref-buffer-mode-map
    "o"   'xref-show-location-at-point
    "Q"   'xref-quit-and-pop-marker-stack

    "C-j" 'xref-next-line
    "C-k" 'xref-prev-line

    "}"   'xref-next-group
    "{"   'xref-prev-group
    "] p" 'xref-next-group
    "[ p" 'xref-prev-group
    "z j" 'xref-next-group
    "z k" 'xref-prev-group))

;;; External packages
;;;; corfu

(with-eval-after-load 'corfu
  ;; Close corfu popup on Insert state exit.
  (add-hook 'hel-insert-state-exit-hook 'corfu-quit))

;;;; consult

(with-eval-after-load 'consult
  (hel-cache-input consult--read)

  (put 'consult-yank-pop 'multiple-cursors t) ; Execute for all cursors.

  (dolist (cmd '(consult-line
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-outline
                 consult-grep
                 consult-git-grep
                 consult-ripgrep))
    (hel-advice-add cmd :before #'hel-deactivate-mark-a)))

;;;; diff-hl

(add-hook 'diff-hl-show-hunk-inline-transient-mode-hook
          (defun hel-switch-to-motion-state-while-diff-hl-show-hunk-h ()
            "Switch to motion state while VC diff hunk is shown."
            (if diff-hl-show-hunk-inline-transient-mode
                (hel-motion-state)
              (hel-normal-state))))

(with-eval-after-load 'diff-hl-show-hunk-inline
  (hel-keymap-set diff-hl-show-hunk-inline-transient-mode-map
    "<remap> <self-insert-command>" 'ignore))

;;; .
(provide 'hel-integration)
;;; hel-integration.el ends here
