;;; hel-keybindings.el --- Hel default keybindings -*- lexical-binding: t -*-
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

(require 'hel-core)
(require 'hel-commands)
(require 'hel-scrolling)

;;; Universal argument

(defun hel--setup-universal-argument-keys-h ()
  "Rebind `universal-argument' to \"M-u\" since \"C-u\" is used for scrolling.
By default \"M-u\" is bound to `upcase-word' for which we have \"gU\", so we can
use it."
  (if hel-mode
      (progn
        (keymap-global-set "M-u" #'universal-argument)
        (keymap-set universal-argument-map "M-u" #'universal-argument-more)
        ;; Unbind "C-u" so that \\[universal-argument] links in help buffers
        ;; are displayed as "M-u".
        (keymap-global-unset "C-u" t)
        (keymap-unset universal-argument-map "C-u" t))
    ;; else
    (keymap-global-set "C-u" #'universal-argument)
    (keymap-set universal-argument-map "C-u" #'universal-argument-more)
    (keymap-global-set "M-u" #'upcase-word)
    (keymap-unset universal-argument-map "M-u" t)))

(add-hook 'hel-mode-hook #'hel--setup-universal-argument-keys-h)

;;; Normal state

(keymap-global-unset "M-<down-mouse-1>")
(hel-keymap-global-set :state 'normal
  "M-<mouse-1>" #'hel-toggle-cursor-on-click)

(hel-keymap-global-set :state 'normal
  ":"   #'execute-extended-command
  "C-:" #'execute-extended-command-for-buffer

  ;; Arrows
  "<left>"  #'hel-backward-char
  "<down>"  #'hel-next-line
  "<up>"    #'hel-previous-line
  "<right>" #'hel-forward-char

  ;; Motions
  "h"   #'hel-backward-char
  "j"   #'hel-next-line
  "k"   #'hel-previous-line
  "l"   #'hel-forward-char
  "w"   #'hel-forward-word-start
  "W"   #'hel-forward-WORD-start
  "b"   #'hel-backward-word-start
  "B"   #'hel-backward-WORD-start
  "e"   #'hel-forward-word-end
  "E"   #'hel-forward-WORD-end
  "f"   #'hel-find-char-forward
  "F"   #'hel-find-char-backward
  "t"   #'hel-till-char-forward
  "T"   #'hel-till-char-backward
  "g s" #'hel-first-non-blank
  "g h" #'hel-beginning-of-line-command
  "g l" #'hel-end-of-line-command
  "g g" #'hel-beginning-of-buffer
  "g e" #'hel-end-of-buffer
  "G"   #'hel-end-of-buffer
  "}"   #'hel-forward-paragraph
  "{"   #'hel-backward-paragraph
  "] p" #'hel-forward-paragraph-end
  "[ p" #'hel-backward-paragraph-end
  "] f" #'hel-mark-function-forward
  "[ f" #'hel-mark-function-backward
  "] s" #'hel-mark-sentence-forward
  "[ s" #'hel-mark-sentence-backward
  "] ." #'hel-mark-sentence-forward
  "[ ." #'hel-mark-sentence-backward
  "] e" #'next-error
  "[ e" #'previous-error
  ;; Because page boudaries are displayed as ^L.
  "] l" #'forward-page
  "[ l" #'backward-page

  ;; Easymotion / Avy
  "g w" #'hel-avy-word-forward
  "g b" #'hel-avy-word-backward
  "g W" #'hel-avy-WORD-forward
  "g B" #'hel-avy-WORD-backward
  "g j" #'hel-avy-next-line
  "g k" #'hel-avy-previous-line

  ;; Changes
  "i"   #'hel-insert
  "a"   #'hel-append
  "I"   #'hel-insert-line
  "A"   #'hel-append-line
  "o"   #'hel-open-below
  "O"   #'hel-open-above
  "c"   #'hel-change
  "d"   #'hel-cut
  "D"   #'hel-delete
  "u"   #'hel-undo
  "U"   #'hel-redo
  "y"   #'hel-copy
  "p"   #'hel-paste-after
  "P"   #'hel-paste-before
  "R"   #'hel-replace-with-kill-ring
  "C-p" #'hel-paste-pop ;; yank-pop
  "C-n" #'hel-paste-undo-pop
  "J"   #'hel-join-line
  "`"   #'hel-downcase
  "M-`" #'hel-upcase
  "~"   #'hel-invert-case
  "g u" #'hel-downcase
  "g U" #'hel-upcase
  "="   #'indent-region
  "<"   #'hel-indent-left
  ">"   #'hel-indent-right

  ;; Selections
  "<escape>" #'hel-normal-state-escape
  "v"   #'hel-extend-selection
  "x"   #'hel-expand-line-selection
  "X"   #'hel-expand-line-selection-backward
  "%"   #'hel-mark-whole-buffer
  "C"   #'hel-copy-selection
  "M-c" #'hel-copy-selection-up
  "s"   #'hel-select-regex
  "S"   #'hel-split-region
  "M-s" #'hel-split-region-on-newline
  ";"   #'hel-collapse-selection
  "C-;" #'hel-exchange-point-and-mark
  "M-;" #'hel-exchange-point-and-mark
  "g ;" #'hel-exchange-point-and-mark
  "_"   #'hel-trim-whitespaces-from-selection
  "g v" #'hel-restore-cursors

  ;; Surround
  "m m" #'hel-jump-to-match-item
  "m s" #'hel-surround
  "m d" #'hel-surround-delete
  "m r" #'hel-surround-change

  ;; Search
  "/"   #'hel-search-forward
  "?"   #'hel-search-backward
  "*"   #'hel-construct-search-pattern
  "M-*" #'hel-construct-search-pattern-no-bounds
  "n"   #'hel-search-next
  "N"   #'hel-search-previous

  ;; Scrolling
  "C-b" #'hel-smooth-scroll-page-up
  "C-f" #'hel-smooth-scroll-page-down
  "C-d" #'hel-smooth-scroll-down
  "C-u" #'hel-smooth-scroll-up
  "C-e" #'hel-mix-scroll-line-down
  "C-y" #'hel-mix-scroll-line-up
  "z z" #'hel-smooth-scroll-line-to-eye-level
  "z t" #'hel-smooth-scroll-line-to-top
  "z b" #'hel-smooth-scroll-line-to-bottom

  ;; Misc
  "."     #'repeat
  "C-s"   #'hel-save-point-to-mark-ring
  "C-o"   #'hel-backward-mark-ring
  "C-<i>" #'hel-forward-mark-ring
  "C-S-o" #'hel-backward-global-mark-ring
  "C-S-i" #'hel-forward-global-mark-ring
  "g a"   #'describe-char
  "g c"   #'comment-dwim
  "g o"   #'imenu
  "g f"   #'find-file-at-point
  "g x"   #'browse-url-at-point
  "g q"   #'fill-paragraph
  "g Q"   #'fill-region-as-paragraph
  "] b"   #'next-buffer
  "[ b"   #'previous-buffer
  "] SPC" #'hel-add-blank-line-below
  "[ SPC" #'hel-add-blank-line-above

  "z x"   #'save-buffer
  "Z Z"   #'save-buffer

  ;; Narrow to region
  "z n" #'hel-narrow-to-region-indirectly
  "z w" #'hel-widen-indirectly-narrowed

  ;; Xref
  "g d" #'xref-find-definitions
  "g r" #'xref-find-references
  "[ x" #'xref-go-back
  "] x" #'xref-go-forward)

;;;; Keys active while there are multiple cursors

(hel-keymap-set hel-multiple-cursors-mode-map :state 'normal
  "K"   #'hel-keep-selections
  "M-K" #'hel-remove-selections
  ","   #'hel-remove-all-fake-cursors
  "M-," #'hel-remove-main-cursor
  "g g" #'hel-first-selection
  "g e" #'hel-last-selection
  "G"   #'hel-last-selection
  "("   #'hel-rotate-selections-backward
  ")"   #'hel-rotate-selections-forward
  "M-(" #'hel-rotate-selections-content-backward
  "M-)" #'hel-rotate-selections-content-forward
  "M--" #'hel-merge-selections
  "&"   #'hel-align-selections)

;;;; Mark commands

(hel-keymap-global-set :state 'normal
  "m w"   #'hel-mark-inner-word
  "m i w" #'hel-mark-inner-word
  "m a w" #'hel-mark-a-word
  "m W"   #'hel-mark-inner-WORD
  "m i W" #'hel-mark-inner-WORD
  "m a W" #'hel-mark-a-WORD
  ;; sentence
  "m ."   #'hel-mark-inner-sentence
  "m i ." #'hel-mark-inner-sentence
  "m a ." #'hel-mark-a-sentence
  "m i s" #'hel-mark-inner-sentence
  "m a s" #'hel-mark-a-sentence
  ;; function
  "m f"   #'hel-mark-inner-function
  "m i f" #'hel-mark-inner-function
  "m a f" #'hel-mark-a-function
  ;; paragraph
  "m p"   #'hel-mark-inner-paragraph
  "m i p" #'hel-mark-inner-paragraph
  "m a p" #'hel-mark-a-paragraph

  "m \""   #'hel-mark-inner-double-quoted
  "m i \"" #'hel-mark-inner-double-quoted
  "m a \"" #'hel-mark-a-double-quoted
  "m '"    #'hel-mark-inner-single-quoted
  "m i '"  #'hel-mark-inner-single-quoted
  "m a '"  #'hel-mark-a-single-quoted
  "m `"    #'hel-mark-inner-back-quoted
  "m i `"  #'hel-mark-inner-back-quoted
  "m a `"  #'hel-mark-a-back-quoted

  "m ("   #'hel-mark-inner-paren
  "m )"   #'hel-mark-inner-paren
  "m i (" #'hel-mark-inner-paren
  "m i )" #'hel-mark-inner-paren
  "m a (" #'hel-mark-a-paren
  "m a )" #'hel-mark-a-paren

  "m ["   #'hel-mark-inner-bracket
  "m ]"   #'hel-mark-inner-bracket
  "m i [" #'hel-mark-inner-bracket
  "m i ]" #'hel-mark-inner-bracket
  "m a [" #'hel-mark-a-bracket
  "m a ]" #'hel-mark-a-bracket

  "m {"   #'hel-mark-inner-curly
  "m }"   #'hel-mark-inner-curly
  "m i {" #'hel-mark-inner-curly
  "m i }" #'hel-mark-inner-curly
  "m a {" #'hel-mark-a-curly
  "m a }" #'hel-mark-a-curly

  "m <"   #'hel-mark-inner-angle
  "m >"   #'hel-mark-inner-angle
  "m i <" #'hel-mark-inner-angle
  "m i >" #'hel-mark-inner-angle
  "m a <" #'hel-mark-an-angle
  "m a >" #'hel-mark-an-angle

  "m !"   #'hel-mark-inner-surround
  "m @"   #'hel-mark-inner-surround
  "m #"   #'hel-mark-inner-surround
  "m $"   #'hel-mark-inner-surround
  "m %"   #'hel-mark-inner-surround
  "m ^"   #'hel-mark-inner-surround
  "m &"   #'hel-mark-inner-surround
  "m *"   #'hel-mark-inner-surround
  "m ~"   #'hel-mark-inner-surround
  "m ="   #'hel-mark-inner-surround
  "m _"   #'hel-mark-inner-surround

  "m i !" #'hel-mark-inner-surround
  "m i @" #'hel-mark-inner-surround
  "m i #" #'hel-mark-inner-surround
  "m i $" #'hel-mark-inner-surround
  "m i %" #'hel-mark-inner-surround
  "m i ^" #'hel-mark-inner-surround
  "m i &" #'hel-mark-inner-surround
  "m i *" #'hel-mark-inner-surround
  "m i ~" #'hel-mark-inner-surround
  "m i =" #'hel-mark-inner-surround
  "m i _" #'hel-mark-inner-surround

  "m a !" #'hel-mark-a-surround
  "m a @" #'hel-mark-a-surround
  "m a #" #'hel-mark-a-surround
  "m a $" #'hel-mark-a-surround
  "m a %" #'hel-mark-a-surround
  "m a ^" #'hel-mark-a-surround
  "m a &" #'hel-mark-a-surround
  "m a *" #'hel-mark-a-surround
  "m a ~" #'hel-mark-a-surround
  "m a =" #'hel-mark-a-surround
  "m a _" #'hel-mark-a-surround

  "m -"   #'hel-m-negative-argument
  "m 0"   #'hel-m-digit-argument
  "m 1"   #'hel-m-digit-argument
  "m 2"   #'hel-m-digit-argument
  "m 3"   #'hel-m-digit-argument
  "m 4"   #'hel-m-digit-argument
  "m 5"   #'hel-m-digit-argument
  "m 6"   #'hel-m-digit-argument
  "m 7"   #'hel-m-digit-argument
  "m 8"   #'hel-m-digit-argument
  "m 9"   #'hel-m-digit-argument

  "m i -" #'hel-mi-negative-argument
  "m i 0" #'hel-mi-digit-argument
  "m i 1" #'hel-mi-digit-argument
  "m i 2" #'hel-mi-digit-argument
  "m i 3" #'hel-mi-digit-argument
  "m i 4" #'hel-mi-digit-argument
  "m i 5" #'hel-mi-digit-argument
  "m i 6" #'hel-mi-digit-argument
  "m i 7" #'hel-mi-digit-argument
  "m i 8" #'hel-mi-digit-argument
  "m i 9" #'hel-mi-digit-argument

  "m a -" #'hel-ma-negative-argument
  "m a 0" #'hel-ma-digit-argument
  "m a 1" #'hel-ma-digit-argument
  "m a 2" #'hel-ma-digit-argument
  "m a 3" #'hel-ma-digit-argument
  "m a 4" #'hel-ma-digit-argument
  "m a 5" #'hel-ma-digit-argument
  "m a 6" #'hel-ma-digit-argument
  "m a 7" #'hel-ma-digit-argument
  "m a 8" #'hel-ma-digit-argument
  "m a 9" #'hel-ma-digit-argument)

;;; Motion state

(hel-keymap-global-set :state 'motion
  ":"   #'execute-extended-command
  "] b" #'next-buffer
  "[ b" #'previous-buffer
  ;; Scrolling
  "C-b" #'hel-smooth-scroll-page-up
  "C-f" #'hel-smooth-scroll-page-down
  "C-d" #'hel-smooth-scroll-down
  "C-u" #'hel-smooth-scroll-up
  "C-e" #'hel-mix-scroll-line-down
  "C-y" #'hel-mix-scroll-line-up
  "z z" '("scroll to eye level" . hel-smooth-scroll-line-to-eye-level)
  "z t" '("scroll to top" . hel-smooth-scroll-line-to-top)
  "z b" '("scroll to bottom" . hel-smooth-scroll-line-to-bottom))

;;; C-w keys

(hel-keymap-global-set :state '(normal motion)
  "C-w" 'hel-window-map)

(hel-keymap-global-set
  "C-c w" 'hel-window-map)

(hel-keymap-set hel-window-map
  ;; windows
  "RET" '("open next command in same window" . same-window-prefix)
  "n"   '("open next command in other window" . other-window-prefix)
  "s"   '("split window horizontally" . hel-window-split)
  "v"   '("split window vertically" . hel-window-vsplit)
  "S"   '("split root window horizontally" . hel-root-window-split)
  "V"   '("split root window vertically" . hel-root-window-vsplit)
  "c"   '("close window" . hel-window-delete)
  "o"   '("close all other windows" . delete-other-windows)
  "p"   '("pin buffer to window" . toggle-window-dedicated)

  "w"   '("goto other window" . other-window)
  "h"   '("goto window left" . windmove-left)
  "j"   '("goto window down" . windmove-down)
  "k"   '("goto window up" . windmove-up)
  "l"   '("goto window right" . windmove-right)

  "H"   '("move window left" . hel-move-window-left)
  "J"   '("move window down" . hel-move-window-down)
  "K"   '("move window up" . hel-move-window-up)
  "L"   '("move window right" . hel-move-window-right)

  ;; buffers
  "r"   #'revert-buffer
  "d"   #'kill-current-buffer
  "q"   '("kill buffer and window" . hel-kill-current-buffer-and-window)
  "b"   '("indirect buffer in other window" . clone-indirect-buffer-other-window)
  "B"   '("indirect buffer in this window" . hel-clone-indirect-buffer-same-window)
  "z"   #'bury-buffer ; mnemonics: "z" is the last letter
  "x"   #'scratch-buffer
  ;; xref
  "g d" #'("find definitions in other window" . xref-find-definitions-other-window)

  ":"   '("execute in other window" . hel-execute-extended-command-other-window)
  "C-:" '("execute for buffer in other window" . hel-execute-extended-command-for-buffer-other-window)
  "M-x" '("execute in other window" . hel-execute-extended-command-other-window)
  "M-X" '("execute for buffer in other window" . hel-execute-extended-command-for-buffer-other-window)

  ;; Duplicate all keys with ctrl prefix.
  "C-n" #'other-window-prefix
  "C-s" #'hel-window-split
  "C-v" #'hel-window-vsplit
  "C-S" #'hel-root-window-split
  "C-V" #'hel-root-window-vsplit
  "C-c" #'hel-window-delete
  "C-o" #'delete-other-windows
  "C-p" #'toggle-window-dedicated
  ;; Jump over windows
  "C-w" #'other-window
  "C-h" #'windmove-left
  "C-j" #'windmove-down
  "C-k" #'windmove-up
  "C-l" #'windmove-right
  ;; buffers
  "C-r" #'revert-buffer
  "C-d" #'kill-current-buffer
  "C-q" #'hel-kill-current-buffer-and-window
  "C-b" #'clone-indirect-buffer-other-window
  "C-x" #'scratch-buffer
  "C-z" #'bury-buffer)

;;; Insert state

(hel-keymap-global-set :state 'insert
  "<escape>" #'hel-normal-state)

;;; Conditional keybindings

(when hel-want-zz-scroll-to-center
  (hel-keymap-global-set :state '(normal motion)
    "z z" #'hel-smooth-scroll-line-to-center))

;;; .
(provide 'hel-keybindings)
;;; hel-keybindings.el ends here
