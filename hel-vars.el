;;; hel-vars.el --- Settings and variables -*- lexical-binding: t -*-
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Customization groups, faces and variables shared by all Hel modules.
;;
;; This file is loaded first and should not depend on any other Hel module.
;;
;;; Code:

(require 'map)
(require 'dash)

(defgroup hel nil
  "Hel emulation."
  :group 'emulations
  :prefix 'hel-)

(defmacro hel-defvar-permanent-local (symbol &optional initvalue docstring)
  "The same as `defvar-local' but additionaly marks SYMBOL as permanent
buffer local variable."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

;;; Appearence

(defcustom hel-match-fake-cursor-style t
  "If non-nil, attempt to match the `cursor-type' that the user has selected.
We only can match `bar' and `box' types.
If nil, the `box' cursor type will be used for all fake cursors."
  :type 'boolean
  :group 'hel)

(defcustom hel-bar-fake-cursor ?\u2000
  "Character used as the fake cursor when `cursor-type' is `bar'
and `hel-match-fake-cursor-style' is non-nil.

The value must be either a character or a single-character string,
representing a narrow space. Recommended candidates are:
- ?\\u2000 EN QUAD (default value)
- ?\\u2002 EN SPACE
- ?\\u2009 THIN SPACE

The appearance depends heavily on:
1. Your main font.
2. The font Emacs selects for this narrow space — it should be
   a variable-pitch font. Use `describe-char' to verify the chosen font.

Note: This all is a hack since Emacs can't render two characters in one
cell. The bar fake cursor character is virtually inserted between cells,
shifting subsequent content to the right."
  :type '(set character string)
  :group 'hel
  :set (lambda (symbol value)
         (set-default symbol (cond ((characterp value)
                                    (char-to-string value))
                                   ((and (stringp value) (length= value 1))
                                    value)
                                   (t
                                    (char-to-string ?\u2000))))))

(let ((type '(choice (const :tag "Frame default" t) (const :tag "Filled box" box)
                     (cons :tag "Box with specified size" (const box) integer)
                     (const :tag "Hollow cursor" hollow) (const :tag "Vertical bar" bar)
                     (cons :tag "Vertical bar with specified height" (const bar) integer)
                     (const :tag "Horizontal bar" hbar)
                     (cons :tag "Horizontal bar with specified width" (const hbar) integer)
                     (const :tag "None " nil))))
  (defcustom hel-normal-state-cursor-type 'bar
    "`cursor-type' used when Hel is in Normal state."
    :type type
    :group 'hel
    :set (lambda (symbol value)
           (set-default symbol value)
           (when (bound-and-true-p hel-state-properties)
             (setcar (-> hel-state-properties
                         (map-elt 'normal)
                         (map-elt :cursor))
                     value))))
  (defcustom hel-insert-state-cursor-type 'box
    "`cursor-type' used when Hel is in Insert state."
    :type type
    :group 'hel
    :set (lambda (symbol value)
           (set-default symbol value)
           (when (bound-and-true-p hel-state-properties)
             (setcar (-> hel-state-properties
                         (map-elt 'insert)
                         (map-elt :cursor))
                     value))))
  (defcustom hel-emacs-state-cursor-type '(hbar . 4)
    "`cursor-type' used when Hel is in Emacs state."
    :type type
    :group 'hel
    :set (lambda (symbol value)
           (set-default symbol value)
           (when (bound-and-true-p hel-state-properties)
             (setcar (-> hel-state-properties
                         (map-elt 'emacs)
                         (map-elt :cursor))
                     value)))))

(defface hel-normal-state-main-cursor
  `((t :background ,(face-background 'cursor)))
  "The `:background' attribute of this face defines the color of the main cursor
when Hel is in  Normal state. All other attributes are ignored."
  :group 'hel)

(defface hel-insert-state-main-cursor
  '((t :inherit hel-normal-state-main-cursor))
  "The `:background' attribute of this face defines the color of the main cursor
when Hel is in Insert state. All other attributes are ignored."
  :group 'hel)

(defface hel-emacs-state-main-cursor
  '((t :inherit hel-normal-state-main-cursor))
  "The `:background' attribute of this face defines the color of the cursor when
Hel is in Emacs state. All other attributes are ignored."
  :group 'hel)

(defface hel-extend-selection-cursor
  '((t :background "orange"))
  "Face that defines the color of all cursors when when extending selection
(\"v\" key) is active."
  :group 'hel)

(defface hel-normal-state-fake-cursor
  '((t :foreground "black"
       :background "#ea0064"))
  "Face for fake cursors when Hel is in Normal state."
  :group 'hel)

(defface hel-insert-state-fake-cursor
  '((t :foreground "white"
       :background "SkyBlue3"))
  "Face for fake cursors when Hel is in Insert state."
  :group 'hel)

(defface hel-search-highlight '((t :inherit lazy-highlight))
  "Face for lazy highlighting all matches during search."
  :group 'hel)

(defface hel-normal-state-tag '((t :inherit mode-line-emphasis))
  "Face for the Normal state tag in the mode line."
  :group 'hel)

(defface hel-insert-state-tag '((t :inherit warning)) ; success
  "Face for the Insert state tag in the mode line."
  :group 'hel)

;;; Customizable variables

(defvar hel-mode nil)
(declare-function hel-local-mode "hel-core" (&optional arg))

(defcustom hel-want-minibuffer t
  "Whether to enable Hel in minibuffer(s)."
  :type 'boolean
  :group 'hel
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and hel-mode value)
             (add-hook 'minibuffer-setup-hook #'hel-local-mode)
           (remove-hook 'minibuffer-setup-hook #'hel-local-mode))))

(defcustom hel-want-gui-esc-translation t
  "Whether to translate a lone GUI `\\e' input to `escape'.
When non-nil, `C-[' behaves as the Escape key if no further input
arrives within `hel-esc-delay' seconds."
  :type 'boolean
  :group 'hel)

(defcustom hel-esc-delay 0.01
  "Seconds to wait for another key after an Esc keypress.
If no further event arrives within this time, the lone `\\e' is
translated to the `escape' event so Hel's `<escape>' bindings fire.
Otherwise it is left as the standard ESC prefix (e.g. for `M-x')."
  :type 'number
  :group 'hel)

(defvar hel-inhibit-esc nil
  "If non-nil, never translate a lone `\\e' to `escape'.")

(defcustom hel-use-pcre-regex t
  "If non-nil use PCRE regexp syntax instead of Emacs regular expressions."
  :type 'boolean
  :group 'hel)

(defcustom hel-mode-line-info
  '(:propertize
    ((hel-multiple-cursors-mode
      (:eval (format " %s cursors " (hel-number-of-cursors))))
    (hel-search--current
      (:eval (format " %s/" hel-search--current))
      " ")
    (hel-search--total
      (:eval (format "%s " hel-search--total))))
    face mode-line-emphasis)
  "Alist of (VARIABLE MODE-LINE-CONSTRUCT...) entries controlling what
Hel shows in the mode line. Each VARIABLE is evaluated; while it is
non-nil, its MODE-LINE-CONSTRUCTs are shown, in the same format as
`mode-line-format'."
  :type 'sexp
  :group 'hel)
(put 'hel-mode-line-info 'risky-local-variable t)

(defcustom hel-regex-history-max 16
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'hel)

(defcustom hel-want-fine-undo t
  "Use more granular undo steps compare to Helix.

When non-nil on typing in Insert state a single undo step finishes at
word boundary or when the kind of editing changes between typing and
deleting. Thus, a single undo typically removes one word.

When nil, all text typed between entering and leaving Insert state is
undone as a single step, as in Helix."
  :type 'boolean
  :group 'hel)

(defcustom hel-want-zz-scroll-to-center nil
  "If non-nil `zz` keybinding will scroll current line to center of the screen.
This variable must be set before Hel is loaded!"
  :type 'boolean
  :group 'hel)

(defcustom hel-scroll-page-duration 0.35
  "Duration (in seconds) of a full-page smooth scroll (\\`C-f' / \\`C-b')."
  :type 'float
  :group 'hel)

(defcustom hel-scroll-half-page-duration 0.28
  "Duration (in seconds) of a half-page / count smooth scroll (\\`C-d' / \\`C-u')."
  :type 'float
  :group 'hel)

(defcustom hel-scroll-line-duration 0.10
  "Duration (in seconds) of a multi-line smooth scroll (\\`C-e' / \\`C-y')."
  :type 'float
  :group 'hel)

(defcustom hel-scroll-easing 'quadratic
  "Velocity curve of the smooth scroll animation.
`linear' scrolls at a constant speed. The others are ease-out curves —
the view starts fast and slows to a gentle stop — of increasing
sharpness: `quadratic', `cubic', `quartic', `sine'. A scroll key tapped
mid-animation restarts the curve from the current position, so repeated
taps read as accelerating pulses."
  :type '(choice (const linear)
                 (const quadratic)
                 (const cubic)
                 (const quartic)
                 (const sine))
  :group 'hel)

(defvar hel-scroll--frame-time 0
  "The delay (in seconds) between scroll animation frames.")

(defcustom hel-scroll-frame-rate 30
  "Number of frames per second of smooth-scroll animation.
Instead of firing `redisplay' as fast as the CPU allows, because
emitting frames faster than the compositor can present them has
no sense."
  :type 'number
  :group 'hel
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq hel-scroll--frame-time (/ 1.0 value))))

(defcustom hel-scroll-hide-cursor t
  "If non-nil, hide the cursor while a scroll drags it along the window edge.
The cursor is restored as soon as the scroll command finishes."
  :type 'boolean
  :group 'hel)

(defcustom hel-scroll-preserve-column t
  "If non-nil, restore the cursor column after a scroll that moved point."
  :type 'boolean
  :group 'hel)

(defcustom hel-want-C-hjkl-keys t
  "If non-nil, bind `C-h', `C-j', `C-k', `C-l' to commands for crawling the AST.
To access help commands, use `F1' instead of `C-h'.
AST stands for Abstract Syntax Tree.

These commands are also bound to `M-i', `M-n', `M-p', `M-o' for compatibility
with the Hel text editor.

This variable must be set before Hel is loaded!"
  :type 'boolean
  :group 'hel)

(defcustom hel-reactivate-selection-after-insert-state t
  "When non-nil, the selection will be reactivated on exiting Insert state if it
was active on entering it."
  :type 'boolean
  :group 'hel)

(defcustom hel-whitelist-file (locate-user-emacs-file "hel-multiple-cursors")
  "File to save users preferences which commands to execute for one cursor
and which for all."
  :type 'file
  :group 'hel)

(defcustom hel-max-cursors-number nil
  "Safety ceiling for the number of active cursors.
If your Emacs slows down or freezes when using too many cursors,
customize this value appropriately.

Cursors will be added until this value is reached, at which point
you can either temporarily override the value or abort the
operation entirely.

If this value is nil, there is no ceiling."
  :type '(integer)
  :group 'hel)

(defvar hel-minor-modes-incompatible-with-multiple-cursors
  '(corfu-mode
    company-mode
    flyspell-mode
    prettify-symbols-mode)
  "List of minor-modes that will be temporarily disabled while there are more
then one cursor in the buffer.")

;; TODO: add examples with `elisp-demos' package
(defvar-local hel-surround-alist
  '((?\) :insert ("(" . ")")
         :remove ("(" . ")") :balanced t)
    (?\} :insert ("{" . "}")
         :remove ("{" . "}") :balanced t)
    (?\] :insert ("[" . "]")
         :remove ("[" . "]") :balanced t)
    (?\> :insert ("<" . ">")
         :remove ("<" . ">") :balanced t)
    (?\( :insert (lambda ()
                   (if (hel-linewise-selection-p)
                       '("(\n" . ")\n")
                     '("( " . " )")))
         :remove (lambda ()
                   (hel-4-bounds-of-brackets-at-point ?\( ?\)))
         ;; or
         ;; :remove ("([[:blank:]\n]*" . "[[:blank:]\n]*)")
         ;; :regexp t
         ;; :balanced t
         )
    (?\[ :insert (lambda ()
                   (if (hel-linewise-selection-p)
                       '("[\n" . "]\n")
                     '("[ " . " ]")))
         :remove (lambda ()
                   (hel-4-bounds-of-brackets-at-point ?\[ ?\])))
    (?\{ :insert (lambda ()
                   (if (hel-linewise-selection-p)
                       '("{\n" . "}\n")
                     '("{ " . " }")))
         :remove (lambda ()
                   (hel-4-bounds-of-brackets-at-point ?{ ?})))
    (?\< :insert (lambda ()
                   (if (hel-linewise-selection-p)
                       '("<\n" . ">\n")
                     '("< " . " >")))
         :remove (lambda ()
                   (hel-4-bounds-of-brackets-at-point ?< ?>)))
    (?\" :insert ("\"" . "\"")
         :remove (lambda ()
                   (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?\"))
                     (list beg (1+ beg) (1- end) end))))
    (?t :insert hel-surround-read-tag
        :remove (lambda ()
                  (-> (read-string "Delete tag: ")
                      (string-trim)
                      (hel-surround-4-bounds-of-html-tag-at-point)))))
  "Alist of (KEY . SPEC) elements for Hel surround functionality.

This variable is buffer-local so that users can modify it from major-mode hooks.

KEY is a character, SPEC is a plist with ideologically 2 group of keys:

1. What \"ms\" (`hel-surround') and \"mr\" (`hel-surround-change') will insert.

   `:insert'    Cons cell (LEFT . RIGHT) of strings, or a function that returns
              such cons cell.

2. What \"md\" (`hel-surround-delete') and \"mr\" (`hel-surround-change')
   will remove.

   Either a group of keys:

   `:remove'    Cons cell (LEFT . RIGHT) of strings, or a function that returns
              such a cons cell. LEFT and RIGHT should be patterns used to search
              for the two substrings to delete.

   `:regexp'    If non-nil, treat LEFT and RIGHT from `:remove' as regular
              expressions. Otherwise, search for them literally.

   `:balanced'  When non-nil, skip all nested balanced LEFT/RIGHT pairs.
              Otherwise, accept the first matching pair found.

   Or a single key

   `:remove'    Function that returns list with 4 positions:

                       (LEFT-START LEFT-END RIGHT-START RIGHT-END)

              of START and END of left and right delimeters. Example:

                       |<tag> |Lorem ipsum dolor sit amet| </tag>|
                       ^      ^                          ^       ^
              LEFT-START      LEFT-END         RIGHT-START       RIGHT-END

See the default value for examples.")

(defcustom hel-search-initial-delay 0.25
  "Seconds to wait before beginning to lazily highlight all matches.
This setting only has effect when the search string is shorter than
`hel-search-no-delay-length' characters."
  :type 'number
  :group 'hel)

(defcustom hel-search-no-delay-length 3
  "For search strings at least this long, lazy highlight starts immediately.
For shorter search strings, `hel-search-initial-delay' applies."
  :type 'integer
  :group 'hel)

(defcustom hel-lazy-highlight-interval 0 ; 0.0625
  "Seconds between successive lazily highlighting rounds."
  :type 'number
  :group 'hel)

(defcustom hel-search-max-at-a-time 200 ; 20 (bug#48581)
  "Maximum matches to highlight at a time in buffer scanning phase.
A value of nil means highlight all matches in the buffer in one run."
  :type '(choice (const :tag "All" nil)
                 (integer :tag "Some"))
  :group 'hel)

(defgroup hel-cjk nil
  "CJK support."
  :prefix "hel-cjk-"
  :group 'hel)

;; (defcustom hel-cjk-emacs-word-boundary nil
;;   "Determine word boundary exactly the same way as Emacs does."
;;   :type 'boolean
;;   :group 'hel-cjk)

(defcustom hel-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `hel-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'hel-cjk)

(defcustom hel-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A))
  "List of pair (cons) of categories to determine word boundary
used in `hel-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'hel-cjk)

;;; Variables

(hel-defvar-permanent-local hel-mode-map-alist nil
  "Association list of keymaps for current Hel state.
This symbol lies in `emulation-mode-map-alists', so Emacs reads it on every
key sequence.  Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.  An element applies only while the value of MODE is
non-nil; MODE is t for a keymap that has no such variable.")

(hel-defvar-permanent-local hel--keymap-fingerprint nil
  "Vector holding the inputs `hel-mode-map-alist' was last built from.")

(hel-defvar-permanent-local hel--keymap-fingerprint-length 0
  "Number of significant slots in `hel--keymap-fingerprint'.")

(hel-defvar-permanent-local hel-state nil
  "The current Hel state.")

(hel-defvar-permanent-local hel-previous-state nil
  "The previous Hel state.")

(defvar hel-state-properties nil
  "Specifications made by `hel-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `hel-state-property' function.

`:keymap'        Symbol `hel-STATE-state-map' with keymap in its variable cell
               that will be active while Hel is in STATE.

`:cursor'        Cursor apperance when Hel is in STATE.
               Can be a cursor type as per `cursor-type', a color string
               as passed to `set-cursor-color', a zero-argument function for
               changing the cursor, a list of all the above, or a symbol
               with such list in its variable cell.

`:input-method'  When non-nil Hell will activate the enabled input method
               in STATE.

`:modes'         List of major and minor modes for which Hel intial state
               is STATE.

`:tag'           Mode-line tag for STATE.  A string, or a zero-argument
               function returning a string.")

(hel-defvar-permanent-local hel-input-method nil
  "Input method used in Hel Insert state.")

(hel-defvar-permanent-local hel-overriding-local-map nil)

(hel-defvar-permanent-local hel--extend-selection nil
  "When this flag is set motions will extend selection.")

(hel-defvar-permanent-local hel-selection-history nil
  "The history of selections.")

(hel-defvar-permanent-local hel--region-was-active-on-insert nil
  "Whether region was active when we last time switched to Insert state.")

(hel-defvar-permanent-local hel-scroll-count 0
  "Hold last used prefix for `hel-scroll-up' and `hel-scroll-down' commands.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(defvar hel--advices nil
  "Inner variable for `hel-define-advice'.")

(defvar hel-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")
(defalias 'hel-window-map hel-window-map)

(defvar hel-regex-history nil
  "List with used regexp patterns.")

(with-eval-after-load 'savehist
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'hel-regex-history))

(hel-defvar-permanent-local hel--cursors-table nil
  "Hash table mapping fake cursors IDs to cursors overlays.")

(defvar hel--fake-cursor-last-used-id 0
  "Last used fake cursor ID.")

(defvar hel-commands-to-run-for-all-cursors nil
  "Commands to execute for all cursors.")

(defvar hel-commands-to-run-once nil
  "Commands to execute only once while multiple cursors are active.")

(defvar hel-fake-cursor-variables
  '(transient-mark-mode ; for `region-active-p'
    mark-active
    mark-ring
    kill-ring
    kill-ring-yank-pointer
    yank-undo-function
    temporary-goal-column
    hel--extend-selection
    ;; Dabbrev
    dabbrev--abbrev-char-regexp
    dabbrev--check-other-buffers
    dabbrev--friend-buffer-list
    dabbrev--last-abbrev-location
    dabbrev--last-abbreviation
    dabbrev--last-buffer
    dabbrev--last-buffer-found
    dabbrev--last-direction
    dabbrev--last-expansion
    dabbrev--last-expansion-location
    dabbrev--last-table)
  "A list of variables that are tracked on a per-cursor basis.")

(defvar hel--whitelist-file-loaded nil
  "Non-nil when `hel-whitelist-file' file has already been loaded.")

(defvar-local hel-this-command nil
  "Like `this-command' but for fake cursors.
The command that that will be executed for each fake cursor.")

(defvar hel-executing-command-for-fake-cursor nil
  "Non-nil if `this-command' is currently executing for fake cursor.")

(hel-defvar-permanent-local hel--temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes while there are
multiple cursors.")

(hel-defvar-permanent-local hel--buffer-undo-list-pointer nil
  "Stores the start of the current undo step in `buffer-undo-list'.")

(hel-defvar-permanent-local hel-undo--cursors-positions nil
  "Positions of all cursors at the beginning of current undo step.")

(hel-defvar-permanent-local hel-undo--previous-command-kind 'other
  "Kind of the previous editing command, as `hel-undo--command-kind' returns it.")

(hel-defvar-permanent-local hel--cursors-positions-history nil
  "For `hel-restore-cursors'.")

(defvar-local hel--input-cache nil)

(hel-defvar-permanent-local hel-search--direction nil)
(hel-defvar-permanent-local hel-search--session nil)
(hel-defvar-permanent-local hel-search--current nil)
(hel-defvar-permanent-local hel-search--total nil)

(hel-defvar-permanent-local hel--narrowed-base-buffer nil)

;;; .
(provide 'hel-vars)
;;; hel-vars.el ends here
