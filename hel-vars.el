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

(defcustom hel-cursor-type 'helix
  "How the main cursor looks in Normal state.
Normal state draws every cursor as a box over its character. The value
is one of:

- `helix'   Cursor match the one in Helix.

- `hybrid'  The cursor of Emacs is a bar at point with the color of the
          main cursor.  A bare cursor also has a box with the face
          `hel-cursor-highlight', which shows the one character that it
          selects.  Any other selection is shown by the region alone.
          The bar shows where Emacs commands act, and it keeps its width with
          a proportional font, where the width of a box changes from character
          to character.

A text terminal always shows its own cursor at point, so there the
terminal cursor takes the place of the bar."
  :type '(choice (const :tag "Helix: a box" helix)
                 (const :tag "Hybrid: a bar at point" hybrid))
  :group 'hel)

(defcustom hel-bar-secondary-cursor ?\u2000
  "Character that shows a secondary cursor as a bar.
Secondary cursors are bars in Insert state on a graphic display.

The value must be either a character or a single-character string,
representing a narrow space. Recommended candidates are:
- ?\\u2000 EN QUAD (default value)
- ?\\u2002 EN SPACE
- ?\\u2009 THIN SPACE

The appearance depends heavily on:
1. Your main font.
2. The font Emacs selects for this narrow space — it should be
   a variable-pitch font. Use `describe-char' to verify the chosen font.

Emacs cannot draw two characters in one cell, so the bar character is
shown between two cells and moves the rest of the line to the right."
  :type '(set character string)
  :group 'hel
  :set (lambda (symbol value)
         (set-default symbol (cond ((characterp value)
                                    (char-to-string value))
                                   ((and (stringp value) (length= value 1))
                                    value)
                                   (t
                                    (char-to-string ?\u2000))))))

(defface hel-normal-state-main-cursor
  `((t :background ,(face-background 'cursor)
       :foreground ,(face-background 'default)))
  "Face of the main cursor in Normal state.
Hel draws this face over the character under the cursor.  Its background
is the color of the cursor, and Hel keeps it equal to the cursor color
of the theme.  Its foreground is the color of that character, and Hel
keeps it equal to the background of the default face, so the character
stays readable, as under the box cursor of Emacs."
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

(defface hel-normal-state-secondary-cursor
  '((t :foreground "white"
       :background "#ea0064"))
  "Face for secondary cursors when Hel is in Normal state."
  :group 'hel)

(defface hel-insert-state-secondary-cursor
  '((t :foreground "white"
       :background "SkyBlue3"))
  "Face for secondary cursors when Hel is in Insert state."
  :group 'hel)

(defface hel-cursor-highlight
  '((t :inherit region))
  "Face of the box of a bare main cursor when `hel-cursor-type' is `hybrid'.
The box covers the one character that the bare cursor selects, so by
default it looks like the region."
  :group 'hel)

(defface hel-other-window-cursor
  '((((class color) (min-colors 88) (background light))
     :background "gray85")
    (((class color) (min-colors 88) (background dark))
     :background "gray30")
    (t :inverse-video t))
  "Face of the box of the main cursor in a window that is not selected.
The selected window of a frame without focus uses it too.  The box only
shows where point is in that window, so it has a faded colour of its own."
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

(defcustom hel-want-C-left-bracket-esc t
  "If non-nil, `C-[' acts as the Escape key in GUI frames (as in a terminal)."
  :type 'boolean
  :group 'hel)

(defcustom hel-esc-delay 0.01
  "Seconds to wait for another key after a terminal Esc keypress.
If no further event arrives within this time, the lone `\\e' is
translated to the `escape' event so Hel's `<escape>' bindings fire.
Otherwise it is left as the standard ESC prefix (e.g. for `M-x')."
  :type 'number
  :group 'hel)

(defvar hel-inhibit-esc nil
  "If non-nil, never translate `\\e' to `escape'.")

(defcustom hel-use-pcre-regex t
  "If non-nil use PCRE regexp syntax instead of Emacs regular expressions."
  :type 'boolean
  :group 'hel)

(defcustom hel-mode-line-info
  '(:propertize
    ((hel-multiple-cursors-mode
      (:eval (format " %s cursors " (hel-number-of-cursors))))
     (:eval (when-let* ((counter (hel-search-counter)))
              (-let [(current . total) counter]
                (if current
                    (format " %d/%d " current total)
                  (format " %d " total))))))
    face mode-line-emphasis)
  "Mode line construct for the Hel state shown in the mode line.
It has the same format as `mode-line-format'.  The default shows the
number of cursors and the search match counter from
`hel-search-counter'."
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

The keys select the previous sibling, shrink the selection, expand it, and
select the next sibling. These commands are also bound to `M-p', `M-i',
`M-o', `M-n', as in the Helix text editor.

This variable must be set before Hel is loaded!"
  :type 'boolean
  :group 'hel)

(defcustom hel-whitelist-file (locate-user-emacs-file "hel-multiple-cursors")
  "File to save users preferences which commands to execute for one cursor
and which for all."
  :type 'file
  :group 'hel)

(defvar hel-minor-modes-incompatible-with-multiple-cursors
  '(corfu-mode
    company-mode
    flyspell-mode
    prettify-symbols-mode)
  "List of minor-modes that will be temporarily disabled while there are more
then one cursor in the buffer.")

(defvar-local hel-surround-alist nil
  "Alist of (KEY . SPEC) elements for the surround commands.
This variable is obsolete. Use `hel-pair-sources' for what `md' and `mr'
find, and `hel-surround-delimiters' for what `ms' and `mr' insert. An
element of this alist wins over both of them.

KEY is a character. SPEC is a plist:

`:insert'    A cons (LEFT . RIGHT) of the strings that `ms' inserts, or
             a function that returns one.

`:remove'    A cons (LEFT . RIGHT) of the strings around the text that
             `md' deletes, or a function that returns the four positions
             (LEFT-START LEFT-END RIGHT-START RIGHT-END) of the two
             delimiters around point.

`:regexp'    When non-nil, LEFT and RIGHT of `:remove' are regexps.

`:balanced'  When non-nil, the nested pairs of LEFT and RIGHT are skipped.")

(make-obsolete-variable 'hel-surround-alist
                        "use `hel-pair-sources' and `hel-surround-delimiters'."
                        "0.14.0")

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

`:main-cursor'   The face of the main cursor in STATE.

`:secondary-cursor'
               The face of the secondary cursors in STATE.

`:input-method'  When non-nil Hell will activate the enabled input method
               in STATE.

`:modes'         List of major and minor modes for which Hel intial state
               is STATE.

`:tag'           Mode-line tag for STATE.  A string, or a zero-argument
               function returning a string.")

(hel-defvar-permanent-local hel-input-method nil
  "Input method used in Hel Insert state.")

(hel-defvar-permanent-local hel-overriding-local-map nil)

(hel-defvar-permanent-local hel-extend-selection nil
  "Non-nil when motions extend the selection instead of replacing it.
The command `hel-extend-selection', bound to `v', toggles it. A motion
command passes it as `:merge-selections' to `hel-define-command'.")

(hel-defvar-permanent-local hel-selection-history nil
  "The selections of expand and text object commands run one after another.
The newest entry comes first and holds the current selection. The oldest
entry holds the selection before the first of these commands.
`hel-shrink-selection' returns to the previous entry.

Each entry is a list (SELECTION RUN). SELECTION has the form that
`hel-selection' returns.

RUN is non-nil when a repeated command selects several text objects in a row.
For example, select two functions with `mif mif'. The first `mif' creates the
selection and the second one extends it. RUN is a list (KEY FIRST LAST).
KEY is a list of the object name and the kind of range, `:inner' or `:around'.
FIRST and LAST are the first and the last selected objects.")

(hel-defvar-permanent-local hel--selections-before-insert nil
  "The selections of all cursors before the command that started Insert state.
Leaving Insert state puts them back when nothing was typed.  Only the
commands that start Insert state without changing the buffer set it, as
`i' and `a' do; it has the form that `hel-selections' returns.")

(hel-defvar-permanent-local hel--insert-tick nil
  "The value of `buffer-chars-modified-tick' when Insert state started.
Leaving Insert state compares it with the tick to tell whether anything
was typed.")

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
  "Hash table mapping secondary cursors IDs to their overlays.")

(defvar hel--secondary-cursor-last-used-id 0
  "Last used secondary cursor ID.")

(defvar hel-commands-to-run-for-all-cursors nil
  "Commands to execute for all cursors.")

(defvar hel-commands-to-run-once nil
  "Commands to execute only once while multiple cursors are active.")

(defvar hel-cursor-variables
  '(transient-mark-mode ; for `region-active-p'
    mark-active
    mark-ring
    kill-ring
    kill-ring-yank-pointer
    yank-undo-function
    temporary-goal-column
    hel-extend-selection
    hel-selection-history
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
  "Like `this-command' but for secondary cursors.
The command that that will be executed for each secondary cursor.")

(defvar hel--selection-record nil
  "The selection of the current buffer before the current command.
It is a list (BUFFER MARK ACTIVE? EXTENDING?): the buffer, the position of
the `mark', `mark-active' and `hel-extend-selection'.")

(defvar hel--main-cursor-error nil
  "The (DEPTH . ERROR) of the command at the main cursor, or nil.
DEPTH is the recursion depth where the command ran: a command in
a minibuffer that the command opened runs deeper, and its error is not
an error of the command.")

(hel-defvar-permanent-local hel--selection-owner nil
  "The window that owns the selection of the buffer.")

(defvar hel-executing-command-for-secondary-cursor nil
  "Non-nil if `this-command' is currently executing for secondary cursor.")

(hel-defvar-permanent-local hel--temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes while there are
multiple cursors.")

(hel-defvar-permanent-local hel--buffer-undo-list-pointer nil
  "Stores the start of the current undo step in `buffer-undo-list'.")

(hel-defvar-permanent-local hel--undo-selections nil
  "The values of all cursors at the beginning of the current undo step.
The list is the one that `hel-selections' returns.")

(hel-defvar-permanent-local hel-undo--previous-command-kind 'other
  "Kind of the previous editing command, as `hel-undo--command-kind' returns it.")

(hel-defvar-permanent-local hel--selections-history nil
  "The values of the cursors that `hel-restore-cursors' brings back.
The list is the one that `hel-selections' returns.")

(defvar-local hel--input-cache nil)

(hel-defvar-permanent-local hel--narrowed-base-buffer nil)

;;; .
(provide 'hel-vars)
;;; hel-vars.el ends here
