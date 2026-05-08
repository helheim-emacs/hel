# Hel — [Helix](https://helix-editor.com/) Emulation Layer for Emacs

## Key features

- Multiple cursors based modal editing inside Emacs!
- Undo/redo that plays well with multiple cursors.
- PCRE regexps by default (thanks to [pcre2el](https://github.com/joddie/pcre2el)).
- Smooth scrolling commands out of the box.

## Can I use Hel without knowing Emacs keys?

> [!IMPORTANT]
> What Vim, Helix, and other modal editors call Normal and Insert **modes**, Hel refers to as **states**. This is because the word "mode" in Emacs is already used for its [major](https://www.gnu.org/software/emacs/manual/html_node/emacs/Major-Modes.html) and [minor](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html) modes.

When several years ago I came to Emacs from Neovim, I was in love with Vim editing model, found Emacs native keybindings ugly and had zero interest in learning them. I want Emacs not as text editor (which it obviously lacks of) but as an operation system with Lisp and all its power. So the first question I asked myself was "Can I use Evil without learning Emacs keys?"

The answer is yes. I have never used — and still don't know — most Emacs keys. I know only a few that you need when something breaks early during Emacs startup and you don't have your Hel keys available. They are:

- `M-x` — Command palette. The main key you need; all other commands can be invoked from it.
- `M-w` — Copy the selected text to google the error message or feed it to LLM.
- `C-x C-s` — Save current buffer.
- `C-x C-c` — Exit Emacs.

That's it.

Hel and Emacs do not interfere much, because Emacs is not a modal editor: letters and numbers are self-inserting, and most command key chords begin with `C-x` or `C-c` (e.g. `C-x n d`). Due to this, Hel works as a layer on top of Emacs.

In Normal state you have selection-based editing, multiple cursors, and all the other Hel features. In Insert state, Hel steps aside and standard Emacs keys work as usual. Also Hel doesn't touch `C-x` and `C-c` so they are always available. This allows you to mix Hel and Emacs in any proportion.

## Kakoune vs Helix

The main difference between Kakoune and Helix, in terms of text editing, is how they handle expanding selections: Kakoune uses `Shift` + motions, while Helix uses a separate state on the `v` key. Since I originally came from Vim, I prefer Helix's `v` key, so I chose Helix. However, Kakoune (as far as I know) was the original inventor of this keyboard-driven multiple-selections approach, and it deserves credit.

## Installation

Hel is not yet on MELPA. You can install it directly from Github.

### Emacs built-in package manager

This is the most minimal example of the `init.el` file:

```emacs-lisp
;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;; Dependencies
(use-package dash :ensure t)
(use-package avy :ensure t)
(use-package pcre2el :ensure t)

(use-package hel
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :custom (inhibit-startup-screen t)
  :config (hel-mode))

;;; init.el ends here
```

### [Elpaca](https://github.com/progfolio/elpaca)

```emacs-lisp
(elpaca 'dash)
(elpaca 'avy)
(elpaca 'pcre2el)

(elpaca '(hel :host github :repo "anuvyklack/hel")
  (setopt inhibit-startup-screen t)
  (hel-mode))
```

### [Straight](https://github.com/radian-software/straight.el)

```emacs-lisp
(straight-use-package 'dash)
(straight-use-package 'avy)
(straight-use-package 'pcre2el)

(straight-use-package '(hel :host github :repo "anuvyklack/hel"))
(setopt inhibit-startup-screen t)
(hel-mode)
```

## Documentation

- [Keybindings](docs/keybindings.org)
- [Customizations](docs/customization.org)

## Differences from Helix text editor

This package is not one-to-one emulation. Some commands are implemented in slightly different way (improved from the author's point of view), and some features like keyboard macros, registers, and jumplists already have their alternatives in Emacs.

- In Emacs the cursor ("point" in Emacs terms) is located **between** two characters rather than **on** a character like in Helix or Vim. I decided to keep this behavior, instead of emulating original one, as Evil does, because the primary object of interaction in Helix approach is a selection, not the cursor itself.

- `x` and `X` commands are reworked. They are expand and contract line-wise selection down when cursor is at the end of the selection, or up when cursor is at the beginning of the selection.

- Inner objects are additionally available directly under `m` prefix to reduce keystrokes: `mw` is the same as `miw` — select word.

- Mark commands accept numeric arguments:
  `m2ip` or `2mip` — select 2 paragraphs.

- You can restore last multiple selections with `gv`.

- `gs`, `gh`, and `gl` make selections. This is done for convenience, since all other motions make selections. In Helix they only move the cursor without creating selection.

- `gs` and `gh` are swapped: `gs` moves to the beginning of a line, `gh` moves to the beginning of a line skipping indentation.

- Keys that are relevant only when multiple cursors are present will be active only in that case (e.g. `K`, `,`, `&` — full list is in `hel-multiple-cursors-mode-map` keymap). This allows to reuse, for example, `K` for documentation lookup or `,` for localleader when there is only one cursor in the buffer.

- `gg` / `G` to go to the first/last line of the buffer like in Vim.
  Helix uses `gg` / `ge`.

- Scrolling keybindings are taken from Vim instead of Helix.

- Six easymotion commands are provided:
  - `gw` / `gb` — chose and mark word forward/backward.
  - `gW` / `gB` — chose and mark WORD forward/backward.
  - `gj` / `gk` — go to line down/up.

  Helix provides only `gw` to place 2-char hints at the beginning of each word.

- `f`, `F`, `t`, `T` commands to move to char are enhanced: they show hints for targets, and while hints are active, they can be repeated with `n` / `N` keys.

- When you search backward with `?` command, while hints are active `n` and `N` keys are swapped: `n` will repeat search backward and `N` — forward, like in Vim.

## Commands that are not implemented

- `.` (repeat) — Need to decide what it should repeat.
- `r` — replace (TODO)
- `M-u`, `M-U` — traverse undo tree
- `q`, `Q` — record keyboard macros

## Extensions

- [hel-leader](https://github.com/anuvyklack/hel-leader) — Use `Space` as a leader key.
- [hel-org](https://github.com/anuvyklack/hel-org) — [Org-mode](https://orgmode.org/) integration.
- [hel-paredit](https://github.com/anuvyklack/hel-paredit) — structural editing for S-expressions.
- [hel-vterm](https://github.com/anuvyklack/hel-vterm) — [vterm](https://github.com/akermu/emacs-libvterm) terminal emulator integration.
- [hel-agent-shell](https://github.com/anuvyklack/hel-agent-shell) — [agent-shell](https://github.com/xenodium/agent-shell) integration.

## Tips

- By default, Hel uses a bar cursor for Normal state and a box cursor for Insert state—the opposite of what Vim does. Your first instinct may be to switch them back to what you're used to, but I recommend not doing so. This was the first I done myself, and went through all the stages of acceptance, give default settings a try — the bar cursor is better suited for Normal state.

- You can set localleader keymap to `,`. It will act as the local leader while there is only one cursor in the buffer, and will delete all secondary cursors when there are multiple cursors.

## Acknowledgments

Hel depends on [dash.el](https://github.com/magnars/dash.el), [pcre2el](https://github.com/joddie/pcre2el), and [avy](https://github.com/abo-abo/avy) wonderful packages.

Hel is heavily inspired by:
- [evil](https://github.com/emacs-evil/evil)
- [multiple-cursors.el](https://github.com/magnars/multiple-cursors.el)
- [kak.el](https://github.com/aome510/kak.el)
- [surround](https://github.com/mkleehammer/surround)
- [meow](https://github.com/meow-edit/meow)
- [doomemacs](https://github.com/doomemacs/doomemacs)
- [crux](https://github.com/bbatsov/crux)

You are welcome to go and give them all at least a star!

## Contributing

### Share

A quick post about this package on your blog or social network could bring
new users to Emacs, which would be great!

### Support the development

Hel was developed on an old laptop with a cracked screen, and I worked on it instead of grinding LeetCode. If you'd like to support Hel's development, you can do so with a donation:

- [PayPal](https://www.paypal.me/anuvyklack)

Every contribution is greatly appreciated.
