# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Hel** is a Helix/Kakoune-inspired modal text editing layer for Emacs 29.1+. It implements a selection-first editing model (select then act), multiple cursors, PCRE regex search, and smooth scrolling. Dependencies: `dash.el`, `avy`, `pcre2el`.

## Architecture

### Module Load Order

`hel.el` is the package entry point and loads core modules in strict dependency order:

```
hel-vars.el              → settings, customization groups, faces
hel-lib.el               → utilities, hel-motion-loop, highlight system, lines, mark ring
hel-macros.el            → hel-define-command, hel-define-advice, hel-advice-add
hel-multiple-cursors-core.el → fake cursor engine, undo/redo integration
hel-core.el              → state machine, hel-define-state, hel-keymap-set, pre/post-command hooks, hel-mode
hel-structural-editing.el → the thing protocol: producers, algorithms, drivers, thing definitions, the motions over things
hel-surround.el          → the text between delimiters: quotes, brackets, HTML tags
hel-commands.el          → all interactive editing commands: motions, selections, node commands, text objects
hel-search.el            → search, replace, highlighting
hel-scroll.el            → smooth and jump scrolling
hel-integration.el       → integration with other Emacs packages
hel-keybindings.el       → default key bindings
```

`hel-structural-editing.el` reads the tree-sitter queries of its text
objects from `queries/<language>/textobjects.scm`. These files are copies
of the Helix files under MPL-2.0; `queries/update.sh` refreshes them, and
nobody edits them by hand. Hel's own patterns live in
`queries/<language>/hel-textobjects.scm` under GPL-3.0-or-later. See
`queries/README.org`.

Extensions are independent and not loaded by default. Each lives in its own repo: `helheim-emacs/hel-leader`, `hel-org`, `hel-paredit`, `hel-vterm`, `hel-agent-shell`.

### State System

States (Normal, Insert, Emacs) are **not** minor modes. Instead, `hel-mode-map-alist` is stored in `emulation-mode-map-alists`, and the bound keymap is swapped on each state change. Each state has a global keymap plus nested keymaps discovered at state-change time by traversing active keymaps for special keys like `<normal-state>`.

### Multiple Cursors

Fake cursors are overlays storing point, mark, kill-ring, and buffer-local state. The real cursor's command runs first; `hel--post-command-hook` then replays it for each fake cursor. Command dispatch uses the `multiple-cursors` symbol property with three cases: `t` = run for all cursors; `nil` (property explicitly set) = run only for the main cursor; property absent = prompt the user and permanently store the choice in `hel-whitelist-file`. `hel-define-command` sets this property via `:multiple-cursors t/nil`. The `merge-selections` property controls whether overlapping selections are merged after the command.

### Key Hooks

- `hel--pre-command-hook` — extends selection if needed, starts undo boundary in Normal state
- `hel--post-command-hook` — executes command for all fake cursors, merges selections, ends undo boundary

## Coding Conventions

**Naming:**
- `hel-` prefix for public symbols
- `hel--` prefix for private/internal symbols
- `hel-fake-cursor-*` for cursor manipulation functions

**Macros to use:**
- `hel-define-command` — defines a command with `:multiple-cursors` and `:merge-selections` metadata
- `hel-define-advice` — advice that auto-attaches/detaches when `hel-mode` toggles
- `hel-advice-add` — equivalent for runtime advice registration
- `hel-defvar-local` (defined in `hel-vars.el`) — buffer-local variable that survives major-mode changes (`permanent-local` property)

**All files use `lexical-binding: t`.**

## No Build System

There is no Makefile, test runner, or CI. Byte-compile via the Emacs MCP
(`mcp__emacs__eval-elisp` + `byte-compile-file`); `emacs --batch` needs every
transitive dependency wired by hand. `byte-compile-file` writes a `.elc` next
to the source — delete it afterward (no build dir). `*Compile-Log*` is
read-only: read it with `buffer-substring-no-properties`, never `erase-buffer`.

To reload after edits (without restarting Emacs):
```elisp
(load-file "/path/to/hel-<module>.el")
```

## Testing

The tests live in `test/`. Load a test file with `load-file` and run it
with `(ert "^hel-")`; the tree-sitter tests skip themselves when a grammar
is missing.

Unit-test pure logic (overlay painting, match ranges, fold protocol) with
`with-temp-buffer` via the Emacs MCP. Do NOT drive interactive minibuffer
commands (search, select, filter) in the live Emacs — they block and disrupt
the user's active session. Simulate folds with overlays carrying `invisible`
plus `isearch-open-invisible`, and a matching `buffer-invisibility-spec`.

## Extension Pattern

Each extension lives in its own repo (`~/code/emacs/hel-<name>`).
Extensions hook into Hel using `hel-keymap-set` with a `:state`
keyword, `add-hook`, and `hel-define-command`. They do not modify
core files.

`hel-keymap-set` accepts an optional `:state` keyword (symbol or
list of symbols) before the key/definition pairs to scope bindings
to specific Hel states:

```elisp
(hel-keymap-set my-mode-map :state '(normal emacs)
  "f" #'my-command
  "b" nil)  ; unbind
```

## WIP / Scratch Files

- `search/` — contains `pcre-search-server.pl` (Perl PCRE backend) and `bench-json.pl` (benchmarking); supporting scripts for the PCRE search feature, not part of the Emacs package load path.
- `docs/` — user-facing documentation, not part of the package.
  Contains `customization.org`, `keybindings.org`,
  `emacs-folding-api.md`, `isearch-lazy-count.org`,
  `isearch-live-update-cycle.{md,org}`, and `archive/`.
- `keypad-notes.org` — scratch/research notes; not part of the package.
