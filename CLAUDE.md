# CLAUDE.md

## Project Overview

**Hel** is a Helix/Kakoune-inspired modal text editing layer for Emacs 29.1+.
It implements a selection-first editing model (select, then act), multiple
cursors, structural editing over syntax trees, text objects, PCRE regex
search, and smooth scrolling.

Dependencies (see `Package-Requires` in `hel.el`): `dash`, `avy`, `pcre2el`,
`ultra-scroll`. Tree-sitter support uses the built-in `treesit` library;
a buffer without a parser falls back to the syntax table.

## Architecture

### Module Load Order

`hel.el` is the package entry point. It loads the core modules in strict
dependency order:

```
hel-vars.el                   settings, customization groups, faces,
                              hel-defvar-permanent-local
hel-lib.el                    utilities, hel-motion-loop, highlight system,
                              lines, mark ring
hel-macros.el                 hel-define-command, hel-define-advice,
                              hel-advice-add
hel-multiple-cursors-core.el  fake cursor engine, undo/redo integration
hel-core.el                   state machine, hel-define-state,
                              hel-keymap-set, pre/post-command hooks, hel-mode
hel-text-objects.el           the thing protocol, run producers, text objects,
                              the motions over things
hel-structural-editing.el     the tree level: operations on nodes, node
                              producers, the algorithms over trees
hel-surround.el               the text between delimiters: quotes, brackets,
                              HTML tags
hel-commands.el               all interactive editing commands: motions,
                              selections, node commands, text objects
hel-search.el                 search, replace, highlighting
hel-scroll.el                 smooth and jump scrolling (on ultra-scroll)
hel-integration.el            integration with other Emacs packages
hel-keybindings.el            default key bindings
```

`hel-text-objects.el` reads the tree-sitter queries of its text objects from
`queries/<language>/textobjects.scm`. These files derive from the Helix files
and stay under MPL-2.0. Their regexes are in Emacs syntax, and Hel's own
patterns sit in a section at the end of each file. `queries/HELIX-REVISION`
records the Helix commit they match, and `queries/helix-changes.sh` shows the
Helix changes to port by hand. See `queries/README.org`.

### Specification

`docs/design/specification.org` describes structural editing, text objects
and ranges: the rules that the code implements. Every change to that code
updates the specification in the same step, so the two always agree.

### State System

States (Normal, Insert, Emacs) are **not** minor modes. The symbol
`hel-mode-map-alist` is stored in `emulation-mode-map-alists`, and the keymap
bound to it is swapped on each state change. Each state has a global keymap
plus nested keymaps stored in other keymaps (usually major-mode maps) under
special keys like `<normal-state>`. On a state change Hel traverses the active
keymaps for these keys and activates the nested keymaps it finds.

### Multiple Cursors

Fake cursors are overlays that store point, mark, kill-ring, and buffer-local
state. The real cursor's command runs first; `hel--post-command-hook` then
replays it for each fake cursor.

Command dispatch uses the `multiple-cursors` symbol property:

- `t`: run the command for all cursors.
- `nil` (property explicitly set): run it only for the main cursor.
- property absent: prompt the user and store the choice permanently in
  `hel-whitelist-file`.

`hel-define-command` sets this property with `:multiple-cursors t/nil`. Its
`:merge-selections` keyword sets the `merge-selections` property: a symbol or
a form that decides whether overlapping selections are merged after the
command.

### Key Hooks

- `hel--pre-command-hook` sets the mark when the selection is being extended.
  In Normal state it opens a single undo step; in Insert state it may split
  the current undo step.
- `hel--post-command-hook` runs the command for all fake cursors, merges
  overlapping selections, closes the undo step in Normal state, and updates
  the active keymaps.

## Coding Conventions

**Naming:**
- `hel-` prefix for public symbols: an extension or a user may rely on them
- `hel--` prefix for symbols internal to Hel core: they can change without
  notice, any core file may use them, and no extension may
- A helper in `hel-lib.el` is public unless it depends on an internal detail
  of Hel
- Each of these words keeps one meaning in a name:
  - `at`: inside or on a boundary, as in Emacs
  - `enclosing`: strictly inside, not on a boundary
  - `touching`: on a boundary, on a given side
  - `under`: holds the character at the position
  - `on`: covers a range
  - `after`: returns a position and does not move point; `skip` moves point
  - `bounds`: the two positions of something that a function returns; `range`
    is the form of a value, a (BEGIN . END) pair, and the name of an argument
- The `hel-node-*` operations always take a position. The private helpers named
  `-at-point` or `-point` always use point and never take it
- `pos` and `dir` are the argument names, not `position` and `direction`
- A direction is the number 1 or -1, never a keyword like `:forward`
- `hel-fake-cursor-*` for cursor manipulation functions
- `-p` for a predicate function, including a local one in `cl-flet`, and `?`
  for a local variable that holds a flag: `forward?`, `extend?`

**Macros to use:**
- `hel-define-command`: defines a command with `:multiple-cursors` and
  `:merge-selections` metadata
- `hel-define-advice`: advice that attaches and detaches itself when
  `hel-mode` toggles
- `hel-advice-add`: the same for advice registered at runtime
- `hel-defvar-permanent-local` (in `hel-vars.el`): a buffer-local variable
  that survives major-mode changes (`permanent-local` property)

**Libraries:** use `dash.el` functions and its threading macros (`->`, `->>`,
`-some->>`) freely, but not the anaphoric `--` forms.

**All files use `lexical-binding: t`.**

**Docstrings:**
- Write for an experienced Emacs user who does not know the internals of Hel
  and may not be a native English speaker. Avoid private shorthand such as
  "the THING on RANGE"; say what the result means to the user.
- Put the main idea first: what the function returns or does. The rules for
  special cases come next, and the form of the arguments comes last ("RANGE is
  a (BEGIN . END) pair", "PRODUCER finds the things").
- A `cl-defgeneric` holds the whole contract: the result, the preconditions,
  and the rules that every method keeps. The callers rely on these rules, so
  they belong to the generic, even when only the default method carries them
  out.
- A `cl-defmethod` documents only what is its own: how this producer finds the
  result, or where it departs from the contract. A method that only carries out
  the contract has no docstring. `describe-function` shows the generic and then
  every method, so a repeated contract appears twice.
- A default method has no docstring. When the default behaviour matters to a
  caller or to someone who writes a producer, the generic says so in a last
  paragraph that starts with "By default".

## No Build System

There is no Makefile, test runner, or CI. Byte-compile via the Emacs MCP
(`mcp__emacs__eval-elisp` + `byte-compile-file`); `emacs --batch` needs every
transitive dependency wired by hand.

- The live Emacs sets `byte-compile-warnings` to nil on purpose. Bind it to t
  around the compile check; never change it globally.
- `byte-compile-file` writes a `.elc` next to the source. Delete it afterward
  (there is no build dir).
- `*Compile-Log*` is read-only: read it with `buffer-substring-no-properties`,
  never `erase-buffer`.
- A compile check can leave stale compiler macros for `cl-defstruct` types in
  the session. `load-file` the source again before running tests.

To reload after edits (without restarting Emacs):

```elisp
(load-file "/path/to/hel-<module>.el")
```

A new core file also needs `elpaca-rebuild` before an Emacs restart, because
Elpaca links the package files one by one.

## Testing

The tests live in `test/`.

- `hel-structural-editing-tests.el`, `hel-surround-tests.el` and
  `hel-search-tests.el` run in the live Emacs: `load-file` the test file and
  run `(ert "^hel-")`. The tree-sitter tests skip themselves when a grammar
  is missing.
- `hel-behaviour-tests.el` drives real commands and stops at minibuffer
  prompts, so it runs only in batch, through `hel-test-setup.el`:

  ```sh
  timeout 300 emacs -Q --batch -l test/hel-test-setup.el \
    -l test/hel-behaviour-tests.el -f ert-run-tests-batch-and-exit
  ```

Unit-test pure logic (overlay painting, match ranges, fold protocol) with
`with-temp-buffer` via the Emacs MCP. Do NOT drive interactive minibuffer
commands (search, select, filter) in the live Emacs: they block and disrupt
the user's active session. Simulate folds with overlays carrying `invisible`
plus `isearch-open-invisible`, and a matching `buffer-invisibility-spec`.

An endless loop in the code under test freezes the live Emacs. Try a new loop
in batch first.

## Extension Pattern

Extensions are independent and not loaded by default. Each lives in its own
repo under the `helheim-emacs` GitHub organization, checked out next to this
one as `~/code/helheim/hel-<name>`: `hel-leader`, `hel-org`, `hel-paredit`,
`hel-vterm`, `hel-ghostel`, `hel-agent-shell`. They do not modify core files
and may use only public `hel-` symbols.

Extensions hook into Hel using `hel-keymap-set` with a `:state` keyword,
`add-hook`, and `hel-define-command`. `hel-keymap-set` accepts an optional
`:state` keyword (symbol or list of symbols) before the key/definition pairs
to scope bindings to specific Hel states:

```elisp
(hel-keymap-set my-mode-map :state '(normal emacs)
  "f" #'my-command
  "b" nil)  ; unbind
```

## Other Directories

- `docs/`: user-facing documentation (`customization.org`,
  `keybindings.org`), demo videos, and `design/`: the specification and
  design notes under review (`open-questions.org`, `search-session.org`,
  `thing-protocol-review.org`). Not part of the package.
- `queries/`: tree-sitter text object queries, see above.
- `CHANGELOG.org`: user-visible changes per release.
