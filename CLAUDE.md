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
hel-lib.el                    the selection, utilities, hel-motion-loop,
                              highlight system, lines, mark ring
hel-macros.el                 hel-define-command, hel-define-advice,
                              hel-advice-add
hel-cursors.el                secondary cursors: the object, access, values,
                              hel-with-each-cursor, the merge; drawing the
                              main and the secondary cursors
hel-core.el                   state machine, hel-define-state,
                              hel-keymap-set, pre/post-command hooks, hel-mode,
                              running a command for every cursor, command
                              settings, whitelists
hel-text-objects.el           the thing protocol, run producers, text objects,
                              the motions over things
hel-structural-editing.el     the tree level: operations on nodes, node
                              producers, the algorithms over trees
hel-surround.el               pairs of delimiters: the pair producers of
                              brackets, quotes and HTML tags, the key tables,
                              hel-mark-pair
hel-commands.el               all interactive editing commands: motions,
                              selections, node commands, text objects
hel-search.el                 search, replace, highlighting
hel-scroll.el                 smooth and jump scrolling (on ultra-scroll)
hel-integration.el            integration with other Emacs packages
hel-keybindings.el            default key bindings
```

`hel-text-objects.el` reads the tree-sitter queries of its text objects from
`queries/<language>/textobjects.scm`. These files derive from the Helix files
and stay under MPL-2.0, but Hel maintains them on its own. A pattern
captures the object itself under the name of the object (`@function`,
`@class`, `@argument`, `@entry`, `@comment`), and a shadowing pattern marks
the node of the smaller object that it replaces with `@_shadowed`. `@string`
captures the strings for the quote keys of `hel-surround.el`. Their regexes
are in Emacs syntax. See `queries/README.org`.

### Specification

`docs/design/specification.org` describes structural editing, text objects
and ranges: the rules that the code implements. Every change to that code
updates the specification in the same step, so the two always agree.
The specification holds rules, their reasons and the laws, a sentence or
two each. It holds no code listings, struct definitions, lists of tests or
private function names, because copied code drifts from the source. Tests
cite its section names (`Section "...` in `test/`), so check them before
renaming a section.

### State System

States (Normal, Insert, Emacs) are **not** minor modes. The symbol
`hel-mode-map-alist` is stored in `emulation-mode-map-alists`, and the keymap
bound to it is swapped on each state change. Each state has a global keymap
plus nested keymaps stored in other keymaps (usually major-mode maps) under
special keys like `<normal-state>`. On a state change Hel traverses the active
keymaps for these keys and activates the nested keymaps it finds.

### Multiple Cursors

Secondary cursors are overlays that store point, mark, kill-ring, and
buffer-local state; point and the mark are empty overlays, not markers.
`hel--save-cursor` and `hel--load-cursor` move the state between Emacs and
an overlay. The main cursor's command runs first; `hel--post-command-hook`
then replays it for each secondary cursor, in buffer order. A cursor where
the command signals an error is deleted (see "Cursors" in the
specification), so a command reports "nothing here" with `user-error`,
never with `message`: a message leaves that cursor behind, unlike the
others.

Command dispatch, in `hel-core.el`, uses the `multiple-cursors` symbol
property:

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
- `hel--post-command-hook` runs the command for all secondary cursors, merges
  overlapping selections, closes the undo step in Normal state, and updates
  the active keymaps.

## Coding Conventions

**Naming:**
- Spell words out: `:begin`, not `:beg`. Short names do not make code
  shorter or faster; they make the reader decode them. `pos` and `dir`
  are the exception
- A name starts with its subject, not with its file: `hel-node-parent`,
  not `hel-tree-parent`, because a node has a parent and a tree does not.
  The tree as a whole takes `hel-tree-`, one node `hel-node-`
- Use common English words, and reuse the term that Hel already has
  ("cursor character", "bare cursor") before you invent one. Write
  `char`, not `character`, as Emacs does (`forward-char`)
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
- `dir` is the direction of a motion. `side`, also 1 or -1, says on which
  side of a position something lies, or at which end of a selection point
  goes: `hel-node-touching`, `hel--tree-expand`
- A function that takes one step takes `dir`. A function that goes over
  COUNT things takes a signed `count`, as Emacs motions do, and computes
  `dir` inside: `hel-thing-reach`
- A name says `selection` or `cursor` by what its caller asks for. A
  function that takes ranges and selects them, or reads them, says
  `selection`, even when it creates or deletes cursors to hold them:
  `hel-select-ranges`, `hel-create-secondary-selection`. A function that
  handles a cursor that its caller holds, or runs code in the context of
  a cursor, says `cursor`: `hel-with-each-cursor`, `hel-make-cursor-main`.
  The names of the look say `cursor` too: faces, drawing, "bare cursor",
  "cursor character". See "Cursors" in the specification
- The selections are main and secondary, never primary: in Emacs
  "primary" names an X selection
- The feature keeps the name "multiple cursors", which users know:
  `hel-multiple-cursors-mode`, `:multiple-cursors`,
  `hel-cursor-variables`. `hel--node-under-cursor` keeps `cursor` too,
  because "head" alone does not say whose head
- `hel-secondary-cursor-*` for cursor manipulation functions
- Only commands read the variable `hel-extend-selection`, in their
  `interactive` form, and pass it on as the argument `extend`, optional in
  a command and required in a helper. No helper reads the flag. Only the
  command `hel-extend-selection`, the change of state and the window switch
  write it, all in `hel-core.el`. See "Ranges as values" in the
  specification.
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

**Libraries:** prefer `dash.el` over `cl-lib`, `seq` and built-in loops
wherever a function fits: `-each` over `dolist`, `-all?` over `cl-every`,
`-last-item` over `(car (last ...))`, `-let` for destructuring. Use the
threading macros (`->`, `->>`, `-some->>`) for a chain of three or more
steps, for a list that would otherwise come after a long lambda, or in
place of a `nil` check before the next call. Leave a simple `(f (g x))` as
it is. Never use the anaphoric `--` forms or `-->`: they bind `it`
implicitly. Fix the first arguments with `-partial`, or write a lambda.
Dash works on lists only: keep `aref` and `seq-*` for vectors. The
installed dash has no `-uniq-by`.

**Structure:**
- Order a file from the top: the main idea first, the details below. Lisp
  allows a call to a function that is defined later, so a forward
  reference is no cost.
- Add a macro only when the reader understands a call without expanding
  it, as with `hel-motion-loop`. Repetition alone is no reason.
- Ask the predicate that names the concept, never compute the concept from
  its parts: `hel--empty-line-range-p`, not a length and a test of the
  characters around the range. As with a raw loop, the reader should see
  the intention, not reconstruct it. When code needs a comment to say what
  it computes, look for the function that already has that name.
- Every public name is a cost. Add no command or option for a situation
  that a restart of Emacs solves.
- A method of a thing keeps `(eql 'THING)` next to its producer
  specializer, so that the reader sees which thing it serves.
- A redesign follows the new model (producers, things, Helix semantics)
  and gives each rule its own reason, never "as the old command does".

**Selections:** a bare cursor is a selection of one character, the cursor
character. Whether the mark is active is invisible to the user, so a
command treats a bare cursor like any other selection, and never gives the
"no region" case behaviour of its own. Where a command does treat one
character in its own way, it names the concept: it asks `hel-bare-cursor-p`,
never `use-region-p` or the length of the range, such as
`(< 1 (- end begin))`. To get the bounds as well, destructure the value:
`(-let [(selection &as (begin . end)) (hel-selection)] ...)`. To pass
the range on whole, ask `(hel-selection-bounds selection)`: never bind it
with `(selection &as range)`, which reads as if `selection` and `range`
were the same thing. Read the
selection once, with `(hel-selection)`, and pass the value to the
predicates and accessors: `(hel-bare-cursor-p selection)`, not
`(hel-bare-cursor-p)` next to `(hel-selection-bounds)`. Read it again only
after the command has changed it. The line
selection of an empty line covers one character too, but it is not a bare
cursor: the user sees a selected line. The code that only stores
a selection, `hel-select`, the writers of the secondary cursors and the
merge, asks the length of the range instead, `(hel-range-length range)`:
there the length is the question, because a direction needs at least two
characters. Never wrap a range as `(list range)` to ask
`hel-bare-cursor-p`. The third element of a selection value is a private
flag. Never read it: ask `hel-line-selection-p` or `hel-bare-cursor-p`.
Pass it to `hel-select` only through `(apply #'hel-select selection)`, or
as t to select an empty line as a line.

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

## Build and Test

The `Makefile` runs the compile check and the tests, each in a fresh
`emacs -Q --batch`. `test/README.org` is the full manual.

```sh
make check                    # compile every file, then run every suite
make test-node                # one suite: behaviour, node, text-object,
                              # word-motion, selection, search, surround
make test-behaviour SELECTOR='^hel-'   # only the tests whose names match
make -j8 compile              # byte-compile each file into build/; a warning fails
make bench SCENARIO=insert    # time a command with 1, 100 and 1000 cursors
make profile PROFILER=cpu     # profile the benchmark scenarios
make deps                     # install the dependencies into .deps/
```

- Use `make` for every compile check and test run. A batch run cannot
  disturb the user's Emacs, and it tests the files on disk.
- The tests run the compiled files in `build/`; `COMPILED=no` runs them on
  the sources.
- `make compile` compiles each file in an Emacs of its own, so a missing
  `require` shows up.
- Never run `hel-behaviour-tests.el` in the live Emacs, and never drive
  interactive minibuffer commands (search, select, filter) there: a prompt
  blocks the user's active session. Under `make`, every prompt fails at once.
- Never run `make emacs`: it opens a window on the user's screen.
- `make test-display` runs the suites in `emacs -nw` inside a hidden
  pseudo-terminal, where Emacs redisplays. Use it for code that depends on
  the display: batch Emacs has no `posn-at-point`, a wrong `window-end`, and
  visual line commands that do not move.
- An endless loop in the code under test freezes the live Emacs. Try a new
  loop with `make` first; `TIMEOUT` ends a batch run.
- Every command runs once more for each secondary cursor, so speed matters.
  Before and after a change to code that runs for every cursor, run
  `make bench` on the scenarios that use it and compare the tables.
- `make` reads the dependencies from `.deps/` when it exists, and otherwise
  from the Elpaca build directory. Set `HEL_TEST_DEPS` for another layout.
- CI (`.github/workflows/test.yml`) runs `make deps` and `make -k check` on
  Emacs 29.4, 30.2, 31.1 and the snapshot.
- The user edits the files while an agent works. A temporary change, such
  as breaking code to check that a test catches it, goes into a separate
  `git worktree` in a scratch directory, never into the user's checkout.
- A rename covers `hel/*.el`, `test/`, `docs/design/`, `docs/*.org` and the
  extensions that use the name, such as `hel-paredit` and its `test/`.
  It leaves `CHANGELOG.org` and the archive alone, because they record
  history. `hel-dot-repeat` is a stale copy of Hel, so a match there says
  nothing about who uses a name. Before renaming an argument or a local
  variable, search the function body for the new name.

The Emacs MCP (`mcp__emacs__eval-elisp`) is still the tool for quick
experiments in the live Emacs:

- Unit-test pure logic (overlay painting, match ranges, fold protocol) with
  `with-temp-buffer`. Simulate folds with overlays carrying `invisible` plus
  `isearch-open-invisible`, and a matching `buffer-invisibility-spec`.
- The live Emacs sets `byte-compile-warnings` to nil on purpose. Never change
  it.
- `byte-compile-file` writes a `.elc` next to the source, and it can leave
  stale compiler macros for `cl-defstruct` types in the session. Use
  `make compile` instead.
- `*Compile-Log*` is read-only: read it with `buffer-substring-no-properties`,
  never `erase-buffer`.

To reload after edits (without restarting Emacs):

```elisp
(load-file "/path/to/hel-<module>.el")
```

A new core file also needs `elpaca-rebuild` before an Emacs restart, because
Elpaca links the package files one by one.

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
  `keybindings.org`), demo videos, and `design/`: the specification, the
  open questions (`open-questions.org`), the notes of work that is still
  open (`line-selection.org`, `multiple-cursors-performance.org`,
  `sean-parent-goals-review.org`), and `archive/` for design notes that are
  implemented or out of date. Not part of the package.
- `queries/`: tree-sitter text object queries, see above.
- `test/`: the test suites, their harness `hel-test-helpers.el`, and the
  files that set up a batch Emacs for them, see Build and Test.
- `CHANGELOG.org`: user-visible changes per release.
