# Build, test and profile Hel in a batch Emacs.
# See test/README.org for the manual.

EMACS    ?= emacs
TIMEOUT  ?= 600

# An ERT selector: a regexp that the test names must match.  Empty means
# all the tests of a suite.
SELECTOR ?=

# COMPILED=no runs the tests on the sources instead of the compiled files.
COMPILED ?= yes

# Lists of directories, separated by colons.  test/hel-test-paths.el
# reads them; empty means the default there.
HEL_TEST_DEPS    ?=
HEL_TEST_TREESIT ?=

# The benchmarks: the numbers of cursors, the repetitions of each scenario,
# a regexp that the scenario names must match, and the profiler for
# `make profile': elp or cpu.
CURSORS   ?= 1 100 1000
REPEAT    ?= 5
SCENARIO  ?=
PROFILER  ?= elp

export HEL_TEST_DEPS HEL_TEST_TREESIT
export HEL_TEST_SELECTOR  := $(SELECTOR)
export HEL_BENCH_CURSORS  := $(CURSORS)
export HEL_BENCH_REPEAT   := $(REPEAT)
export HEL_BENCH_SCENARIO := $(SCENARIO)
export HEL_BENCH_PROFILER := $(PROFILER)

BUILD := build

# The tests load the compiled files from BUILD before the sources.  The
# compilation itself always reads the sources.
ifeq ($(COMPILED),yes)
RUN_FROM := HEL_TEST_BUILD=$(BUILD)
TEST_PREREQUISITES := compile
else
RUN_FROM := HEL_TEST_BUILD=
TEST_PREREQUISITES :=
endif

# A batch Emacs reads a minibuffer prompt from standard input.  With no
# input, a prompt in a test fails at once instead of waiting.
BATCH := timeout $(TIMEOUT) $(EMACS) -Q --batch

SUITES  := behaviour node text-object word-motion selection search surround
SOURCES := $(wildcard hel*.el)
ELC     := $(SOURCES:%.el=$(BUILD)/%.elc)

TEST_TARGETS    := $(addprefix test-,$(SUITES))
DISPLAY_TARGETS := $(addprefix test-display-,$(SUITES))
COMPILE_TARGETS := $(addprefix compile-,$(basename $(SOURCES)))

RUN_ERT := --eval '(ert-run-tests-batch-and-exit (let ((s (getenv "HEL_TEST_SELECTOR"))) (if (and s (not (string-empty-p s))) s t)))'

.PHONY: all check test test-display compile deps emacs bench profile clean help \
        $(TEST_TARGETS) $(DISPLAY_TARGETS) $(COMPILE_TARGETS)

all: check

check: compile test

# Tests

test: $(TEST_TARGETS)

$(TEST_TARGETS): test-%: $(TEST_PREREQUISITES)
	$(RUN_FROM) $(BATCH) -l test/hel-test-setup.el -l test/hel-$*-tests.el \
	  $(RUN_ERT) < /dev/null

# The same suites in `emacs -nw' inside a pseudo-terminal of 30x100, where
# Emacs redisplays as for a user.  The results go to a report file, which
# the target prints.
test-display: $(DISPLAY_TARGETS)

$(DISPLAY_TARGETS): test-display-%: $(TEST_PREREQUISITES)
	@mkdir -p $(BUILD)
	@rm -f $(BUILD)/display-$*.txt
	@$(RUN_FROM) HEL_TEST_REPORT=$(BUILD)/display-$*.txt \
	  python3 test/hel-test-tty.py 30 100 $(TIMEOUT) \
	  $(EMACS) -nw -Q -l test/hel-test-setup.el -l test/hel-$*-tests.el \
	  -l test/hel-test-display.el; \
	  status=$$?; echo 'test-display-$*:'; cat $(BUILD)/display-$*.txt; exit $$status

# A fresh interactive Emacs with the test setup and every suite loaded.
# ERT shows the results, and a failing test can be inspected and rerun.
emacs: $(TEST_PREREQUISITES)
	$(RUN_FROM) $(EMACS) -Q -l test/hel-test-setup.el \
	  $(foreach suite,$(SUITES),-l test/hel-$(suite)-tests.el) \
	  --eval '(ert (let ((s (getenv "HEL_TEST_SELECTOR"))) (if (and s (not (string-empty-p s))) s t)))'

# Compilation

compile: $(ELC)

$(COMPILE_TARGETS): compile-%: $(BUILD)/%.elc

# A change to any source can change the macros that another file expands,
# so every compiled file depends on every source.
$(BUILD)/%.elc: %.el $(SOURCES)
	HEL_TEST_BUILD= HEL_TEST_OUTPUT=$(BUILD) $(BATCH) -l test/hel-test-compile.el $<

# Dependencies

deps:
	$(BATCH) -l test/hel-test-deps.el

# Benchmarks

bench: $(TEST_PREREQUISITES)
	$(RUN_FROM) $(BATCH) -l test/hel-test-setup.el -l test/hel-benchmarks.el \
	  -f hel-bench-run < /dev/null

profile: $(TEST_PREREQUISITES)
	$(RUN_FROM) $(BATCH) -l test/hel-test-setup.el -l test/hel-benchmarks.el \
	  -f hel-bench-profile < /dev/null

clean:
	rm -rf $(BUILD)
	rm -f *.elc test/*.elc

help:
	@echo 'make check          compile every file, then run every suite'
	@echo 'make test           run every suite on the compiled files'
	@echo 'make test-SUITE     run one suite: $(SUITES)'
	@echo 'make test-display   run every suite in a terminal frame, with redisplay'
	@echo 'make compile        byte-compile every file into $(BUILD)/, fail on warnings'
	@echo 'make compile-FILE   byte-compile one file, e.g. compile-hel-lib'
	@echo 'make deps           install the dependencies into .deps/'
	@echo 'make emacs          open an interactive Emacs with the tests loaded'
	@echo 'make bench          time the commands with many cursors'
	@echo 'make profile        profile the benchmark scenarios'
	@echo 'make clean          remove $(BUILD)/ and stray .elc files'
	@echo
	@echo 'Variables: SELECTOR, COMPILED, EMACS, TIMEOUT, HEL_TEST_DEPS,'
	@echo '           HEL_TEST_TREESIT, CURSORS, REPEAT, SCENARIO, PROFILER'
