;;; hel-behaviour-tests.el --- Behaviour tests for Hel -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; These tests describe what a Hel user observes: they press keys through the
;; real keymaps and look at the text, the selection, the cursors and the state.
;;
;; Run them from the root of a checkout:
;;
;;   timeout 300 emacs -Q --batch -l test/hel-test-setup.el \
;;     -l test/hel-behaviour-tests.el -f ert-run-tests-batch-and-exit
;;
;; The texts of the tests use the notation of `hel-test-helpers.el'.  A
;; table row
;;
;;   ("foo |bar baz" "w" "foo ^bar |baz")
;;
;; reads: with the cursor before "bar", press `w', and "bar " becomes selected
;; with the cursor on its right side.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'python)
(require 'cc-mode)
(require 'outline)
(require 'hel)
(require 'hel-test-helpers
         (expand-file-name "hel-test-helpers"
                           (file-name-directory (or load-file-name buffer-file-name))))

;;; Reading the state of Hel

(defun hel-test-search-register ()
  "Return the content of the register `/' that holds the search pattern."
  (get-register '/))

(defun hel-test-search-highlights ()
  "Return the number of search matches that are highlighted in the buffer."
  (cl-count-if (lambda (overlay)
                 (eq (overlay-get overlay 'face) 'hel-search-highlight))
               (overlays-in (point-min) (point-max))))

;;; Tables of test cases

(cl-defun hel-test--check-row (mode input keys expected
                                    &key state error kill yank text-only)
  "Check one row of a table.
Press KEYS in a buffer in major MODE with marked text INPUT, and compare the
buffer with the marked text EXPECTED.

STATE is the expected Hel state, Normal by default.
ERROR is a regexp for the message of the error that the keys must signal.
KILL is the expected top of the `kill-ring', and YANK is put on it before.
TEXT-ONLY non-nil means that the row compares only the text and ignores the
cursors and selections; EXPECTED then has no marks."
  (hel-test-with mode input
    (when yank (kill-new yank))
    (let ((failure (hel-test-press keys)))
      (cond (error
             (should (stringp failure))
             (should (string-match-p error failure)))
            (t
             (should (equal failure nil)))))
    (if text-only
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       expected))
      (should (equal (hel-test-render) expected)))
    (should (eq (hel-test-state) (or state 'normal)))
    (when kill
      (should (equal (substring-no-properties (or (car kill-ring) "")) kill)))))

(defmacro hel-test-table (group &rest args)
  "Define one ERT test for each row of a table of cases.
GROUP is the part of the test name after the prefix `hel-behaviour-'.
ARGS starts with an optional :mode MODE, `text-mode' by default.
Each remaining element is a row (INPUT KEYS EXPECTED [KEYWORD VALUE]...),
see `hel-test--check-row'.  A row can start with the keyword :mode to replace
MODE: (:mode MODE INPUT KEYS EXPECTED...)."
  (declare (indent 1))
  (let ((mode 'text-mode)
        (index 0)
        forms)
    (when (eq (car args) :mode)
      (setq mode (cadr args)
            args (cddr args)))
    (dolist (row args)
      (cl-incf index)
      (let ((row-mode mode))
        (when (eq (car row) :mode)
          (setq row-mode (cadr row)
                row (cddr row)))
        (push `(ert-deftest ,(intern (format "hel-behaviour-%s-%02d" group index)) ()
                 ,(format "%s: `%s' in %s on %S." group (nth 1 row) row-mode (car row))
                 (hel-test--check-row ',row-mode
                                      ,@(mapcar (lambda (x) (list 'quote x)) row)))
              forms)))
    `(progn ,@(nreverse forms))))

;;; Environment

(ert-deftest hel-behaviour-environment-loads-this-checkout ()
  "The Hel under test comes from `load-path', and not from an Elpaca build."
  (should-not (string-match-p "/elpaca/builds/hel" (locate-library "hel"))))

(ert-deftest hel-behaviour-environment-is-isolated ()
  "The test session writes only into its temporary directory."
  (should (file-in-directory-p user-emacs-directory hel-test--tmp-dir))
  (should (file-in-directory-p hel-whitelist-file hel-test--tmp-dir)))

(ert-deftest hel-behaviour-environment-starts-in-normal-state ()
  "A text buffer starts in Normal state with one cursor and no selection."
  (hel-test-with 'text-mode "foo|"
    (should (eq (hel-test-state) 'normal))
    (should (equal (hel-test-fake-cursors) nil))
    (should-not (use-region-p))))

(ert-deftest hel-behaviour-environment-prompt-is-reported ()
  "A key sequence that ends inside a prompt is an error and does not block."
  (hel-test-with 'text-mode "foo|"
    (should (hel-test-press "f"))))

;;; Motions: characters and lines

(hel-test-table motion-char
  ("foo |bar" "l" "foo b|ar")
  ("foo |bar" "h" "foo| bar")
  ("foo |bar" "<right>" "foo b|ar")
  ("foo |bar" "<left>" "foo| bar")
  ("|foobar" "3l" "foo|bar")
  ("foobar|" "3h" "foo|bar")
  ;; A character motion drops the selection.
  ("^foo| bar" "l" "foo |bar")
  ("^foo| bar" "h" "fo|o bar")
  ;; Point crosses the end of a line.
  ("foo|\nbar" "l" "foo\n|bar")
  ("foo\n|bar" "h" "foo|\nbar")
  ;; At the end of the buffer the motion fails and nothing moves.
  ("foo|" "l" "foo|" :error "End of buffer")
  ("|foo" "h" "|foo" :error "Beginning of buffer"))

(hel-test-table motion-line
  ("ab|c\ndef" "j" "abc\nde|f")
  ("abc\nde|f" "k" "ab|c\ndef")
  ("ab|c\ndef" "<down>" "abc\nde|f")
  ("abc\nde|f" "<up>" "ab|c\ndef")
  ("a|\nb\nc\nd" "3j" "a\nb\nc\nd|")
  ("a\nb\nc\nd|" "3k" "a|\nb\nc\nd")
  ;; Vertical motion remembers the goal column over a short line.
  ("abcde|f\nab\nabcdef" "jj" "abcdef\nab\nabcde|f")
  ("abcde|f\nab\nabcdef" "j" "abcdef\nab|\nabcdef")
  ;; A line motion drops the selection.
  ("^abc|\ndef" "j" "abc\ndef|")
  ;; After `gl' point sticks to the ends of lines.
  ("a|b\nabcd\nabc" "glj" "ab\nabcd|\nabc")
  ("a|b\nabcd\nabc" "gljj" "ab\nabcd\nabc|")
  ;; There is no line to go to.
  ("abc\nde|f" "j" "abc\ndef|" :error "End of buffer")
  ("a|bc\ndef" "k" "|abc\ndef" :error "Beginning of buffer"))

;;; Motions: words

(hel-test-table motion-w
  ("|foo bar" "w" "^foo |bar")
  ("foo |bar baz" "w" "foo ^bar |baz")
  ("f|oo bar" "w" "f^oo |bar")
  ;; From spaces the motion selects only the spaces.
  ("foo|  bar" "w" "foo^  |bar")
  ;; A word ends at punctuation.
  ("|foo.bar" "w" "^foo|.bar")
  ("foo|.bar" "w" "foo^.|bar")
  ("|foo-bar baz" "w" "^foo|-bar baz")
  ;; The motion starts on the next line when point is at the end of a line.
  ("foo|\nbar baz" "w" "foo\n^bar |baz")
  ("foo|\n\nbar baz" "w" "foo\n\n^bar |baz")
  ;; The last word of a line stops at the end of the line.
  ("foo |bar\nbaz" "w" "foo ^bar|\nbaz")
  ;; At the end of the buffer there is nothing to select.
  ("foo |bar" "w" "foo ^bar|")
  ("foo bar|" "w" "foo bar|")
  ;; Count.
  ("|a b c d e" "3w" "^a b c |d e")
  ("|a b c d e" "2w" "^a b |c d e")
  ;; A second motion starts a new selection from the end of the first.
  ("|foo bar baz" "ww" "foo ^bar |baz")
  ("|foo bar baz" "www" "foo bar ^baz|"))

(hel-test-table motion-W
  ("|foo-bar baz" "W" "^foo-bar |baz")
  ("foo|-bar baz" "W" "foo^-bar |baz")
  ("|a.b c.d e.f" "2W" "^a.b c.d |e.f")
  ("foo|  bar" "W" "foo^  |bar")
  ("|foo-bar" "W" "^foo-bar|")
  ("foo|\nbar-baz qux" "W" "foo\n^bar-baz |qux"))

(hel-test-table motion-b
  ("foo bar|" "b" "foo |bar^")
  ("foo ba|r" "b" "foo |ba^r")
  ("foo |bar" "b" "|foo ^bar")
  ("foo.bar|" "b" "foo.|bar^")
  ("foo.|bar" "b" "foo|.^bar")
  ;; The motion starts on the previous line when point is at a line start.
  ("foo bar\n|baz" "b" "foo |bar^\nbaz")
  ("a b c d e|" "3b" "a b |c d e^")
  ("foo bar baz|" "bb" "foo |bar ^baz")
  ("|foo" "b" "|foo"))

(hel-test-table motion-B
  ("a-b c-d|" "B" "a-b |c-d^")
  ("a-b c-|d" "B" "a-b |c-^d")
  ("a-b |c-d" "B" "|a-b ^c-d")
  ("a.b c.d e.f|" "2B" "a.b |c.d e.f^")
  ("a-b\n|c-d" "B" "|a-b^\nc-d"))

(hel-test-table motion-e
  ("|foo bar" "e" "^foo| bar")
  ("f|oo bar" "e" "f^oo| bar")
  ("foo| bar" "e" "foo^ bar|")
  ("|foo.bar" "e" "^foo|.bar")
  ("foo|.bar" "e" "foo^.|bar")
  ("|a b c d" "2e" "^a b| c d")
  ("|foo bar baz" "ee" "foo^ bar| baz")
  ("foo|\nbar baz" "e" "foo\n^bar| baz")
  ("foo bar|" "e" "foo bar|"))

(hel-test-table motion-E
  ("|foo-bar baz" "E" "^foo-bar| baz")
  ("foo-bar| baz" "E" "foo-bar^ baz|")
  ("|a.b c.d e" "2E" "^a.b c.d| e")
  ("foo|\nbar-baz qux" "E" "foo\n^bar-baz| qux"))

;; Word motions in programming modes follow the syntax of the mode.
(hel-test-table motion-word-modes
  ;; A word is a run of characters with word syntax, so `_' ends a word.
  (:mode c-mode "|foo_bar baz;" "w" "^foo|_bar baz;")
  (:mode c-mode "|foo_bar baz;" "W" "^foo_bar |baz;")
  (:mode c-mode "foo_bar |baz;" "w" "foo_bar ^baz|;")
  (:mode python-mode "|foo_bar = 1" "e" "^foo|_bar = 1")
  (:mode python-mode "|foo_bar = 1" "E" "^foo_bar| = 1")
  (:mode emacs-lisp-mode "|foo-bar baz" "w" "^foo|-bar baz")
  (:mode emacs-lisp-mode "|foo-bar baz" "W" "^foo-bar |baz"))

;;; Motions: find a character

(hel-test-table motion-find
  ;; `f' goes after the character, `t' goes before it.
  ("|foo bar" "f b" "^foo b|ar")
  ("|foo bar" "t b" "^foo |bar")
  ("foo bar|" "F o" "fo|o bar^")
  ("foo bar|" "T o" "foo| bar^")
  ;; The search crosses lines.
  ("|foo\nbar" "f r" "^foo\nbar|")
  ("foo\nbar|" "F f" "|foo\nbar^")
  ;; A new motion starts a new selection.
  ("^foo| bar" "f a" "foo^ ba|r")
  ;; No target: nothing is selected.
  ("|foo" "f z" "|foo")
  ("foo|" "F z" "foo|")
  ;; Lower case finds both cases, upper case finds only upper case.
  ("|xBx" "f b" "^xB|x")
  ("|xbxB" "f B" "^xbxB|")
  ;; Special characters are literal.
  ("|a.b" "f ." "^a.|b")
  ("|a*b" "t *" "^a|*b")
  ("|foo\nbar" "f RET" "^foo\n|bar")
  ;; `n' and `N' repeat the motion while the hints are active, and the
  ;; selection keeps its start.
  ("|a b a b a" "f a f a" "a^ b a| b a")
  ("|xa b a b a" "f a n" "^xa b a| b a")
  ("|xa b a b a" "f a n n" "^xa b a b a|")
  ("|xa b a b a" "f a n N" "^xa| b a b a")
  ("|xa b a b ax" "t a n" "^xa b |a b ax")
  ("xa b a b ax|" "F a n" "xa b |a b ax^")
  ("xa b a b ax|" "T a n" "xa b a| b ax^")
  ;; `v' extends the selection with a find motion.
  ("|foo bar" "v f b" "^foo b|ar")
  ("|foo bar" "w v f a" "^foo ba|r"))

;;; Motions: line ends and buffer ends

(hel-test-table motion-line-ends
  ("  foo b|ar\nbaz" "gh" "|  foo b^ar\nbaz")
  ("  foo b|ar\nbaz" "gl" "  foo b^ar|\nbaz")
  ("  foo b|ar\nbaz" "gs" "  |foo b^ar\nbaz")
  ("|  foo" "gs" "^  |foo")
  ;; Already at the target there is no selection.
  ("|foo" "gh" "|foo")
  ("foo|" "gl" "foo|")
  ;; `v' keeps the start of the selection.
  ("fo|o bar" "v l gl" "fo^o bar|")
  ("foo b|ar" "v h gh" "|foo b^ar"))

(hel-test-table motion-buffer-ends
  ("a\nb|\nc" "gg" "|a\nb\nc")
  ("a\nb|\nc" "ge" "a\nb\nc|")
  ("a\nb|\nc" "G" "a\nb\nc|")
  ;; The motions drop the selection.
  ("a\n^b|\nc" "gg" "|a\nb\nc")
  ("a\n^b|\nc" "G" "a\nb\nc|")
  ;; With `v' the selection extends.
  ("a\nb|\nc" "vG" "a\nb^\nc|")
  ("a\nb|\nc" "vgg" "|a\nb^\nc")
  ;; `G' with a number goes to that line.
  ("|a\nb\nc\nd" "3G" "a\nb\n|c\nd")
  ("a\nb\nc|\nd" "1G" "|a\nb\nc\nd")
  ("|a\nb\nc\nd" "3ge" "a\nb\n|c\nd")
  ;; `gg' with a number goes N/10 of the way, then to the start of the next line.
  ("|0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n" "5gg" "0\n1\n2\n3\n4\n5\n|6\n7\n8\n9\n"))

;;; Motions: paragraphs

(hel-test-table motion-paragraph
  ("|aa\nbb\n\ncc\ndd\n" "}" "^aa\nbb\n\n|cc\ndd\n")
  ("a|a\nbb\n\ncc\ndd\n" "}" "a^a\nbb\n\n|cc\ndd\n")
  ("aa\nbb\n\ncc\nd|d\n" "{" "aa\nbb\n\n|cc\nd^d\n")
  ("aa\nbb\n\n|cc\ndd\n" "{" "|aa\nbb\n\n^cc\ndd\n")
  ("|a\n\nb\n\nc\n" "2}" "^a\n\nb\n\n|c\n")
  ("a\n\nb\n\nc|\n" "2{" "a\n\n|b\n\nc^\n")
  ;; `]p' and `[p' run the same commands as `}' and `{'.
  ("|aa\nbb\n\ncc\ndd\n" "]p" "^aa\nbb\n\n|cc\ndd\n")
  ("aa\nbb\n\ncc\nd|d\n" "[p" "aa\nbb\n\n|cc\nd^d\n")
  ;; `v' keeps the start of the selection.
  ("|a\n\nb\n\nc\n" "v}}" "^a\n\nb\n\n|c\n")
  ;; At the edge of the buffer the cursor stays.
  ("aa\nbb\n|" "}" "aa\nbb\n|")
  ("|aa\nbb\n" "{" "|aa\nbb\n"))

;;; Motions: functions and sentences

(hel-test-table motion-function
  :mode emacs-lisp-mode
  ;; The selection ends after the newline that closes the function.
  ("(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
   "]f" "(defun a ()\n  ^1)\n|\n(defun b ()\n  2)\n")
  ("(defun a ()\n  1)\n\n(defun b ()\n  |2)\n"
   "[f" "(defun a ()\n  1)\n\n|(defun b ()\n  ^2)\n")
  ("|\n(defun a ()\n  1)\n" "]f" "\n^(defun a ()\n  1)\n|")
  ("(defun a ()\n  1)|\n\n(defun b ()\n  2)\n"
   "]f" "(defun a ()\n  1)^\n|\n(defun b ()\n  2)\n")
  (:mode c-mode "int foo(void)\n{\n  |return 1;\n}\n\nint bar(void)\n{\n  return 2;\n}\n"
         "]f" "int foo(void)\n{\n  ^return 1;\n}\n|\nint bar(void)\n{\n  return 2;\n}\n")
  (:mode python-mode "def a():\n    |return 1\n\n\ndef b():\n    return 2\n"
         "]f" "def a():\n    ^return 1\n|\n\ndef b():\n    return 2\n"))

(hel-test-table motion-sentence
  ("|One two.  Three four.  Five." "]s" "^One two.|  Three four.  Five.")
  ("One |two.  Three four.  Five." "]s" "One ^two.|  Three four.  Five.")
  ("One two.  Three |four.  Five." "[s" "One two.  |Three ^four.  Five.")
  ("|One two.  Three four.  Five." "2]s" "^One two.  Three four.|  Five.")
  ("|One two.  Three four.  Five." "].]." "One two.  ^Three four.|  Five.")
  ("One two.  Three |four.  Five." "[." "One two.  |Three ^four.  Five."))

(hel-test-table motion-page
  ("|a\n\f\nb\n\f\nc" "]l" "a\n\f|\nb\n\f\nc")
  ("a\n\f\nb\n\f\n|c" "[l" "a\n\f\nb\n\f|\nc"))

;; With one cursor `(' and `)' go over sentences in text modes and over
;; functions in programming modes.
(hel-test-table motion-paren
  ("|One two.  Three four.  Five." ")" "^One two.|  Three four.  Five.")
  ("One two.  Three |four.  Five." "(" "One two.  |Three ^four.  Five.")
  (:mode emacs-lisp-mode "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
         ")" "(defun a ()\n  ^1)\n|\n(defun b ()\n  2)\n")
  (:mode emacs-lisp-mode "(defun a ()\n  1)\n\n(defun b ()\n  |2)\n"
         "(" "(defun a ()\n  1)\n\n|(defun b ()\n  ^2)\n")
  (:mode c-mode "int a(void)\n{\n  |return 1;\n}\n"
         ")" "int a(void)\n{\n  ^return 1;\n}\n|")
  (:mode python-mode "def a():\n    |return 1\n"
         ")" "def a():\n    ^return 1\n|"))

;;; Motions: hints

;; The hints use the keys of `avy-keys'.  The first candidate is on `a', the
;; second is on `s'.
(hel-test-table motion-hint
  ("|foo bar baz" "gwa" "^foo| bar baz")
  ("|foo bar baz" "gws" "foo ^bar| baz")
  ("foo bar baz|" "gba" "foo bar ^baz|")
  ("foo bar baz|" "gbs" "foo ^bar| baz")
  ("|foo-a bar-b baz" "gWs" "foo-a ^bar-b| baz")
  ("foo-a bar-b baz|" "gBs" "foo-a ^bar-b| baz")
  ("|a\nb\nc\nd" "gja" "a\n|b\nc\nd")
  ("|a\nb\nc\nd" "gjs" "a\nb\n|c\nd")
  ("a\nb\nc\n|d" "gka" "a\nb\n|c\nd"))

;;; The minibuffer

(hel-test-table minibuffer
  ;; `:' runs a command by name, for all cursors too.
  ("|foo" ":forward-char RET" "f|oo")
  ;; In the minibuffer `Esc' goes to Normal state, and Hel keys edit the input.
  ("|foo bar foo" "/fooo<escape>dRET" "^foo| bar foo")
  ;; The second `Esc' cancels the search.
  ("|foo bar" "/bar<escape><escape>" "|foo bar"))

;;; Folds

(defmacro hel-test-with-fold (&rest body)
  "Evaluate BODY in an `outline-minor-mode' buffer with a closed first section."
  `(hel-test-with 'text-mode "|* a\nbody\nmore\n* b\nbody b\n"
     (outline-minor-mode 1)
     (outline-hide-subtree)
     ,@body))

(ert-deftest hel-behaviour-fold-line-selection-takes-the-fold ()
  "`x' on a closed section selects the whole section."
  (hel-test-with-fold
   (hel-test-expect "x" "|* a\nbody\nmore\n^* b\nbody b\n")))

(ert-deftest hel-behaviour-fold-line-motion-skips-the-fold ()
  "`j' goes over a closed section to the next visible line."
  (hel-test-with-fold
   (hel-test-press! "j")
   (should (= (line-number-at-pos) 4))))

(ert-deftest hel-behaviour-fold-search-opens-the-fold ()
  "A search match in a closed section opens the section."
  (hel-test-with-fold
   (should (invisible-p (+ (point-min) 8)))
   (hel-test-expect "/more RET" "* a\nbody\n^more|\n* b\nbody b\n")
   (should-not (invisible-p (+ (point-min) 8)))))

(ert-deftest hel-behaviour-fold-next-opens-the-fold ()
  "`n' to a match in a closed section opens the section."
  (hel-test-with 'text-mode "^more|\n* a\nbody\nmore\n* b\n"
    (outline-minor-mode 1)
    (hel-test-press! "*")
    (goto-char 6)
    (outline-hide-subtree)
    (should (invisible-p 15))
    (hel-test-press! "<escape>n")
    (should (equal (hel-test-render) "more\n* a\nbody\n^more|\n* b\n"))
    (should-not (invisible-p 15))))

;;; Selections

(hel-test-table select-extend
  ;; `v' makes motions extend the selection.
  ("|foo bar baz" "vw" "^foo |bar baz")
  ("|foo bar baz" "vww" "^foo bar |baz")
  ("|foo bar" "vll" "^fo|o bar")
  ("foo bar|" "vb" "foo |bar^")
  ("foo bar|" "vbb" "|foo bar^")
  ("|a\nb\nc" "vj" "^a\n|b\nc")
  ;; A second `v' stops extending and keeps the selection.
  ("|foo bar baz" "vwvw" "foo ^bar |baz")
  ("|foobar" "vllvl" "foo|bar")
  ;; `v' keeps an existing selection and extends it.
  ("^foo| bar" "vl" "^foo |bar")
  ("|foo bar^" "vl" "f|oo bar^")
  ;; Motions shrink the selection when they go toward its other end.
  ("|foobar" "vlllh" "^fo|obar"))

(hel-test-table select-escape
  ("^foo| bar" "<escape>" "foo| bar")
  ("|foo bar" "<escape>" "|foo bar")
  ;; The first `Esc' stops extending and keeps the selection, the second one
  ;; drops the selection.
  ("|foo bar" "vll<escape>" "^fo|o bar")
  ("|foo bar" "vll<escape><escape>" "fo|o bar")
  ("|foo bar baz" "vll<escape>w" "fo^o |bar baz"))

(hel-test-table select-collapse
  ("^foo| bar" ";" "foo| bar")
  ("|foo^ bar" ";" "|foo bar")
  ("foo| bar" ";" "foo| bar")
  ;; With `v' the selection collapses and motions still extend.
  ("|foo bar baz" "vw;" "foo |bar baz")
  ("|foo bar baz" "vw;w" "foo ^bar |baz"))

(hel-test-table select-exchange
  ("^foo| bar" "M-;" "|foo^ bar")
  ("|foo^ bar" "M-;" "^foo| bar")
  ("^foo| bar" "C-;" "|foo^ bar")
  ("^foo| bar" "g;" "|foo^ bar")
  ("foo| bar" "g;" "foo| bar")
  ;; The side of the cursor decides which end of the selection `v' moves.
  ("^foo bar| baz" "M-;vl" "f|oo bar^ baz"))

(hel-test-table select-line
  ;; A one-line selection has the cursor at its start.
  ("fo|o\nbar\nbaz\n" "x" "|foo\n^bar\nbaz\n")
  ;; More lines put the cursor at the end, and each `x' adds a line.
  ("fo|o\nbar\nbaz\n" "xx" "^foo\nbar\n|baz\n")
  ("fo|o\nbar\nbaz\n" "xxx" "^foo\nbar\nbaz\n|")
  ;; `x' on the last line selects up to the end of the buffer.
  ("foo\nb|ar\n" "x" "foo\n|bar\n^")
  ;; A last line without a newline gets one.
  ("foo\nb|ar" "x" "foo\n|bar\n^")
  ;; A count selects that many lines.
  ("|a\nb\nc\nd\n" "2x" "^a\nb\n|c\nd\n")
  ("|a\nb\nc\nd\n" "3x" "^a\nb\nc\n|d\n")
  ;; A partial selection grows to full lines.
  ("f^oo\nb|ar\nbaz\n" "x" "^foo\nbar\n|baz\n")
  ("f|oo\nb^ar\nbaz\n" "x" "^foo\nbar\n|baz\n")
  ;; `X' contracts the selection from the end of the cursor.
  ("|a\nb\nc\n" "xxX" "|a\n^b\nc\n")
  ("|a\nb\nc\nd\n" "xxxX" "^a\nb\n|c\nd\n")
  ;; With the cursor at the start `X' extends up and `x' contracts.
  ("a\nb\n|c\nd\n" "X" "a\nb\n|c\n^d\n")
  ("a\nb\n|c\nd\n" "XX" "a\n|b\nc\n^d\n")
  ("a\nb\n|c\nd\n" "XXX" "|a\nb\nc\n^d\n")
  ("a\nb\n|c\nd\n" "XXx" "a\nb\n|c\n^d\n")
  ;; Line selection keeps its ends through a `v'.
  ("|a\nb\nc\nd\n" "xxvj" "^a\nb\nc\n|d\n")
  ("|a\nb\nc\nd\n" "xxvjk" "^a\nb\n|c\nd\n"))

(hel-test-table select-whole-buffer
  ("fo|o\nbar\n" "%" "|foo\nbar\n^")
  ("^fo|o\nbar\n" "%" "|foo\nbar\n^")
  ("|" "%" "|"))

(hel-test-table select-trim
  ("^  foo  |bar" "_" "  ^foo|  bar")
  ("|  foo  ^bar" "_" "  |foo^  bar")
  ("^\n foo \n|x" "_" "\n ^foo| \nx")
  ("foo| bar" "_" "foo| bar"))

;;; Editing: cut, delete, change

(hel-test-table edit-cut
  ("^foo| bar" "d" "| bar" :kill "foo")
  ("|foo^ bar" "d" "| bar" :kill "foo")
  ;; Without a selection `d' deletes before point and keeps the kill ring.
  ("foo| bar" "d" "fo| bar" :yank "kept" :kill "kept")
  ("foo| bar" "2d" "f| bar" :yank "kept" :kill "kept")
  ("|foo" "d" "|foo" :error "Beginning of buffer")
  ;; Linewise.
  ("fo|o\nbar\n" "xd" "|bar\n" :kill "foo\n")
  ("|a\nb\nc\n" "xxd" "|c\n" :kill "a\nb\n"))

(hel-test-table edit-delete
  ("^foo| bar" "D" "| bar" :yank "kept" :kill "kept")
  ("|foo^ bar" "D" "| bar" :yank "kept" :kill "kept")
  ;; Without a selection `D' deletes after point.
  ("foo| bar" "D" "foo|bar" :yank "kept" :kill "kept")
  ("|foobar" "3D" "|bar")
  ("foo|" "D" "foo|" :error "End of buffer")
  ;; `DEL' does nothing in Normal state, because `d' deletes.
  ("foo|" "DEL" "foo|")
  ("^foo| bar" "DEL" "^foo| bar"))

(hel-test-table edit-change
  ("^foo| bar" "c" "| bar" :state insert :kill "foo")
  ("|foo^ bar" "c" "| bar" :state insert :kill "foo")
  ;; The typed text becomes the selection on leaving Insert state.
  ("^foo| bar" "cbaz<escape>" "^baz| bar")
  ;; Without a selection `c' deletes the character before point.
  ("foo| bar" "c" "fo| bar" :state insert)
  ("|foo" "c" "|foo" :state insert)
  ;; A linewise selection leaves an empty line.
  ("fo|o\nbar\n" "xc" "|\nbar\n" :state insert :kill "foo\n")
  ;; On an empty line `c' inserts the indentation.
  (:mode c-mode "{\n|\n}\n" "c" "{\n  |\n}\n" :state insert)
  (:mode python-mode "def a():\n|\n" "c" "def a():\n    |\n" :state insert)
  ;; A linewise change keeps the indentation of the mode.
  (:mode c-mode "{\n  fo|o;\n}\n" "xc" "{\n  |\n}\n" :state insert))

;;; Editing: copy and paste

(hel-test-table edit-copy
  ("^foo| bar" "y" "^foo| bar" :kill "foo")
  ("|foo^ bar" "y" "|foo^ bar" :kill "foo")
  ;; No selection: nothing is copied.
  ("foo| bar" "y" "foo| bar" :yank "kept" :kill "kept")
  ("fo|o\nbar\n" "xy" "|foo\n^bar\n" :kill "foo\n")
  ;; `y' stops extending.
  ("|foo bar" "vlly" "^fo|o bar" :kill "fo")
  ("|foo bar" "vllyl" "foo| bar" :kill "fo")
  ;; With the universal argument the selection goes to the last kill.
  ("^foo| bar" "M-uy" "^foo| bar" :yank "first" :kill "first foo"))

(hel-test-table edit-paste
  ;; The pasted text becomes the selection.
  ("foo| bar" "p" "foo^xx| bar" :yank "xx")
  ("foo| bar" "P" "foo^xx| bar" :yank "xx")
  ("^foo| bar" "p" "foo^xx| bar" :yank "xx")
  ("^foo| bar" "P" "^xx|foo bar" :yank "xx")
  ;; The selection direction is kept.
  ("|foo^ bar" "p" "foo|xx^ bar" :yank "xx")
  ("|foo^ bar" "P" "|xx^foo bar" :yank "xx")
  ;; A text with a newline loses it on a selection that is not linewise.
  ("^foo| bar" "p" "foo^xx| bar" :yank "xx\n")
  ;; A linewise selection pastes on separate lines.
  ("fo|o\nbar\n" "xp" "foo\n|xx\n^bar\n" :yank "xx\n")
  ("fo|o\nbar\n" "xP" "|xx\n^foo\nbar\n" :yank "xx\n")
  ("fo|o\nbar\n" "xp" "foo\n|xx\n^bar\n" :yank "xx")
  ("foo\nb|ar" "xp" "foo\nbar\n|xx\n^" :yank "xx")
  ;; Copy and paste together.
  ("^foo| bar" "yp" "foo^foo| bar")
  ("fo|o\nbar\n" "xyp" "foo\n|foo\n^bar\n")
  ;; Without a selection a text with a newline loses it.
  ("fo|o\nbar\n" "xdp" "^foo|bar\n" :kill "foo\n")
  ;; Paste in programming modes indents the pasted lines.
  (:mode c-mode "{\n  a;|\n}\n" "p" "{\n  a;^xx|\n}\n" :yank "xx")
  (:mode c-mode "{\n|  a;\n}\n" "xp" "{\n  a;\n|  xx\n^}\n" :yank "xx\n"))

(ert-deftest hel-behaviour-edit-paste-pop ()
  "`C-p' and `C-n' replace the pasted text with other kills."
  (hel-test-with 'text-mode "foo| bar"
    (kill-new "one")
    (kill-new "two")
    (kill-new "three")
    (hel-test-expect "p" "foo^three| bar")
    (hel-test-expect "C-p" "foo^two| bar")
    (hel-test-expect "C-p" "foo^one| bar")
    (hel-test-expect "C-n" "foo^two| bar")))

(hel-test-table edit-replace-char
  ;; With a selection `r' replaces each character.
  ("^foo| bar" "rx" "^xxx| bar")
  ("|foo^ bar" "rx" "|xxx^ bar")
  ("^a\nb|c" "rx" "^x\nx|c")
  ;; Without a selection `r' works before point and `R' after point.
  ("foo| bar" "rx" "fox| bar")
  ("foo| bar" "2rx" "fxx| bar")
  ("foo| bar" "Rx" "foo|xbar")
  ("foo| bar" "2Rx" "foo|xxar")
  ;; A count larger than the line stops at the buffer end.
  ("fo|o" "5Rx" "fo|x")
  ("fo|o" "5rx" "xx|o"))

(hel-test-table edit-replace-with-kill
  ("^foo| bar" "R" "^xx| bar" :yank "xx" :kill "xx")
  ("|foo^ bar" "R" "|xx^ bar" :yank "xx" :kill "xx")
  ("fo|o\nbar\n" "xR" "|xx\n^bar\n" :yank "xx"))

;;; Editing: lines

(hel-test-table edit-join
  ("fo|o\n  bar\nbaz" "J" "fo|o bar\nbaz")
  ("foo|\nbar" "J" "foo |bar")
  ;; A selection joins all of its lines.
  ("^a\nb\nc\n|d" "J" "^a b c\n|d")
  ("^a\nb\nc|\nd" "J" "^a b c|\nd")
  ;; Comment starters of the joined lines are removed.
  (:mode emacs-lisp-mode "|;; a\n;; b\n" "J" "|;; a b\n")
  (:mode python-mode "|# a\n# b\n" "J" "|# a b\n")
  (:mode c-mode "|// a\n// b\n" "J" "|// a b\n")
  ;; A line of code that is not a comment keeps its next line as it is.
  (:mode emacs-lisp-mode "|(a)\n  ;; b\n" "J" "|(a) ;; b\n"))

(hel-test-table edit-open
  ("fo|o\nbar" "o" "foo\n|\nbar" :state insert)
  ("foo\nb|ar" "O" "foo\n|\nbar" :state insert)
  ("foo|" "o" "foo\n|" :state insert)
  ("|foo" "O" "|\nfoo" :state insert)
  ("fo|o\nbar" "ox<escape>" "foo\nx|\nbar")
  ;; A selection: the new line goes outside of its end.
  ("^a\nb|\nc" "o" "a\nb\n|\nc" :state insert)
  ("^a\nb|\nc" "O" "|\na\nb\nc" :state insert)
  ;; A line selection: the same.
  ("|a\nb\nc\n" "xo" "a\n|\nb\nc\n" :state insert)
  ("|a\nb\nc\n" "xxo" "a\nb\n|\nc\n" :state insert)
  ("a\n|b\nc\n" "xO" "a\n|\nb\nc\n" :state insert)
  ;; The new line has the indentation of the mode.
  (:mode c-mode "{\n  fo|o;\n}\n" "o" "{\n  foo;\n  |\n}\n" :state insert)
  (:mode c-mode "{\n  fo|o;\n}\n" "O" "{\n  |\n  foo;\n}\n" :state insert)
  (:mode python-mode "def a():|\n" "o" "def a():\n    |\n" :state insert)
  (:mode emacs-lisp-mode "(foo|\n bar)\n" "o" "(foo\n |\n bar)\n" :state insert))

(hel-test-table edit-blank-line
  ("fo|o\nbar" "]SPC" "fo|o\n\nbar")
  ("fo|o\nbar" "[SPC" "\nfo|o\nbar")
  ("fo|o\nbar" "2]SPC" "fo|o\n\n\nbar")
  ("fo|o\nbar" "2[SPC" "\n\nfo|o\nbar")
  ;; The selection keeps its text.
  ("^foo| bar\nbaz" "]SPC" "^foo| bar\n\nbaz")
  ("^foo| bar\nbaz" "[SPC" "\n^foo| bar\nbaz"))

(hel-test-table edit-indent
  ;; Without a selection the current line moves by one column.
  ("fo|o" ">" " fo|o")
  ("  fo|o" "<" " fo|o")
  ("fo|o" "3>" "   fo|o")
  ("   fo|o" "2<" " fo|o")
  ;; A partial selection moves all of its lines and stays on its text.
  ("f^oo\nb|ar\nbaz" ">" " f^oo\n b|ar\nbaz")
  ("  f^oo\n  b|ar\nbaz" "<" " f^oo\n b|ar\nbaz")
  ;; A line selection stays linewise.
  ("^a\nb\n|c" ">" "^ a\n b\n|c")
  ("^ a\n b\n|c" "<" "^a\nb\n|c")
  (:mode c-mode "fo|o" ">" " fo|o"))

(hel-test-table edit-reindent
  (:mode c-mode "^{\nfoo;\n}\n|" "=" "^{\n  foo;\n}\n|")
  (:mode emacs-lisp-mode "^(foo\nbar)\n|" "=" "^(foo\n bar)\n|")
  (:mode python-mode "^if a:\nb\n|" "=" "^if a:\n    b\n|"))

(hel-test-table edit-comment
  (:mode emacs-lisp-mode "^foo\n|bar\n" "gc" "^;; foo\n|bar\n")
  (:mode c-mode "^foo\n|bar\n" "gc" "^/* foo */\n|bar\n")
  (:mode python-mode "^foo\n|bar\n" "gc" "^# foo\n|bar\n")
  ;; A second press removes the comment.
  (:mode emacs-lisp-mode "^foo\n|bar\n" "gcgc" "^foo\n|bar\n")
  (:mode python-mode "^foo\n|bar\n" "gcgc" "^foo\n|bar\n")
  ;; Without a selection a comment is added at the end of the line.
  (:mode python-mode "fo|o\n" "gc" "foo                             # |\n"))

(hel-test-table edit-case
  ("^Foo BAR| baz" "`" "^foo bar| baz")
  ("^Foo bar| baz" "gu" "^foo bar| baz")
  ("^Foo bar| baz" "M-`" "^FOO BAR| baz")
  ("^Foo bar| baz" "gU" "^FOO BAR| baz")
  ("^Foo bAr| baz" "~" "^fOO BaR| baz")
  ("|Foo^ bar" "~" "|fOO^ bar")
  ;; Without a selection the cursor stays and the count goes after it.
  ("|foo" "gU" "|Foo")
  ("|foo" "2gU" "|FOo")
  ("|FOO" "gu" "|fOO")
  ("|FOO" "3`" "|foo")
  ;; `~' without a selection moves over the character, as it does in Vim.
  ("|foo" "~" "F|oo")
  ("|foo" "~~" "FO|o"))

(hel-test-table edit-fill
  ("|aaa bbb\nccc\n" "gq" "|aaa bbb ccc\n")
  ("^aaa\nbbb\n\nccc\nddd\n|" "gq" "^aaa bbb\n\nccc ddd\n|")
  ("^aaa\nbbb\n\nccc\n|" "gQ" "^aaa bbb ccc\n|"))

(ert-deftest hel-behaviour-edit-repeat ()
  "`.' repeats the last command."
  (hel-test-with 'text-mode "|abcd"
    (hel-test-expect "D." "|cd")))

;;; Undo and redo

(hel-test-table undo
  ("^foo| bar" "du" "^foo| bar")
  ("^foo| bar" "duU" "| bar")
  ("|foo" "Du" "|foo")
  ("|foo" "DDDuu" "|oo")
  ("|foo" "DDDuuU" "|o")
  ;; `D' twice and `u' twice bring back all the text.
  ("|ab" "DDuu" "|ab")
  ;; There is nothing to undo.
  ("|foo" "u" "|foo" :error "No further undo")
  ;; Undo works over the whole buffer, and not in the selection.
  ("^foo| bar" "D%u" "^foo| bar")
  ;; An insert session with one word is one step.
  ("|foo" "ibar<escape>u" "|foo")
  ;; Words typed in Insert state are separate steps.
  ("|" "ifoo SPC bar SPC baz<escape>u" "foo bar|")
  ("|" "ifoo SPC bar SPC baz<escape>uu" "foo|"))

;;; Insert state

(hel-test-table insert
  ("foo| bar" "i" "foo| bar" :state insert)
  ("foo| bar" "a" "foo| bar" :state insert)
  ("foo| bar" "ix" "foox| bar" :state insert)
  ("foo| bar" "ax<escape>" "foox| bar")
  ;; `i' goes to the start of the selection, `a' to its end.
  ("^foo| bar" "i" "|foo bar" :state insert)
  ("|foo^ bar" "a" "foo| bar" :state insert)
  ;; The selection comes back on leaving Insert state, with the typed text
  ;; on the side of the cursor.
  ("^foo| bar" "ix<escape>" "x|foo^ bar")
  ("^foo| bar" "ax<escape>" "^foox| bar")
  ("|foo^ bar" "ax<escape>" "^foox| bar")
  ;; `I' and `A' go to the ends of the line.
  ("  fo|o bar\nbaz" "I" "  |foo bar\nbaz" :state insert)
  ("  fo|o bar\nbaz" "A" "  foo bar|\nbaz" :state insert)
  ("  ^foo| bar\nbaz" "I" "  |foo bar\nbaz" :state insert)
  ("  ^foo| bar\nbaz" "A" "  foo bar|\nbaz" :state insert)
  ("fo|o" "Ix<escape>" "xfoo" :text-only t)
  ("fo|o" "Ax<escape>" "foox|")
  ;; Keys insert their own text in Insert state.
  ("|" "ihjkl SPC wbe" "hjkl wbe|" :state insert)
  ("fo|o" "iRET" "fo\n|o" :state insert)
  ("foo|" "iDEL" "fo|" :state insert)
  ;; Emacs keys work in Insert state.
  ("fo|o" "iC-a" "|foo" :state insert)
  ("fo|o" "iC-e" "foo|" :state insert)
  ("foo bar|" "iC-w" "foo |" :state insert)
  ;; A line in a programming mode keeps its indentation.
  (:mode c-mode "{\n  fo|o;\n}\n" "I" "{\n  |foo;\n}\n" :state insert)
  (:mode python-mode "def a():\n    fo|o\n" "I" "def a():\n    |foo\n" :state insert))

;;; Text objects: words

(hel-test-table object-word
  ("foo b|ar baz" "miw" "foo ^bar| baz")
  ("foo b|ar baz" "mw" "foo ^bar| baz")
  ("foo |bar baz" "miw" "foo ^bar| baz")
  ("foo bar| baz" "miw" "foo ^bar| baz")
  ("foo b|ar baz" "maw" "foo ^bar |baz")
  ;; At the end of a line `maw' takes the space before the word.
  ("foo b|ar" "maw" "foo^ bar|")
  ("foo b|ar\nbaz" "maw" "foo^ bar|\nbaz")
  ("f|oo" "maw" "^foo|")
  ;; Punctuation is a separate word.
  ("foo.b|ar.baz" "miw" "foo.^bar|.baz")
  ("foo-b|ar baz" "miw" "foo-^bar| baz")
  ;; On the blanks after a word `miw' takes that word.
  ("foo |  bar" "miw" "^foo|   bar")
  ;; A selected character right after a comment finds that comment, as
  ;; a cursor without a selection does.
  (:mode emacs-lisp-mode ";; note\n|foo" "mic" "|;; note\n^foo")
  (:mode emacs-lisp-mode ";; note\n^f|oo" "mic" "|;; note\n^foo")
  ;; A count adds the next words.
  ("foo b|ar baz qux" "2miw" "foo ^bar baz| qux")
  ("foo b|ar baz qux" "m2iw" "foo ^bar baz| qux")
  ("foo b|ar baz qux" "mi2w" "foo ^bar baz| qux")
  ("foo b|ar baz qux" "2mw" "foo ^bar baz| qux")
  ("foo b|ar baz qux" "m2w" "foo ^bar baz| qux")
  ;; A negative count goes backward.
  ("foo b|ar baz qux" "-2miw" "|foo bar^ baz qux")
  ;; A word in programming modes.
  (:mode c-mode "int fo|o_bar;" "miw" "int ^foo|_bar;")
  (:mode c-mode "int fo|o_bar;" "miW" "int ^foo_bar|;")
  (:mode python-mode "fo|o = 1" "maw" "^foo |= 1"))

(hel-test-table object-WORD
  ("a-b c|-d e-f" "miW" "a-b ^c-d| e-f")
  ("a-b c|-d e-f" "mW" "a-b ^c-d| e-f")
  ("a-b c|-d e-f" "maW" "a-b ^c-d |e-f")
  ("a-b c|-d" "maW" "a-b^ c-d|")
  ("a-b c|-d e-f g-h" "2miW" "a-b ^c-d e-f| g-h"))

;;; Text objects: sentences

(hel-test-table object-sentence
  ("One two.  Th|ree four.  Five." "mis" "One two.  ^Three four.|  Five.")
  ("One two.  Th|ree four.  Five." "m." "One two.  ^Three four.|  Five.")
  ("One two.  Th|ree four.  Five." "mi." "One two.  ^Three four.|  Five.")
  ("One two.  Th|ree four.  Five." "mas" "One two.  ^Three four.  |Five.")
  ("One two.  Th|ree four.  Five." "ma." "One two.  ^Three four.  |Five.")
  ;; The last sentence takes the space before it.
  ("One two.  Three fo|ur." "mas" "One two.^  Three four.|")
  ("One two.  Th|ree four.  Five.  Six." "2mis" "One two.  ^Three four.  Five.|  Six."))

;;; Text objects: paragraphs

(hel-test-table object-paragraph
  ("aa\nb|b\n\ncc\n" "mip" "|aa\nbb\n^\ncc\n")
  ("aa\nb|b\n\ncc\n" "mp" "|aa\nbb\n^\ncc\n")
  ("aa\nb|b\n\ncc\n" "map" "|aa\nbb\n\n^cc\n")
  ;; The last paragraph takes the blank lines before it.
  ("aa\n\ncc\nd|d\n" "map" "aa\n|\ncc\ndd\n^")
  ("aa\nb|b\n\ncc\n\ndd\n" "2mip" "^aa\nbb\n\ncc\n|\ndd\n")
  ("aa\nb|b\n\ncc\n\ndd\n" "m2ip" "^aa\nbb\n\ncc\n|\ndd\n")
  ("aa\nb|b\n\ncc\n\ndd\n" "mi2p" "^aa\nbb\n\ncc\n|\ndd\n")
  ("aa\nb|b\n\ncc\n\ndd\n" "2map" "^aa\nbb\n\ncc\n\n|dd\n")
  (:mode c-mode "int a;\nin|t b;\n\nint c;\n" "mip" "|int a;\nint b;\n^\nint c;\n"))

;;; Text objects: functions

(hel-test-table object-function
  ;; `mif' puts the cursor at the start of the function.
  (:mode emacs-lisp-mode "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
         "mif" "|(defun a ()\n  1)\n^\n(defun b ()\n  2)\n")
  (:mode emacs-lisp-mode "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
         "mf" "|(defun a ()\n  1)\n^\n(defun b ()\n  2)\n")
  (:mode emacs-lisp-mode "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
         "maf" "|(defun a ()\n  1)\n\n^(defun b ()\n  2)\n")
  ;; The last function takes the space before it.
  (:mode emacs-lisp-mode "(defun a ()\n  1)\n\n(defun b ()\n  |2)\n"
         "maf" "(defun a ()\n  1)\n|\n(defun b ()\n  2)\n^")
  ;; `maf' takes the comments that belong to the function.
  (:mode emacs-lisp-mode ";; doc a\n(defun a ()\n  |1)\n\n;; doc b\n(defun b ()\n  2)\n"
         "maf" "|;; doc a\n(defun a ()\n  1)\n\n^;; doc b\n(defun b ()\n  2)\n")
  ;; A count.
  (:mode emacs-lisp-mode "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n\n(defun c ()\n  3)\n"
         "2mif" "^(defun a ()\n  1)\n\n(defun b ()\n  2)\n|\n(defun c ()\n  3)\n")
  (:mode c-mode "int a(void)\n{\n  |return 1;\n}\n\nint b(void)\n{\n  return 2;\n}\n"
         "mif" "|int a(void)\n{\n  return 1;\n}\n^\nint b(void)\n{\n  return 2;\n}\n")
  (:mode c-mode "int a(void)\n{\n  |return 1;\n}\n\nint b(void)\n{\n  return 2;\n}\n"
         "maf" "|int a(void)\n{\n  return 1;\n}\n\n^int b(void)\n{\n  return 2;\n}\n")
  (:mode python-mode "def a():\n    |return 1\n\n\ndef b():\n    return 2\n"
         "mif" "|def a():\n    return 1\n^\n\ndef b():\n    return 2\n")
  (:mode python-mode "def a():\n    |return 1\n\n\ndef b():\n    return 2\n"
         "maf" "|def a():\n    return 1\n\n\n^def b():\n    return 2\n"))

;;; Text objects: quotes

(hel-test-table object-quote
  ("foo \"b|ar\" baz" "mi\"" "foo \"^bar|\" baz")
  ("foo \"b|ar\" baz" "m\"" "foo \"^bar|\" baz")
  ("foo \"b|ar\" baz" "ma\"" "foo ^\"bar\"| baz")
  ("foo 'b|ar' baz" "mi'" "foo '^bar|' baz")
  ("foo 'b|ar' baz" "m'" "foo '^bar|' baz")
  ("foo 'b|ar' baz" "ma'" "foo ^'bar'| baz")
  ("foo `b|ar` baz" "mi`" "foo `^bar|` baz")
  ("foo `b|ar` baz" "m`" "foo `^bar|` baz")
  ("foo `b|ar` baz" "ma`" "foo ^`bar`| baz")
  ;; Point at the delimiters.
  ("foo |\"bar\" baz" "mi\"" "foo \"^bar|\" baz")
  ;; After the closing quote point is outside of the quotes.
  ("foo \"bar\"| baz" "mi\"" "foo \"bar\"| baz")
  ;; Nothing to select.
  ("fo|o bar" "mi\"" "fo|o bar")
  ;; Strings in programming modes.
  (:mode c-mode "char *a = \"b|ar\";" "mi\"" "char *a = \"^bar|\";")
  (:mode c-mode "char *a = \"b|ar\";" "ma\"" "char *a = ^\"bar\"|;")
  (:mode python-mode "a = 'b|ar'" "mi'" "a = '^bar|'")
  (:mode python-mode "a = \"b|ar\"" "ma\"" "a = ^\"bar\"|")
  (:mode emacs-lisp-mode "(foo \"b|ar\")" "mi\"" "(foo \"^bar|\")"))

;;; Text objects: brackets

(hel-test-table object-bracket
  ("(foo b|ar) baz" "mi(" "(^foo bar|) baz")
  ("(foo b|ar) baz" "mi)" "(^foo bar|) baz")
  ("(foo b|ar) baz" "m(" "(^foo bar|) baz")
  ("(foo b|ar) baz" "m)" "(^foo bar|) baz")
  ("(foo b|ar) baz" "ma(" "^(foo bar)| baz")
  ("(foo b|ar) baz" "ma)" "^(foo bar)| baz")
  ("[foo b|ar] baz" "mi[" "[^foo bar|] baz")
  ("[foo b|ar] baz" "mi]" "[^foo bar|] baz")
  ("[foo b|ar] baz" "m[" "[^foo bar|] baz")
  ("[foo b|ar] baz" "m]" "[^foo bar|] baz")
  ("[foo b|ar] baz" "ma[" "^[foo bar]| baz")
  ("[foo b|ar] baz" "ma]" "^[foo bar]| baz")
  ("{foo b|ar} baz" "mi{" "{^foo bar|} baz")
  ("{foo b|ar} baz" "mi}" "{^foo bar|} baz")
  ("{foo b|ar} baz" "m{" "{^foo bar|} baz")
  ("{foo b|ar} baz" "m}" "{^foo bar|} baz")
  ("{foo b|ar} baz" "ma{" "^{foo bar}| baz")
  ("{foo b|ar} baz" "ma}" "^{foo bar}| baz")
  ("<foo b|ar> baz" "mi<" "<^foo bar|> baz")
  ("<foo b|ar> baz" "mi>" "<^foo bar|> baz")
  ("<foo b|ar> baz" "m<" "<^foo bar|> baz")
  ("<foo b|ar> baz" "m>" "<^foo bar|> baz")
  ("<foo b|ar> baz" "ma<" "^<foo bar>| baz")
  ("<foo b|ar> baz" "ma>" "^<foo bar>| baz")
  ;; Nested brackets: the innermost pair around point.
  ("(a (b|) c)" "mi(" "(a (^b|) c)")
  ("(a (b) |c)" "mi(" "(^a (b) c|)")
  ("(a [b|] c)" "mi(" "(^a [b] c|)")
  ;; Point next to a bracket.
  ("|(foo)" "mi(" "(^foo|)")
  ("(foo)|" "mi(" "(^foo|)")
  ("|(foo)" "ma(" "^(foo)|")
  ;; Across lines.
  ("(foo\n b|ar\n baz)" "mi(" "(^foo\n bar\n baz|)")
  ;; `mi(' leaves out the spaces next to the brackets.
  ("( fo|o )" "mi(" "( ^foo| )")
  ;; Nothing to select.
  ("fo|o" "mi(" "fo|o")
  ("fo|o" "ma[" "fo|o")
  ;; Brackets in strings do not count outside of the string.
  (:mode c-mode "a(\")\", b|);" "mi(" "a(^\")\", b|);")
  (:mode c-mode "a[fo|o];" "ma[" "a^[foo]|;")
  (:mode c-mode "{ fo|o; }" "ma{" "^{ foo; }|")
  (:mode emacs-lisp-mode "(foo (ba|r))" "ma(" "(foo ^(bar)|)")
  (:mode python-mode "a = [1, |2]" "mi[" "a = [^1, 2|]"))

;;; Text objects: other delimiters

(hel-test-table object-delimiter
  ("x !fo|o! bar" "mi!" "x !^foo|! bar")
  ("x @fo|o@ bar" "mi@" "x @^foo|@ bar")
  ("x #fo|o# bar" "mi#" "x #^foo|# bar")
  ("x $fo|o$ bar" "mi$" "x $^foo|$ bar")
  ("x %fo|o% bar" "mi%" "x %^foo|% bar")
  ("x ^^fo|o^^ bar" "mi^" "x ^^^foo|^^ bar")
  ("x &fo|o& bar" "mi&" "x &^foo|& bar")
  ("x *fo|o* bar" "mi*" "x *^foo|* bar")
  ("x ~fo|o~ bar" "mi~" "x ~^foo|~ bar")
  ("x =fo|o= bar" "mi=" "x =^foo|= bar")
  ("x _fo|o_ bar" "mi_" "x _^foo|_ bar")
  ("x !fo|o! bar" "m!" "x !^foo|! bar")
  ("x @fo|o@ bar" "m@" "x @^foo|@ bar")
  ("x #fo|o# bar" "m#" "x #^foo|# bar")
  ("x $fo|o$ bar" "m$" "x $^foo|$ bar")
  ("x %fo|o% bar" "m%" "x %^foo|% bar")
  ("x ^^fo|o^^ bar" "m^" "x ^^^foo|^^ bar")
  ("x &fo|o& bar" "m&" "x &^foo|& bar")
  ("x *fo|o* bar" "m*" "x *^foo|* bar")
  ("x ~fo|o~ bar" "m~" "x ~^foo|~ bar")
  ("x =fo|o= bar" "m=" "x =^foo|= bar")
  ("x _fo|o_ bar" "m_" "x _^foo|_ bar")
  ("x !fo|o! bar" "ma!" "x ^!foo!| bar")
  ("x @fo|o@ bar" "ma@" "x ^@foo@| bar")
  ("x #fo|o# bar" "ma#" "x ^#foo#| bar")
  ("x $fo|o$ bar" "ma$" "x ^$foo$| bar")
  ("x %fo|o% bar" "ma%" "x ^%foo%| bar")
  ("x ^^fo|o^^ bar" "ma^" "x ^^^foo^^| bar")
  ("x &fo|o& bar" "ma&" "x ^&foo&| bar")
  ("x *fo|o* bar" "ma*" "x ^*foo*| bar")
  ("x ~fo|o~ bar" "ma~" "x ^~foo~| bar")
  ("x =fo|o= bar" "ma=" "x ^=foo=| bar")
  ("x _fo|o_ bar" "ma_" "x ^_foo_| bar")
  ("fo|o bar" "mi*" "fo|o bar"))

;;; Match brackets

(hel-test-table match
  ("|(foo)" "mm" "(foo)|")
  ("(foo)|" "mm" "|(foo)")
  ("|[foo]" "mm" "[foo]|")
  ("[foo]|" "mm" "|[foo]")
  ("|{foo}" "mm" "{foo}|")
  ("{foo}|" "mm" "|{foo}")
  ;; Inside of brackets `mm' goes after the closing one.
  ("(f|oo)" "mm" "(foo)|")
  ("(a (b|) c)" "mm" "(a (b)| c)")
  ("(a |(b) c)" "mm" "(a (b)| c)")
  ;; Nesting and lines.
  ("|(a (b)\n c)" "mm" "(a (b)\n c)|")
  ("(a (b)\n c)|" "mm" "|(a (b)\n c)")
  ;; `mm' drops the selection.
  ("^(foo)|" "mm" "|(foo)")
  ;; Nothing to match.
  ("fo|o" "mm" "fo|o")
  (:mode c-mode "|{ a(b); }" "mm" "{ a(b); }|")
  (:mode emacs-lisp-mode "|(a \")\" b)" "mm" "(a \")\" b)|"))

;;; Surround

(hel-test-table surround-add
  ;; The selection stays on the surrounded text.
  ;; Left brackets add spaces, right brackets do not.
  ("^foo| bar" "ms(" "( ^foo| ) bar")
  ("^foo| bar" "ms)" "(^foo|) bar")
  ("^foo| bar" "ms[" "[ ^foo| ] bar")
  ("^foo| bar" "ms]" "[^foo|] bar")
  ("^foo| bar" "ms{" "{ ^foo| } bar")
  ("^foo| bar" "ms}" "{^foo|} bar")
  ("^foo| bar" "ms<" "< ^foo| > bar")
  ("^foo| bar" "ms>" "<^foo|> bar")
  ;; Quotes and other characters surround without spaces.
  ("^foo| bar" "ms\"" "\"^foo|\" bar")
  ("^foo| bar" "ms'" "'^foo|' bar")
  ("^foo| bar" "ms`" "`^foo|` bar")
  ("^foo| bar" "ms*" "*^foo|* bar")
  ;; The selection direction is kept.
  ("|foo^ bar" "ms)" "(|foo^) bar")
  ;; Spaces at the ends of the selection stay outside of the delimiters.
  ("x^ foo |bar" "ms)" "x (^foo|) bar")
  ("x^ foo |bar" "ms(" "x ( ^foo| ) bar")
  ;; Without a selection `ms' does nothing and does not ask for a character.
  ("foo| bar" "ms" "foo| bar")
  ;; A line selection puts the delimiters of left brackets on separate lines.
  ("|foo\nbar\n" "xms(" "(\n|foo\n^)\nbar\n")
  ("|foo\nbar\n" "xms{" "{\n|foo\n^}\nbar\n")
  ;; A right bracket removes the newline from the selection and is tight.
  ("|foo\nbar\n" "xms)" "(|foo^)\nbar\n")
  ;; Programming modes indent the result.
  (:mode c-mode "|foo;\n" "xms{" "{\n  |foo;\n^}\n")
  ;; Tags.
  ("^foo| bar" "mstdiv RET" "<div>^foo|</div> bar")
  ("^foo| bar" "mstdiv SPC class=\"box\" RET" "<div class=\"box\">^foo|</div> bar")
  ("|foo\nbar\n" "xmstdiv RET" "<div>\n|foo\n^</div>\nbar\n"))

(hel-test-table surround-delete
  ("(fo|o) bar" "md(" "fo|o bar")
  ("(fo|o) bar" "md)" "fo|o bar")
  ("[fo|o] bar" "md[" "fo|o bar")
  ("{fo|o} bar" "md{" "fo|o bar")
  ("<fo|o> bar" "md<" "fo|o bar")
  ("\"fo|o\" bar" "md\"" "fo|o bar")
  ("*fo|o* bar" "md*" "fo|o bar")
  ;; The innermost pair goes away.
  ("(a (b|) c)" "md(" "(a b| c)")
  ;; No pair: nothing changes.
  ("fo|o" "md(" "fo|o")
  ;; A selection stays on its text.
  ("(^foo|) bar" "md(" "^foo| bar")
  ;; Tags.
  ("<div>fo|o</div>" "mdtdiv RET" "fo|o")
  ("<div class=\"box\">fo|o</div>" "mdtdiv RET" "fo|o")
  ("<a><b>fo|o</b></a>" "mdta RET" "<b>fo|o</b>")
  ("<a><b>fo|o</b></a>" "mdtRET" "<a>fo|o</a>"))

(hel-test-table surround-change
  ("(fo|o) bar" "mr([" "[ fo|o ] bar")
  ("(fo|o) bar" "mr(]" "[fo|o] bar")
  ("[fo|o] bar" "mr])" "(fo|o) bar")
  ("{fo|o} bar" "mr{(" "( fo|o ) bar")
  ("\"fo|o\" bar" "mr\"'" "'fo|o' bar")
  ("(fo|o) bar" "mr(\"" "\"fo|o\" bar")
  ("*fo|o* bar" "mr*_" "_fo|o_ bar")
  ;; The innermost pair changes.
  ("(a (b|b) c)" "mr(]" "(a [b|b] c)")
  ;; A selection stays on its text.
  ("(^foo|) bar" "mr(]" "[^foo|] bar")
  ;; Tags.
  ("<a>fo|o</a>" "mrta RET b RET" "<b>fo|o</b>")
  ("<a>fo|o</a>" "mrtRET b RET" "<b>fo|o</b>")
  ("<a>fo|o</a>" "mrtRET b SPC c=\"d\" RET" "<b c=\"d\">fo|o</b>"))

;;; Multiple cursors: creating

(hel-test-table cursors-copy
  ("|foo\nbar\nbaz" "C" "¦foo\n|bar\nbaz")
  ("|foo\nbar\nbaz" "CC" "¦foo\n¦bar\n|baz")
  ("|foo\nbar\nbaz" "2C" "¦foo\n¦bar\n|baz")
  ("foo\nbar\n|baz" "M-c" "foo\n|bar\n¦baz")
  ("foo\nbar\n|baz" "M-cM-c" "|foo\n¦bar\n¦baz")
  ("foo\nbar\n|baz" "2M-c" "|foo\n¦bar\n¦baz")
  ;; A line that is too short gets no cursor.
  ("abc|\nd\nabc" "C" "abc¦\nd\nabc|")
  ;; No line below: nothing changes.
  ("foo\nb|ar" "C" "foo\nb|ar")
  ;; A selection is copied with its shape.
  ("^fo|o\nbar" "C" "¬fo¦o\n^ba|r")
  ("|fo^o\nbar" "C" "¦fo¬o\n|ba^r")
  ("foo\n^ba|r" "M-c" "^fo|o\n¬ba¦r")
  ;; A selection over two lines is copied over the next two lines.
  ("^a\nb|\nc\nd\n" "C" "¬a\nb¦\n^c\nd|\n"))

(hel-test-table cursors-split-lines
  ("^ab\ncd\nef|" "M-s" "¬ab¦\n¬cd¦\n^ef|")
  ("^ab\ncd\n|" "M-s" "¬ab¦\n^cd|\n")
  ("a^b\nc|d" "M-s" "a¬b¦\n^c|d")
  ("^abc|" "M-s" "^abc|")
  ("a|b" "M-s" "a|b"))

(hel-test-table cursors-select
  ;; `s' selects all matches of a regexp inside the selection.
  ("^foo bar foo|" "sfoo RET" "^foo| bar ¬foo¦")
  ("^foo bar foo| foo" "sfoo RET" "^foo| bar ¬foo¦ foo")
  ("^a1 b22 c333|" "s\\d+ RET" "a^1| b¬22¦ c¬333¦")
  ;; Each match of a group selects only the group.
  ("^foo=1 bar=2|" "s(\\w+)= RET" "^foo|=1 ¬bar¦=2")
  ;; PCRE syntax.
  ("^aa bb|" "s[ab]{2} RET" "^aa| ¬bb¦")
  ("^foo FOO|" "sfoo RET" "^foo| ¬FOO¦")
  ;; No match or no input keeps the selection.
  ("^foo|" "szzz RET" "^foo|")
  ("^foo|" "sRET" "^foo|")
  ;; Without a selection `s' does nothing and does not prompt.
  ("foo|" "s" "foo|")
  ;; `S' selects the text between the matches.
  ("^a,b,c|" "S, RET" "^a|,¬b¦,¬c¦")
  ("^a, b, c|" "S,\\s* RET" "^a|, ¬b¦, ¬c¦")
  ("^abc|" "Szzz RET" "^abc|")
  ;; `%' and then `s' work on the whole buffer.
  ("|foo\nbar\nfoo\n" "%sfoo RET" "^foo|\nbar\n¬foo¦\n")
  ;; `s' works inside of each selection.
  ("|ab ab\nab ab\n" "wC" "¬ab ¦ab\n^ab |ab\n"))

;;; Multiple cursors: keys active with several cursors

(hel-test-table cursors-remove
  ;; `,' keeps only the main cursor.
  ("|a\nb\nc" "C," "a\n|b\nc")
  ("|a\nb\nc" "CC," "a\nb\n|c")
  ;; `M-,' removes the main cursor, and the next cursor becomes the main one.
  ("|a\nb\nc" "CCM-," "|a\n¦b\nc")
  ("|a\nb\nc" "CCggM-," "a\n|b\n¦c")
  ("|a\nb" "CM-," "|a\nb")
  ;; `Esc' and `;' drop selections and keep the cursors.
  ("^fo|o\nbar" "C<escape>" "fo¦o\nba|r")
  ("^fo|o\nbar" "C;" "fo¦o\nba|r"))

(hel-test-table cursors-main
  ;; `gg', `ge' and `G' choose the first and the last cursor.
  ("|a\nb\nc" "CCgg" "|a\n¦b\n¦c")
  ("|a\nb\nc" "CCggG" "¦a\n¦b\n|c")
  ("|a\nb\nc" "CCggge" "¦a\n¦b\n|c")
  ;; `(' and `)' rotate the main cursor.  The cursors are in the middle of the
  ;; buffer, because a cursor at the very start of the buffer behaves
  ;; differently.
  ("x\n|a\nb\nc\ny" "CC(" "x\n¦a\n|b\n¦c\ny")
  ("x\n|a\nb\nc\ny" "CC((" "x\n|a\n¦b\n¦c\ny")
  ("x\n|a\nb\nc\ny" "CC(((" "x\n¦a\n¦b\n|c\ny")
  ("x\n|a\nb\nc\ny" "CC2(" "x\n|a\n¦b\n¦c\ny")
  ("x\n|a\nb\nc\ny" "CC)" "x\n|a\n¦b\n¦c\ny")
  ("x\n|a\nb\nc\ny" "CC))" "x\n¦a\n|b\n¦c\ny")
  ("x\n|a\nb\nc\ny" "CC)))" "x\n¦a\n¦b\n|c\ny")
  ("x\n|a\nb\nc\ny" "CC2)" "x\n¦a\n|b\n¦c\ny"))

(ert-deftest hel-behaviour-cursors-restore ()
  "`gv' restores the cursors that `,' removed."
  (hel-test-with 'text-mode "|a\nb\nc"
    (hel-test-expect "CC," "a\nb\n|c")
    (hel-test-expect "gv" "¦a\n¦b\n|c")))

(ert-deftest hel-behaviour-cursors-keys-need-several-cursors ()
  "The keys of multiple cursors do nothing with one cursor."
  (dolist (keys '("K" "," "&"))
    (hel-test-with 'text-mode "fo|o"
      (hel-test-press keys)
      (should (equal (hel-test-render) "fo|o"))
      (should (eq (hel-test-state) 'normal))))
  ;; `gg', `ge' and `G' keep their usual meaning.
  (hel-test-with 'text-mode "a\n|b\nc"
    (hel-test-expect "G" "a\nb\nc|")
    (hel-test-expect "gg" "|a\nb\nc")))

(hel-test-table cursors-filter
  ;; `K' keeps the selections that match, `M-K' removes them.
  ("^foo bar baz|" "s\\w+ RET Kba RET" "foo ^bar| ¬baz¦")
  ("^foo bar baz|" "s\\w+ RET M-Kba RET" "^foo| bar baz")
  ("^foo bar baz|" "s\\w+ RET Kzzz RET" "^foo| ¬bar¦ ¬baz¦")
  ("^foo bar baz|" "s\\w+ RET KRET" "^foo| ¬bar¦ ¬baz¦"))

(hel-test-table cursors-align
  ("^a=1\nbbb=2|" "s= RET &" "a  ^=|1\nbbb¬=¦2")
  ("^aaa=1\nb=2|" "s= RET &" "aaa^=|1\nb  ¬=¦2")
  ;; Each column of cursors aligns separately.
  ("^a,b\nccc,ddd,|" "s, RET &" "a  ^,|b\nccc¬,¦ddd¬,¦"))

(hel-test-table cursors-merge
  ("^a,b,c|" "s\\w RET M--" "^a,b,c|")
  ("|a\nb\nc" "CC M--" "^a\nb\n|c")
  ;; The selections merge when their lines meet.
  ("|a\nb\nc\n" "Cxx" "^a\nb\nc\n|"))

(hel-test-table cursors-rotate-content
  ("^a b c|" "s\\w RET M-)" "^c| ¬a¦ ¬b¦")
  ("^a b c|" "s\\w RET M-(" "^b| ¬c¦ ¬a¦")
  ("^aa b ccc|" "s\\w+ RET M-)" "^ccc| ¬aa¦ ¬b¦")
  ("^a b c|" "s\\w RET 2M-)" "^b| ¬c¦ ¬a¦")
  ("^a b c|" "s\\w RET M-)M-(" "^a| ¬b¦ ¬c¦"))

;;; Multiple cursors: every command runs for all cursors

(hel-test-table cursors-edit
  ("|foo\nbar" "CD" "¦oo\n|ar")
  ("^fo|o\nbar" "Cd" "¦o\n|r")
  ("fo|o\nbar" "Cd" "f¦o\nb|r")
  ("^fo|o\nbar" "CD" "¦o\n|r")
  ("^fo|o\nbar" "Cc" "¦o\n|r" :state insert)
  ("|foo\nbar" "Cix<escape>" "x¦foo\nx|bar")
  ("|foo\nbar" "Cax<escape>" "x¦foo\nx|bar")
  ("^fo|o\nbar" "Cax<escape>" "¬fox¦o\n^bax|r")
  ("^fo|o\nbar" "Cix<escape>" "x¦fo¬o\nx|ba^r")
  ("f|oo\nbar" "Cox<escape>" "foo\nx¦\nbar\nx|")
  ("f|oo\nbar" "COx<escape>" "x¦\nfoo\nx|\nbar")
  ("f|oo\nbar" "CAx<escape>" "foox¦\nbarx|")
  ("f|oo\nbar" "CIx<escape>" "xfoo\nxbar" :text-only t)
  ("^fo|o\nbar" "CrX" "¬XX¦o\n^XX|r")
  ("^fo|o\nbar" "C~" "¬FO¦o\n^BA|r")
  ("^fo|o\nbar" "CgU" "¬FO¦o\n^BA|r")
  ("f|oo\nbar\nbaz\nqux\n" "CJ" "f¦oo b|ar baz\nqux\n")
  ("^fo|o\nbar\n" "C>" " ¬fo¦o\n ^ba|r\n")
  ("f|oo\nbar" "C]SPC" "f¦oo\n\nb|ar\n")
  ("^fo|o\nbar" "Cms)" "(¬fo¦)o\n(^ba|)r")
  ("^fo|o\nbar" "CmsX" "X¬fo¦Xo\nX^ba|Xr")
  ("(a|)\n(b)" "Cmd(" "a¦\nb|")
  ("(a|)\n(b)" "Cmr(]" "[a]¦\n[b]|")
  ("^foo bar| baz" "s\\w+ RET mi\"" "^foo| ¬bar¦ baz"))

(hel-test-table cursors-motion
  ("|foo\nbar" "Cl" "f¦oo\nb|ar")
  ("fo|o\nbar" "Ch" "f¦oo\nb|ar")
  ("|foo\nbar\nbaz" "Cj" "foo\n¦bar\n|baz")
  ("|foo a\nbar b" "Cw" "¬foo ¦a\n^bar |b")
  ("|foo a\nbar b" "Ce" "¬foo¦ a\n^bar| b")
  ("fo|o a\nbar b" "Cgh" "¦fo¬o a\n|ba^r b")
  ("fo|o\nbar" "Cgl" "fo¬o¦\nba^r|")
  ("|foo\nbar\nbaz\n" "Cx" "¦foo\n|¬bar\n^baz\n")
  ("|foo bar\nfoo bar" "Cfa" "¬foo ba¦r\n^foo ba|r")
  ("|(a)\n(b)" "Cmm" "(a)¦\n(b)|")
  ("f|oo\nbar" "Cmiw" "¬foo¦\n^bar|")
  ("(|a)\n(b)" "Cmi(" "(¬a¦)\n(^b|)")
  ;; Cursors that meet merge into one.
  ("foo\n|bar" "M-c l" "f|oo\nb¦ar"))

(hel-test-table cursors-commands-that-reset
  ;; `%' keeps one cursor.
  ("|foo\nbar" "C%" "|foo\nbar^"))

(ert-deftest hel-behaviour-cursors-copy-paste ()
  "Each cursor has its own kill ring."
  (hel-test-with 'text-mode "^foo bar|"
    (hel-test-expect "s\\w+ RET" "^foo| ¬bar¦")
    (hel-test-expect "y" "^foo| ¬bar¦")
    (hel-test-expect "<escape>" "foo| bar¦")
    (hel-test-expect "p" "foo^foo| bar¬bar¦")))

(ert-deftest hel-behaviour-cursors-same-text-pastes-for-all ()
  "A text that the main cursor copied before the cursors exist goes to all."
  (hel-test-with 'text-mode "^x| a\nb c"
    (hel-test-press! "y;")
    (hel-test-expect "l C p" "x ¬x¦a\nb ^x|c")))

(ert-deftest hel-behaviour-cursors-paste-rectangle ()
  "`M-u p' pastes the texts of all selections on separate lines."
  (hel-test-with 'text-mode "^foo bar|\nz"
    (hel-test-press! "s\\w+ RET y,G")
    (hel-test-press! "M-u p")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "foo bar\nzfoo\n bar"))))

(ert-deftest hel-behaviour-cursors-undo-is-one-step ()
  "One undo reverts the change of all cursors and brings back their selections."
  (hel-test-with 'text-mode "^foo foo|"
    (hel-test-expect "sfoo RET" "^foo| ¬foo¦")
    (hel-test-expect "d" "| ¦")
    (hel-test-expect "u" "^foo| ¬foo¦")
    (hel-test-expect "U" "| ¦")))

(ert-deftest hel-behaviour-cursors-insert-undo-is-one-step ()
  "One undo reverts a word typed with all cursors."
  (hel-test-with 'text-mode "|foo\nbar"
    (hel-test-expect "Cix<escape>" "x¦foo\nx|bar")
    (hel-test-expect "u" "¦foo\n|bar")))

(ert-deftest hel-behaviour-cursors-unknown-command-needs-a-choice ()
  "A command that Hel does not know asks once whether to run for all cursors.
In this test the question is the guard of the test setup, which fails on it."
  (defalias 'hel-test--unknown-command (lambda () (interactive)))
  (global-set-key [f34] 'hel-test--unknown-command)
  (unwind-protect
      (hel-test-with 'text-mode "|a\nb"
        (hel-test-press! "C")
        (should-error (hel-test-press "<f34>") :type 'ert-test-failed)
        (should (equal hel-test--unexpected-prompts
                       '("Do hel-test--unknown-command for all cursors?"))))
    (global-unset-key [f34])))

;;; Search

(hel-test-table search
  ;; `/' selects the next match.
  ("|foo bar foo bar" "/bar RET" "foo ^bar| foo bar")
  ("foo |bar foo bar" "/bar RET" "foo ^bar| foo bar")
  ("foo bar| foo bar" "/bar RET" "foo bar foo ^bar|")
  ;; `?' selects the previous match, with the cursor at its end.
  ("foo bar foo bar|" "?foo RET" "foo bar ^foo| bar")
  ;; The search wraps around the buffer.
  ("foo bar|" "/foo RET" "^foo| bar")
  ("|foo bar" "?bar RET" "foo ^bar|")
  ;; PCRE syntax.
  ("|a1 b22 c" "/\\d\\d RET" "a1 b^22| c")
  ("|foo(bar)" "/\\(b RET" "foo^(b|ar)")
  ("|ab abab" "/(ab){2} RET" "ab ab^ab|")
  ;; A group selects only the group.
  ("|foo=1" "/(\\w+)=1 RET" "^foo|=1")
  ;; Lower case input finds both cases.
  ("|x FOO" "/foo RET" "x ^FOO|")
  ;; No match: the cursor stays.
  ("foo |bar" "/zzz RET" "foo |bar")
  ;; An empty input and a cancel change nothing.
  ("foo |bar" "/RET" "foo |bar")
  ("foo |bar" "/bar C-g" "foo |bar")
  ("foo |bar" "?foo C-g" "foo |bar")
  ;; A new search replaces the selection.
  ("^foo| bar" "/bar RET" "foo ^bar|"))

(hel-test-table search-next
  ("|a a a a" "/a RET n" "a ^a| a a")
  ("|a a a a" "/a RET nn" "a a ^a| a")
  ("|a a a a" "/a RET 2n" "a a ^a| a")
  ("|a a a a" "/a RET nnN" "a ^a| a a")
  ;; `n' wraps around the buffer.
  ("|a b a" "/a RET nn" "^a| b a")
  ("|a b a" "/a RET N" "a b ^a|")
  ;; After `?' the keys `n' and `N' are swapped.
  ("a b a c a|" "?a RET" "a b a c ^a|")
  ("a b a c a|" "?a RET n" "a b ^a| c a")
  ("a b a c a|" "?a RET nn" "^a| b a c a")
  ("a b a c a|" "?a RET nN" "a b a c ^a|")
  ;; After another command `n' goes forward from the cursor.
  ("a b a c a|" "?a RET hn" "a b a c ^a|")
  ("|x a b a" "/a RET lln" "x a b ^a|")
  ;; With `v' `n' adds a cursor at each match.
  ("|a b a c a" "/a RET vn" "¬a¦ b ^a| c a")
  ("|a b a c a" "/a RET vnn" "¬a¦ b ¬a¦ c ^a|")
  ;; An empty register is an error.
  ("fo|o" "n" "fo|o" :error "Register / is empty")
  ("fo|o" "N" "fo|o" :error "Register / is empty"))

(hel-test-table search-from-selection
  ;; `*' takes the selection as the pattern, with word boundaries for a word.
  ("^foo| foobar foo" "*n" "foo foobar ^foo|")
  ("^foo| foobar foo" "*nn" "^foo| foobar foo")
  ;; `M-*' has no word boundaries.
  ("^foo| foobar foo" "M-*n" "foo ^foo|bar foo")
  ("^foo| foobar foo" "M-*nn" "foo foobar ^foo|")
  ;; A part of a word has no boundaries.
  ("^fo|o foobar" "*n" "foo ^fo|obar")
  ;; Special characters are literal.
  ("^a.b| axb a.b" "*n" "a.b axb ^a.b|")
  ("^a.b| axb a.b" "M-*n" "a.b axb ^a.b|")
  ;; Selections of all cursors go to the pattern.
  ("^foo bar| baz bar foo" "s\\w+ RET*,n" "foo ^bar| baz bar foo")
  ("^foo bar| baz bar foo" "s\\w+ RET*,nn" "foo bar baz ^bar| foo")
  ("^foo bar| baz bar foo" "s\\w+ RET*,nnn" "foo bar baz bar ^foo|")
  ;; Without a selection nothing happens.
  ("fo|o" "*" "fo|o")
  ("fo|o" "*n" "fo|o" :error "Register / is empty"))

(ert-deftest hel-behaviour-search-register ()
  "`*' and `/' fill the `/' register, so that `n' works in any buffer."
  (hel-test-with 'text-mode "^foo| bar foo"
    (hel-test-press! "*")
    (should (equal (hel-test-search-register) "\\bfoo\\b"))
    (hel-test-press! ";/bar RET")
    (should (equal (hel-test-search-register) "bar"))))

(ert-deftest hel-behaviour-search-esc-keeps-pattern ()
  "`Esc' drops the selection and the highlights, and `n' still works."
  (hel-test-with 'text-mode "|a b a b"
    (hel-test-expect "/b RET" "a ^b| a b")
    (hel-test-expect "<escape>" "a b| a b")
    (hel-test-expect "<escape>" "a b| a b")
    (hel-test-expect "n" "a b a ^b|")))

(ert-deftest hel-behaviour-search-highlights ()
  "A search highlights its matches, and the second `Esc' removes them."
  (hel-test-with 'text-mode "^foo| bar foo"
    (hel-test-press! "*")
    ;; Timers scan the buffer, so the highlights need a short wait.
    (let ((tries 200))
      (while (and (< (hel-test-search-highlights) 2) (< 0 tries))
        (sleep-for 0.01)
        (cl-decf tries)))
    (should (= (hel-test-search-highlights) 2))
    (hel-test-press! "<escape><escape>")
    (should (= (hel-test-search-highlights) 0))))

;;; Mark ring and jumps

(ert-deftest hel-behaviour-jump-back-from-buffer-end ()
  "Jumps push the position, and `C-o' and `C-<i>' walk over the positions."
  (hel-test-with 'text-mode "a|b\nc\nd"
    (hel-test-expect "G" "ab\nc\nd|")
    (hel-test-expect "C-o" "a|b\nc\nd")))

(hel-test-table jump
  ("a|b\nc\nd" "C-sjC-o" "a|b\nc\nd")
  ("a|b\nc\nd" "ggC-o" "a|b\nc\nd")
  ("a|b\nc\nd" "%C-o" "a|b\nc\nd")
  ;; `C-o' drops the selection.
  ("a|b\nc\nd" "C-s%C-o" "a|b\nc\nd")
  ;; An empty mark ring: nothing happens.
  ("a|b" "C-o" "a|b")
  ("a|b" "C-<i>" "a|b")
  ;; Paragraph motions and searches push the position.
  ("|a\n\nb\n\nc\n" "}C-o" "|a\n\nb\n\nc\n")
  ("|a b" "/b RET" "a ^b|")
  ;; With `v' a jump back extends the selection.
  ("a|b\nc\nd" "C-sjvC-o" "a|b\nc^\nd"))

(ert-deftest hel-behaviour-jump-walk-over-the-ring ()
  "`C-o' goes to older positions and `C-<i>' to newer ones."
  (hel-test-with 'text-mode "|a\nb\nc\nd\ne"
    (hel-test-expect "C-s jj C-s jj C-s" "a\nb\nc\nd\n|e")
    ;; Point is on the newest position, so the first `C-o' goes to the older one.
    (hel-test-expect "C-o" "a\nb\n|c\nd\ne")
    (hel-test-expect "C-o" "|a\nb\nc\nd\ne")
    (hel-test-expect "C-<i>" "a\nb\n|c\nd\ne")
    (hel-test-expect "C-<i>" "a\nb\nc\nd\n|e")))

(ert-deftest hel-behaviour-jump-global-ring ()
  "`C-S-o' goes back to another buffer."
  (let ((other (generate-new-buffer "*hel-test-other*")))
    (unwind-protect
        (hel-test-with 'text-mode "a|b\nc"
          (let ((first (current-buffer)))
            (hel-test-press! "G")
            (with-current-buffer other
              (text-mode)
              (insert "other text"))
            (switch-to-buffer other)
            (goto-char 3)
            (hel-test-press! "gg")
            (hel-test-press! "C-S-o")
            (should (eq (current-buffer) first))))
      (kill-buffer other))))

;;; Scrolling

(defun hel-test--long-text ()
  "Return a text of 300 numbered lines."
  (mapconcat (lambda (n) (format "line %d\n" n)) (number-sequence 1 300) ""))

(defun hel-test--top-line ()
  "Return the number of the buffer line at the top of the window."
  (line-number-at-pos (window-start)))

(ert-deftest hel-behaviour-scroll-page ()
  "`C-f' and `C-b' scroll by a full window, and the cursor stays on the screen."
  (hel-test-with 'text-mode (concat "|" (hel-test--long-text))
    (let ((height (window-body-height)))
      (hel-test-press! "C-f")
      (should (= (hel-test--top-line) (1+ height)))
      (should (>= (point) (window-start)))
      (hel-test-press! "C-f")
      (should (= (hel-test--top-line) (1+ (* 2 height))))
      (hel-test-press! "C-b")
      (should (= (hel-test--top-line) (1+ height)))
      (hel-test-press! "C-b")
      (should (= (hel-test--top-line) 1)))))

(ert-deftest hel-behaviour-scroll-half-page ()
  "`C-d' and `C-u' scroll by a half of the window."
  (hel-test-with 'text-mode (concat "|" (hel-test--long-text))
    (hel-test-press! "C-d")
    (let ((half (1- (hel-test--top-line))))
      (should (= half (/ (window-body-height) 2)))
      (hel-test-press! "C-d")
      (should (= (hel-test--top-line) (1+ (* 2 half))))
      (hel-test-press! "C-u")
      (should (= (hel-test--top-line) (1+ half))))))

(ert-deftest hel-behaviour-scroll-half-page-count ()
  "A count of `C-d' sets the number of lines for this and later scrolls."
  (hel-test-with 'text-mode (concat "|" (hel-test--long-text))
    (unwind-protect
        (progn
          (hel-test-press! "3 C-d")
          (should (= (hel-test--top-line) 4))
          (hel-test-press! "C-d")
          (should (= (hel-test--top-line) 7))
          (hel-test-press! "C-u")
          (should (= (hel-test--top-line) 4)))
      ;; The count 0 goes back to half a window.
      (hel-test-press! "0 C-d"))
    (should (= (hel-test--top-line) (+ 4 (/ (window-body-height) 2))))))

(ert-deftest hel-behaviour-scroll-line ()
  "`C-e' and `C-y' scroll by one line or by a count of lines."
  (hel-test-with 'text-mode (concat "|" (hel-test--long-text))
    (hel-test-press! "C-e")
    (should (= (hel-test--top-line) 2))
    (hel-test-press! "5 C-e")
    (should (= (hel-test--top-line) 7))
    (hel-test-press! "2 C-y")
    (should (= (hel-test--top-line) 5))))

(ert-deftest hel-behaviour-scroll-cursor-line-to-window-edges ()
  "`zt', `zz' and `zb' put the line of the cursor at a place in the window."
  (hel-test-with 'text-mode (concat (hel-test--long-text) "|")
    (goto-char (point-min))
    (forward-line 149)
    (hel-test-press! "zt")
    (should (= (hel-test--top-line) 150))
    (hel-test-press! "zb")
    (should (< (hel-test--top-line) 150))
    (should (<= 150 (+ (hel-test--top-line) (window-body-height))))))

(ert-deftest hel-behaviour-scroll-keeps-the-buffer ()
  "Scrolling at the ends of the buffer changes nothing and signals nothing."
  (hel-test-with 'text-mode "|a\nb\n"
    (hel-test-press! "C-b")
    (should (= (hel-test--top-line) 1))
    (should (equal (buffer-string) "a\nb\n"))))

;;; Windows and buffers

(ert-deftest hel-behaviour-window-split-and-close ()
  "`C-w s', `C-w v', `C-w c', `C-w o' and `C-w w' manage windows."
  (hel-test-with 'text-mode "fo|o"
    (should (= (count-windows) 1))
    (hel-test-press! "C-w s")
    (should (= (count-windows) 2))
    (hel-test-press! "C-w v")
    (should (= (count-windows) 3))
    (let ((window (selected-window)))
      (hel-test-press! "C-w w")
      (should-not (eq (selected-window) window)))
    (hel-test-press! "C-w c")
    (should (= (count-windows) 2))
    (hel-test-press! "C-w o")
    (should (= (count-windows) 1))
    (should (equal (hel-test-render) "fo|o"))))

(ert-deftest hel-behaviour-window-move-by-direction ()
  "`C-w j', `C-w k', `C-w h' and `C-w l' go to the window in that direction."
  (hel-test-with 'text-mode "fo|o"
    (hel-test-press! "C-w s")
    (let ((bottom (selected-window)))
      (hel-test-press! "C-w k")
      (should-not (eq (selected-window) bottom))
      (hel-test-press! "C-w j")
      (should (eq (selected-window) bottom)))
    (hel-test-press! "C-w o C-w v")
    (let ((right (selected-window)))
      (hel-test-press! "C-w h")
      (should-not (eq (selected-window) right))
      (hel-test-press! "C-w l")
      (should (eq (selected-window) right)))))

(ert-deftest hel-behaviour-window-second-prefix ()
  "`C-c w' is a second prefix for window keys."
  (hel-test-with 'text-mode "fo|o"
    (hel-test-press! "C-c w s")
    (should (= (count-windows) 2))
    (hel-test-press! "C-c w C-o")
    (should (= (count-windows) 1))))

(ert-deftest hel-behaviour-window-move ()
  "`C-w J' moves the window, and the number of windows stays."
  (hel-test-with 'text-mode "fo|o"
    (hel-test-press! "C-w v")
    (let ((window-buffer (window-buffer)))
      (hel-test-press! "C-w J")
      (should (= (count-windows) 2))
      (should (eq (window-buffer) window-buffer)))))

(ert-deftest hel-behaviour-window-kill-buffer ()
  "`C-w d' kills the buffer of the window."
  (let ((other (generate-new-buffer "*hel-test-other*")))
    (hel-test-with 'text-mode "fo|o"
      (with-current-buffer other (text-mode))
      (switch-to-buffer other)
      (hel-test-press! "C-w d")
      (should-not (buffer-live-p other)))))

(ert-deftest hel-behaviour-window-kill-buffer-and-window ()
  "`C-w q' kills the buffer of the window, and closes the window."
  (let ((other (generate-new-buffer "*hel-test-other*")))
    (hel-test-with 'text-mode "fo|o"
      (hel-test-press! "C-w s")
      (with-current-buffer other (text-mode))
      (switch-to-buffer other)
      (hel-test-press! "C-w q")
      (should-not (buffer-live-p other))
      (should (= (count-windows) 1)))))

(ert-deftest hel-behaviour-window-scratch ()
  "`C-w x' shows the scratch buffer."
  (hel-test-with 'text-mode "fo|o"
    (hel-test-press! "C-w x")
    (should (equal (buffer-name) "*scratch*"))))

(ert-deftest hel-behaviour-buffer-switch ()
  "`] b' and `[ b' switch to the next and the previous buffer."
  (let ((other (generate-new-buffer "*hel-test-other*")))
    (unwind-protect
        (hel-test-with 'text-mode "fo|o"
          (let ((first (current-buffer)))
            (with-current-buffer other (text-mode))
            (switch-to-buffer other)
            (hel-test-press! "[b")
            (should-not (eq (current-buffer) other))
            (switch-to-buffer first)))
      (kill-buffer other))))

(ert-deftest hel-behaviour-buffer-save ()
  "`zx' and `ZZ' save the buffer."
  (let ((file (make-temp-file "hel-test-save")))
    (unwind-protect
        (hel-test-with 'text-mode "fo|o"
          (dolist (keys '("zx" "ZZ"))
            (setq buffer-file-name file)
            (set-buffer-modified-p t)
            (hel-test-press! keys)
            (should-not (buffer-modified-p))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           "foo\n")))
          (setq buffer-file-name nil))
      (delete-file file))))

;;; Narrowing

(ert-deftest hel-behaviour-narrow-and-widen ()
  "`zn' narrows to the selection in a new indirect buffer, and `zw' goes back."
  (hel-test-with 'text-mode "foo ^bar| baz"
    (let ((base (current-buffer)))
      (hel-test-press! "zn")
      (should-not (eq (current-buffer) base))
      (should (eq (buffer-base-buffer) base))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) "bar"))
      (hel-test-press! "zw")
      (should (eq (current-buffer) base))
      (should (equal (buffer-string) "foo bar baz")))))

(hel-test-table narrow
  ("foo| bar" "zn" "foo| bar")
  ("foo| bar" "zw" "foo| bar" :error "narrowed"))

;;; Emacs state

(define-derived-mode hel-test-special-mode special-mode "Hel-Test"
  "A major mode like the modes of help, list and error buffers.")

(ert-deftest hel-behaviour-emacs-state-in-special-mode ()
  "A mode in which letters do not insert themselves starts in Emacs state."
  (hel-test-with 'hel-test-special-mode "fo|o"
    (should (eq (hel-test-state) 'emacs))))

(ert-deftest hel-behaviour-emacs-state-switch-with-i ()
  "`i' in Emacs state goes to Normal state, and `zx' and `Esc' go back."
  (dolist (keys '("zx" "<escape>"))
    (hel-test-with 'hel-test-special-mode "fo|o"
      (hel-test-press! "i")
      (should (eq (hel-test-state) 'normal))
      (hel-test-press! "w")
      (should (equal (hel-test-render) "fo^o|"))
      (hel-test-press! keys)
      (should (eq (hel-test-state) 'emacs))
      (should-not (use-region-p)))))

(ert-deftest hel-behaviour-emacs-state-keys ()
  "Window keys and scrolling also work in Emacs state."
  (hel-test-with 'hel-test-special-mode (concat "|" (hel-test--long-text))
    (hel-test-press! "C-w s")
    (should (= (count-windows) 2))
    (hel-test-press! "C-w o")
    (should (= (count-windows) 1))
    (hel-test-press! "C-f")
    (should (> (hel-test--top-line) 1))))

(ert-deftest hel-behaviour-state-after-major-mode-change ()
  "The state follows the major mode of the buffer."
  (hel-test-with 'text-mode "fo|o"
    (should (eq (hel-test-state) 'normal))
    (hel-test-special-mode)
    (should (eq (hel-test-state) 'emacs))
    (let ((inhibit-read-only t))
      (emacs-lisp-mode))
    (should (eq (hel-test-state) 'normal))))

;;; Counts and the universal argument

(hel-test-table count
  ("|a b c d e f" "M-uw" "^a b c d |e f")
  ("|abcdef" "M-ul" "abcd|ef")
  ("|abcdefghijkl" "12l" "abcdefghijkl|")
  ("|abc" "2D" "|c")
  ("ab|c" "2d" "|c")
  ("|a\nb\nc\nd\n" "2j" "a\nb\n|c\nd\n")
  ("a\nb\nc\n|d\n" "2k" "a\n|b\nc\nd\n")
  ("|a b a b a" "/a RET 2n" "a b a b ^a|")
  ("|a\nb\nc\nd\n" "2C" "¦a\n¦b\n|c\nd\n")
  ("|a b" "1w" "^a |b")
  ;; Brackets take no count.
  ("(a) (b|) (c)" "2mi(" "(a) (^b|) (c)")
  ;; A negative count reverses a character or a line motion.
  ("foo |bar" "-l" "foo| bar")
  ("foo |bar" "-h" "foo b|ar")
  ("a\n|b" "-j" "|a\nb")
  ("|a\nb" "-k" "a\n|b"))

;;; An empty buffer

(hel-test-table empty-buffer
  ("|" "w" "|")
  ("|" "b" "|")
  ("|" "e" "|")
  ("|" "x" "|")
  ("|" "%" "|")
  ("|" "miw" "|")
  ("|" "mip" "|")
  ("|" "mm" "|")
  ("|" "gg" "|")
  ("|" "G" "|")
  ("|" "d" "|" :error "Beginning of buffer")
  ("|" "D" "|" :error "End of buffer")
  ("|" "p" "|" :error "Kill ring is empty")
  ("|" "i" "|" :state insert)
  ("|" "o" "\n|" :state insert))

;;; The end of the buffer

(hel-test-table buffer-end
  ("foo|" "~" "foo|" :error "End of buffer")
  ("foo|" "gU" "foo|" :error "End of buffer")
  ("foo|" "`" "foo|" :error "End of buffer")
  ("|fo" "~~" "FO|"))

;;; Read-only buffers

(ert-deftest hel-behaviour-read-only-rejects-edits ()
  "An edit key in a read-only buffer fails and changes nothing."
  (dolist (keys '("d" "D" "c" "i" "a" "o" "O" "p" "~" "gU"))
    (hel-test-with 'text-mode "fo|o"
      (setq buffer-read-only t)
      (kill-new "x")
      (should (string-match-p "read-only" (hel-test-press keys)))
      (should (equal (hel-test-render) "fo|o"))
      (should (eq (hel-test-state) 'normal)))))

(ert-deftest hel-behaviour-read-only-allows-motions ()
  "Motions, selections and copy work in a read-only buffer."
  (hel-test-with 'text-mode "|foo bar\nbaz\n"
    (setq buffer-read-only t)
    (hel-test-expect "w" "^foo |bar\nbaz\n")
    (hel-test-expect "y" "^foo |bar\nbaz\n")
    (should (equal (car kill-ring) "foo "))
    (hel-test-expect "x" "|foo bar\n^baz\n")
    (hel-test-expect "%" "|foo bar\nbaz\n^")))

;;; Keys without a binding

(ert-deftest hel-behaviour-unbound-keys-do-not-insert ()
  "Letters without a binding in Normal state do not insert text."
  (dolist (keys '("q" "Q" "H" "L" "M" "V" "Y"))
    (hel-test-with 'text-mode "fo|o"
      (hel-test-press keys)
      (should (equal (hel-test-render) "fo|o"))
      (should (eq (hel-test-state) 'normal)))))

;;; hel-behaviour-tests.el ends here
