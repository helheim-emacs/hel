;;; hel-node-tests.el --- Tests of the node commands -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Expand and shrink, the siblings, the Lisp nodes, and the node commands
;; with and without a parser.
;;
;; The texts of the tests use the notation of `hel-test-helpers.el'.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dash)
(require 'treesit)
(require 'eldoc)
(require 'hel)
(require 'hel-commands)
(require 'hel-structural-editing)
(require 'hel-test-helpers
         (expand-file-name "hel-test-helpers"
                           (file-name-directory (or load-file-name buffer-file-name))))

;;; Helpers

(defconst hel-node-tests--python
  "def f():\n    return g(abc, [1, 2])\n\nx = 1\n")

(defconst hel-node-tests--prose
  "First sentence here. Second one, with (a parenthetical remark) inside.\n")

(defun hel-node-tests--all-selections ()
  "Return the texts of all selections, sorted."
  (sort (hel-test-selections) #'string<))

(defun hel-node-tests--expand (n)
  "Expand N times and return the selection after each step."
  (let (result)
    (dotimes (_ n)
      (hel-expand-selection-backward 1)
      (push (hel-test-selection) result))
    (nreverse result)))

;;; Expand and shrink: syntax table

(ert-deftest hel-expand-text-mode-steps ()
  (hel-test-with #'text-mode (hel-test-mark (concat hel-node-tests--prose "\nNext paragraph.\n") "parenth")
    (let ((sentence-end-double-space nil))
      (should (equal (hel-node-tests--expand 6)
                     '("parenthetical"
                       "a parenthetical remark"
                       "(a parenthetical remark)"
                       "Second one, with (a parenthetical remark) inside."
                       "First sentence here. Second one, with (a parenthetical remark) inside.\n"
                       "First sentence here. Second one, with (a parenthetical remark) inside.\n"))))))

(ert-deftest hel-expand-sentence-double-space ()
  "With `sentence-end-double-space', one space does not end the sentence."
  (hel-test-with #'text-mode (hel-test-mark hel-node-tests--prose "parenth")
    (let ((sentence-end-double-space t))
      (should (equal (nth 3 (hel-node-tests--expand 4))
                     (string-trim-right hel-node-tests--prose))))))

(ert-deftest hel-expand-comment-in-prog-mode ()
  "A comment gives no sentence step, and its brackets give no steps."
  (hel-test-with #'python-mode (hel-test-mark (concat "x = 1  # " hel-node-tests--prose) "parenth")
    (let ((prose (string-trim-right hel-node-tests--prose)))
      (should (equal (hel-node-tests--expand 2)
                     (list "parenthetical"
                           (concat "# " prose)))))))

(ert-deftest hel-expand-string-inside-first ()
  (hel-test-with #'c-mode (hel-test-mark "(g \"some text\" [y])\n" "text")
    (should (equal (hel-node-tests--expand 4)
                   '("text" "some text" "\"some text\"" "g \"some text\" [y]")))))

(ert-deftest hel-expand-on-opening-delimiter ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(g \"some text\" (h x))\n" "\"some")
    (should (equal (hel-node-tests--expand 1) '("\"some text\""))))
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(g \"some text\" (h x))\n" "(h")
    (should (equal (hel-node-tests--expand 1) '("(h x)")))))

(ert-deftest hel-expand-comments ()
  (hel-test-with #'python-mode (hel-test-mark "# hello world\nx = 1\n" "world")
    (should (equal (hel-node-tests--expand 2)
                   '("world" "# hello world"))))
  (hel-test-with #'c-mode (hel-test-mark "int x; /* hello world */\n" "world")
    (should (equal (hel-node-tests--expand 2)
                   '("world" "/* hello world */")))))

(ert-deftest hel-expand-comment-of-two-lines ()
  "Expand goes from a word in a comment straight to the whole comment."
  (hel-test-with #'python-mode (hel-test-mark "x = 1\n  # one two\n  # three\ny = 2\n" "two")
    (should (equal (hel-node-tests--expand 2)
                   '("two" "# one two\n  # three"))))
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(foo)\n;; one two\n;; three\n(bar)\n" "two")
    (should (equal (hel-node-tests--expand 1)
                   '(";; one two\n;; three")))))

(ert-deftest hel-expand-two-brackets ()
  "A selection over two bracket pairs expands to the pair around both."
  (hel-test-with #'c-mode (hel-test-mark "(f (a b) [c d])\n" "b)")
    (set-mark (point))
    (search-forward "[c")
    (should (equal (hel-node-tests--expand 1)
                   '("(f (a b) [c d])")))))

(ert-deftest hel-expand-inside-of-innermost-bracket-only ()
  "Only the innermost bracket pair gives an inside step."
  (hel-test-with #'c-mode (hel-test-mark "f(a, (b + c));\n" "b +")
    (deactivate-mark)
    (should (equal (hel-node-tests--expand 4)
                   '("b" "b + c" "(b + c)" "(a, (b + c))")))))

;;; Expand and shrink: driver

(ert-deftest hel-expand-selects-backward ()
  (hel-test-with #'c-mode (hel-test-mark "(foo bar)" "foo")
    (set-mark 5)
    (hel-expand-selection-backward 1)
    (should (equal (hel-test-selection) "foo bar"))
    (should (= (point) 2))))

(ert-deftest hel-expand-count-and-shrink ()
  (hel-test-with #'c-mode (hel-test-mark "(a (b (c d)))" " d")
    ;; Point touches `c', so the first step selects it.
    (hel-expand-selection-backward 3)
    (should (equal (hel-test-selection) "(c d)"))
    (hel-shrink-selection 3)
    (should (eq (hel-test-selection) nil))
    (should (= (point) 9))))

(ert-deftest hel-expand-negative-counts ()
  "A negative count of shrink expands, and a negative count of expand
shrinks."
  (hel-test-with #'c-mode (hel-test-mark "(a (b (c d)))" " d")
    (hel-shrink-selection -3)
    (should (equal (hel-test-selection) "(c d)"))
    (hel-expand-selection-backward -3)
    (should (eq (hel-test-selection) nil))
    (should (= (point) 9))))

(ert-deftest hel-expand-shrink-retraces-path ()
  (hel-test-with #'text-mode (hel-test-mark hel-node-tests--prose "parenth")
    (let ((sentence-end-double-space nil)
          (steps (hel-node-tests--expand 4))
          back)
      (dotimes (_ 4)
        (hel-shrink-selection 1)
        (push (hel-test-selection) back))
      (should (equal (nreverse back)
                     (append (cdr (reverse steps)) '(nil)))))))

(ert-deftest hel-expand-shrink-without-history ()
  (hel-test-with #'text-mode (hel-test-mark "abc def" "abc")
    (set-mark 1)
    (goto-char 4)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "abc"))))

(ert-deftest hel-expand-shrink-first-sexp ()
  "Without a history, shrink selects the first sexp inside the region."
  (hel-test-with #'text-mode (hel-test-mark "abc def" "abc")
    (set-mark 1)
    (goto-char 8)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "abc")))
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(foo bar)" "foo")
    (set-mark 1)
    (goto-char 10)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "foo"))
    (should (= (point) 5))))

(ert-deftest hel-expand-shrink-after-other-motion ()
  "Shrink does nothing once the selection differs from the last expand."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(foo bar)" "foo")
    (hel-expand-selection-backward 1)
    (goto-char 6)
    (set-mark 9)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "bar"))
    (should-not hel-selection-history)))

(ert-deftest hel-shrink-restores-exchanged-direction ()
  "Shrink returns to a selection with the direction that it had last."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(foo bar)" "foo")
    (hel-expand-selection-backward 1)
    (hel-exchange-point-and-mark)
    (hel-expand-selection-backward 1)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "foo"))
    (should (= (point) (region-end)))))

(ert-deftest hel-text-object-extends-after-exchange-point-and-mark ()
  "A repeat extends the run when only the direction of the selection differs."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "one\n\ntwo\n\nthree\n" "one")
    (hel-mark-inner-paragraph 1)
    (let ((end (region-end)))
      (hel-exchange-point-and-mark)
      (hel-mark-inner-paragraph 1)
      (should (< end (region-end))))))

(ert-deftest hel-text-object-extends-after-shrink ()
  "Shrink restores the run of the selection that it returns to."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "one\n\ntwo\n\nthree\n" "one")
    (hel-mark-inner-paragraph 1)
    (hel-mark-inner-paragraph 1)
    (let ((end (region-end)))
      (hel-shrink-selection 1)
      (should (< (region-end) end))
      (hel-mark-inner-paragraph 1)
      (should (= (region-end) end)))))

(ert-deftest hel-expand-history-per-cursor ()
  (should (memq 'hel-selection-history hel-fake-cursor-variables))
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(f (a x) (b y))" "a x")
    (hel-local-mode 1)
    (hel-normal-state)
    (let ((fake (hel-create-fake-cursor (save-excursion
                                          (search-forward "b y")
                                          (match-beginning 0)))))
      (unwind-protect
          (progn
            (hel-expand-selection-backward 1)
            (hel-save-excursion
              (hel-with-fake-cursor fake
                (hel-expand-selection-backward 1)))
            (hel-shrink-selection 1)
            (should (eq (hel-test-selection) nil))
            (should (equal (buffer-substring (point) (1+ (point))) "a")))
        (hel-remove-all-fake-cursors)))))

;;; Expand and shrink: tree-sitter

(ert-deftest hel-treesit-expand-from-region ()
  "Expand from a region that crosses node borders reaches the smallest node
that covers it."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    (hel-test-select "abc, [1")
    (hel-expand-selection-backward 1)
    (should (equal (hel-test-selection) "(abc, [1, 2])"))))

(ert-deftest hel-treesit-shrink-without-history ()
  "Without a history, shrink selects the first named child."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    (hel-test-select "(abc, [1, 2])")
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "abc"))
    (hel-test-select "[1, 2]")
    (hel-shrink-selection 2)
    (should (equal (hel-test-selection) "1"))))

;;; Siblings

(ert-deftest hel-treesit-siblings-skip-commas ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    ;; Without a selection, the first press selects the node after point.
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "abc"))
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "[1, 2]"))
    (should (= (point) (region-end)))
    (hel-select-previous-sibling 1)
    (should (equal (hel-test-selection) "abc"))
    (should (= (point) (region-end)))))

(ert-deftest hel-treesit-sibling-stays-in-parent ()
  "At the last argument, the selection does not change."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    (hel-test-select "[1, 2]")
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "[1, 2]"))))

(ert-deftest hel-treesit-sibling-of-wrapper ()
  "The `assignment' of `x = 1' has no siblings, but its
`expression_statement' with the same range has."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "x = 1")
    (hel-test-select "x = 1")
    (hel-select-previous-sibling 1)
    (should (equal (hel-test-selection)
                   "def f():\n    return g(abc, [1, 2])"))))

(ert-deftest hel-treesit-sibling-from-word-with-space ()
  "A selection with a trailing space, as `w' makes it, starts from the
node of the word."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "x = 1")
    (hel-test-select "x ")
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "1"))))

(ert-deftest hel-treesit-sibling-count ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "1,")
    (hel-select-next-sibling 2)
    (should (equal (hel-test-selection) "2"))
    (hel-select-previous-sibling 1)
    (should (equal (hel-test-selection) "1"))))

;;; Point without a selection

;; Rule 10 of the design: without a selection, point is a position. A
;; command with a direction takes the node on its side of point, and
;; expand takes the node that touches point, the one after point first.

(defun hel-node-tests--bare (mode point-text command)
  "Run COMMAND once from a bare point in a MODE buffer, return the selection.
POINT-TEXT is a marked text with point and no selection."
  (hel-test-with mode point-text
    (funcall command 1)
    (hel-test-selection)))

(defconst hel-node-tests--bare-lisp
  '(("(defun name |(mode state))" "(mode state)" "name" "(mode state)")
    ("(defun name| (mode state))" "(mode state)" "name" "name")
    ("(defun na|me (mode state))" "name" "name" "name")
    ("(f foo|(x))" "(x)" "foo" "(x)")
    ;; In Lisp, expand from whitespace selects the whole list: a Lisp
    ;; buffer has no step for the inside of a list.
    ("(a | b)" "b" "a" "(a  b)")
    ("(g \"so|me text\" y)" "\"some text\"" "\"some text\"" "\"some text\""))
  "Cases of a bare point without a parser: the text with `|' at point,
and the selections of `M-n', `M-p' and `M-o'.")

(ert-deftest hel-bare-point-sexps ()
  "Without a parser, the nodes are sexps, and a string is a leaf."
  (pcase-dolist (`(,text ,next ,previous ,expand) hel-node-tests--bare-lisp)
    (should (equal (list text
                         (hel-node-tests--bare #'emacs-lisp-mode text
                                               #'hel-select-next-sibling)
                         (hel-node-tests--bare #'emacs-lisp-mode text
                                               #'hel-select-previous-sibling)
                         (hel-node-tests--bare #'emacs-lisp-mode text
                                               #'hel-expand-selection-backward))
                   (list text next previous expand)))))

(ert-deftest hel-treesit-bare-point ()
  "With a parser, a bare point takes the node on the side of the command."
  (skip-unless (treesit-language-available-p 'python))
  (pcase-dolist (`(,text ,next ,previous ,expand)
                 '(("def g(a: int, b: str|, c): pass\n" "c" "b: str" "str")
                   ("def g(a: int, |b: str, c): pass\n" "b: str" "a: int" "b")
                   ("def g(a: int, b: s|tr, c): pass\n" "str" "str" "str")))
    (should (equal (list text
                         (hel-node-tests--bare #'python-ts-mode text
                                               #'hel-select-next-sibling)
                         (hel-node-tests--bare #'python-ts-mode text
                                               #'hel-select-previous-sibling)
                         (hel-node-tests--bare #'python-ts-mode text
                                               #'hel-expand-selection-backward))
                   (list text next previous expand)))))

(ert-deftest hel-treesit-bare-point-all-siblings ()
  "`M-a' at a bare point starts from the node that touches point."
  (skip-unless (treesit-language-available-p 'python))
  (hel-test-with #'python-ts-mode (hel-test-mark "def g(a: int, b: str, c): pass\n" "")
    ;; Point touches `str' on its left. The comma after it is not a node.
    (goto-char (point-min))
    (search-forward "str")
    (deactivate-mark)
    (hel-select-all-siblings)
    (should (equal (hel-node-tests--all-selections) '("b" "str")))))

(ert-deftest hel-bare-point-all-siblings-between-nodes ()
  "`M-a' where no node touches point selects the nodes around point."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark "(f a  b)\n" "  b")
    (forward-char 1)
    (deactivate-mark)
    (hel-select-all-siblings)
    (should (equal (hel-node-tests--all-selections) '("a" "b" "f")))))

(ert-deftest hel-bare-point-text-object-tie ()
  "When point touches two objects, `mi' takes the one after point."
  (hel-test-with #'text-mode (hel-test-mark "foo bar\n" " bar")
    (forward-char 1)
    (deactivate-mark)
    (hel-mark-inner-word 1)
    (should (equal (hel-test-selection) "bar")))
  (hel-test-with #'text-mode (hel-test-mark "foo.\n" ".")
    (deactivate-mark)
    (hel-mark-inner-word 1)
    (should (equal (hel-test-selection) "."))))

;;; Lisp: sexps as nodes

;; In a Lisp buffer without a parser, a node is a whole sexp, as if
;; a parser gave the sexps as the nodes of the tree. So expand goes from
;; the atom or the string at point to the enclosing lists, whole, with no
;; step for the inside of a list or of a string.

(defconst hel-node-tests--lisp
  "(defun f (x)\n  (let ((y (* x 2)))\n    (g \"some text\" [y] 'sym)))\n")

(defun hel-node-tests--lisp-steps (start command n &optional select)
  "Run COMMAND N times in an Emacs Lisp buffer, return the selections.
Point starts at the first match of START. With SELECT non-nil, the match
is selected first."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-node-tests--lisp start)
    (if select
        (progn (set-mark (point))
               (forward-char (length start)))
      (deactivate-mark))
    (cl-loop repeat n
             do (funcall command 1)
             collect (hel-test-selection))))

(ert-deftest hel-lisp-expand-whole-sexps ()
  "In Lisp, `M-o' goes from sexp to whole sexp."
  (should (equal (hel-node-tests--lisp-steps "x 2" #'hel-expand-selection-backward 3)
                 '("x" "(* x 2)" "(y (* x 2))")))
  (should (equal (hel-node-tests--lisp-steps "g \"" #'hel-expand-selection-backward 2)
                 '("g" "(g \"some text\" [y] 'sym)")))
  (should (equal (hel-node-tests--lisp-steps
                  "(* x 2)" #'hel-expand-selection-backward 2 t)
                 '("(y (* x 2))" "((y (* x 2)))"))))

(ert-deftest hel-lisp-expand-string ()
  "In Lisp, a string is one node: the first step selects it whole."
  (should (equal (hel-node-tests--lisp-steps "text" #'hel-expand-selection-backward 2)
                 '("\"some text\"" "(g \"some text\" [y] 'sym)"))))

(ert-deftest hel-lisp-expand-and-shrink ()
  "`M-i' shrinks back along the path of the Lisp expand."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-node-tests--lisp "x 2")
    (deactivate-mark)
    (hel-expand-selection-backward 3)
    (hel-shrink-selection 2)
    (should (equal (hel-test-selection) "x"))))

(ert-deftest hel-lisp-expand-backward-and-forward ()
  "`M-b' and `M-e' expand as `M-o' does, with point at the start or the end."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-node-tests--lisp "x 2")
    (deactivate-mark)
    (hel-expand-selection-backward 2)
    (should (equal (hel-test-selection) "(* x 2)"))
    (should (= (point) (region-beginning)))
    (hel-expand-selection-forward 1)
    (should (equal (hel-test-selection) "(y (* x 2))"))
    (should (= (point) (region-end)))))

(ert-deftest hel-lisp-sibling-of-prefixed-atom ()
  "Point after the quote of `'sym' lies inside the sexp `'sym'."
  (should (equal (hel-node-tests--lisp-steps
                  "sym" #'hel-select-next-sibling 1)
                 '("'sym"))))

(ert-deftest hel-lisp-prefixed-string-leaf-has-no-children ()
  "A leaf has no children, also a string with a prefix character."
  (hel-test-with #'emacs-lisp-mode "(a '\"foo\" b)|"
    (let* ((producer (hel-tree-syntax-source))
           (leaf (hel-node-enclosing-leaf producer 6)))
      (should (equal leaf '(5 . 10)))
      (should-not (hel-node-children producer leaf)))))

(ert-deftest hel-lisp-comment-is-one-leaf ()
  "In Lisp, a comment of several lines is one leaf.
The line ends between its lines belong to it, and the last one does not."
  (hel-test-with #'emacs-lisp-mode "(a)\n;; one\n;; two\n(b)\n|"
    (let ((producer (hel-tree-syntax-source)))
      (dolist (pos '(7 11 15))
        (ert-info ((format "at %d" pos))
          (should (equal (hel-node-enclosing-leaf producer pos) '(5 . 18)))))
      (should-not (hel-node-enclosing-leaf producer 18)))))

(ert-deftest hel-expand-outside-lisp-keeps-inner-steps ()
  "Outside Lisp, `M-o' still selects the inside of the brackets first."
  (hel-test-with #'c-mode (hel-test-mark "f(a, (b + c));\n" "b +")
    (deactivate-mark)
    (should (equal (hel-node-tests--expand 3)
                   '("b" "b + c" "(b + c)")))))

;;; All siblings and all children

(ert-deftest hel-select-all-children ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-test-select "[1, 2]")
          (hel-select-all-children)
          (should (equal (hel-node-tests--all-selections) '("1" "2"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-select-all-children-of-leaf ()
  "A node without named children keeps the selection."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-node-tests--python "abc")
    (hel-local-mode 1)
    (hel-normal-state)
    (hel-test-select "abc")
    (hel-select-all-children)
    (should (equal (hel-test-selection) "abc"))
    (should-not (hel-all-fake-cursors))))

;;; Node commands without a parser

(defconst hel-node-tests--c-call "int x = f(alpha, beta, gamma);\n")

(ert-deftest hel-sexp-siblings ()
  (hel-test-with #'c-mode (hel-test-mark hel-node-tests--c-call "alpha")
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "alpha"))
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "beta"))
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "gamma"))
    (hel-select-next-sibling 1)
    (should (equal (hel-test-selection) "gamma"))
    (hel-select-previous-sibling 2)
    (should (equal (hel-test-selection) "alpha"))
    (should (= (point) (region-end)))
    (hel-select-previous-sibling 1)
    (should (equal (hel-test-selection) "alpha"))))

;;; Text of the tree command tests

(defconst hel-node-tests--py
  "def f():\n    return g(abc, [1, 2])\nx = 1\n"
  "The Python text of the tree command tests (section Tests).")

(defun hel-node-tests--py (line2 &optional line3)
  "Return `hel-node-tests--py' with LINE2 and LINE3 as the second and third lines."
  (concat "def f():\n    " line2 "\n" (or line3 "x = 1") "\n"))

;;; Expand and shrink with a parser

(ert-deftest hel-expand-from-abc ()
  "Expand from abc gives abc, the argument list, the call and the
return statement (The core picture; Selection rule 1)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(|abc, [1, 2])")
    (should (equal (hel-test-steps #'hel-expand-selection-backward 4)
                   '("abc" "(abc, [1, 2])" "g(abc, [1, 2])"
                     "return g(abc, [1, 2])")))))

(ert-deftest hel-expand-from-partial-region ()
  "Expand from the region `abc, [1' reaches `(abc, [1, 2])'.
Rule 1 and rule 7: expand selects backward."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(^abc, [1|, 2])")
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render)
                   (hel-node-tests--py "return g|(abc, [1, 2])^")))))

(ert-deftest hel-expand-on-bracket ()
  "With the cursor on `[', the first expand selects `[1, 2]'.
Rule 10: M-o starts from the smallest named node that starts at point."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, |[1, 2])")
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) "[1, 2]"))))

(ert-deftest hel-expand-from-selected-bracket-lisp ()
  "In a Lisp mode, with only `(' selected, M-o selects the whole list."
  (hel-test-with #'emacs-lisp-mode "(foo 1 ^(|bar baz))\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) "(bar baz)"))))

(ert-deftest hel-expand-shrink-roundtrip-parser ()
  "Shrink after three expands returns to the starting cursor (rule 3)."
  (hel-test-need 'python)
  (let ((text (hel-node-tests--py "return g(|abc, [1, 2])")))
    (hel-test-with #'python-ts-mode text
      (hel-test-run #'hel-expand-selection-backward 3)
      (should (equal (hel-test-selection) "g(abc, [1, 2])"))
      (hel-test-run #'hel-shrink-selection 3)
      (should-not (use-region-p))
      (should (equal (hel-test-render) text)))))

(ert-deftest hel-expand-shrink-retrace-parser ()
  "Rule 3: each M-i goes back one step along the exact path of M-o."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(|abc, [1, 2])")
    (hel-test-steps #'hel-expand-selection-backward 3)
    (should (equal (hel-test-steps #'hel-shrink-selection 2)
                   '("(abc, [1, 2])" "abc")))))

(ert-deftest hel-shrink-first-named-child ()
  "Shrink without a history selects the first named child (rule 3).
Rule 7: shrink keeps the direction of the selection."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return ^g(abc, [1, 2])|")
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-render) (hel-node-tests--py "return ^g|(abc, [1, 2])")))))

(ert-deftest hel-shrink-first-named-child-backward ()
  "Rule 7: shrink keeps a backward selection backward."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return |g(abc, [1, 2])^")
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-render) (hel-node-tests--py "return |g^(abc, [1, 2])")))))

(ert-deftest hel-shrink-first-named-child-count ()
  "A count repeats shrink without a history (rules 3 and 8)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "^return g(abc, [1, 2])|")
    (hel-test-run #'hel-shrink-selection 2)
    (should (equal (hel-test-render) (hel-node-tests--py "return ^g|(abc, [1, 2])")))))

;;; Siblings with a parser

(ert-deftest hel-next-sibling-skips-comma ()
  "M-n skips commas and puts point at the end (rules 2 and 7)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(^abc|, [1, 2])")
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) (hel-node-tests--py "return g(abc, ^[1, 2]|)")))))

(ert-deftest hel-previous-sibling-skips-comma ()
  "M-p skips commas and keeps the direction (rules 2 and 7)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, ^[1, 2]|)")
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) (hel-node-tests--py "return g(^abc|, [1, 2])")))))

(ert-deftest hel-next-sibling-count ()
  "Rule 8: `3 M-n' moves three siblings forward."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "f(^a|, b, c, d)\n"
    (hel-test-run #'hel-select-next-sibling 3)
    (should (equal (hel-test-render) "f(a, b, c, ^d|)\n"))))

(ert-deftest hel-next-sibling-negative-count ()
  "Rule 8: a negative count reverses the command, so `-1 M-n' acts as M-p."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "f(a, b, c, ^d|)\n"
    (hel-test-run #'hel-select-next-sibling -1)
    (should (equal (hel-test-render) "f(a, b, ^c|, d)\n"))))

(ert-deftest hel-next-sibling-at-last-argument ()
  "M-n at the last argument leaves the selection as it is.
Section Selection rules: a sibling command stays inside the parent."
  (hel-test-need 'python)
  (should (hel-test-unchanged-p #'hel-select-next-sibling
                                (hel-node-tests--py "return g(abc, ^[1, 2]|)")
                                #'python-ts-mode)))

(ert-deftest hel-previous-sibling-through-wrapper ()
  "M-p on `x = 1' selects the function before it, through the
wrapper `expression_statement'."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, [1, 2])" "^x = 1|")
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render)
                   "^def f():\n    return g(abc, [1, 2])|\nx = 1\n"))))

(ert-deftest hel-previous-sibling-through-wrapper-bare ()
  "Rule 10: from a bare point at `x', M-p takes the last child of the
module that ends before point: the function."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, [1, 2])" "|x = 1")
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render)
                   "|def f():\n    return g(abc, [1, 2])^\nx = 1\n"))))

(ert-deftest hel-next-sibling-at-last-statement ()
  "M-n at the last statement leaves the selection as it is."
  (hel-test-need 'python)
  (should (hel-test-unchanged-p #'hel-select-next-sibling
                                (hel-node-tests--py "return g(abc, [1, 2])" "^x = 1|")
                                #'python-ts-mode)))

(ert-deftest hel-sibling-trims-selection ()
  "Rule 10: the node at a selection ignores the whitespace at its edges.
The selection `x ' starts from the identifier x, whose next sibling is 1."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "^x |= 1\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "x = ^1|\n"))))

;;; Rule 10: bare point, with a parser

(defun hel-node-tests--bare-steps (mode text)
  "Return the selections that M-n, M-p and M-o give from TEXT in MODE."
  (mapcar (lambda (command)
            (hel-test-with mode text
              (hel-test-run command)
              (hel-test-selection)))
          (list #'hel-select-next-sibling
                #'hel-select-previous-sibling
                #'hel-expand-selection-backward)))

(ert-deftest hel-bare-point-before-list-parser ()
  "Rule 10 table, row `name |(mode state)', in Python."
  (hel-test-need 'python)
  (should (equal (hel-node-tests--bare-steps #'python-ts-mode "name |(mode, state)\n")
                 '("(mode, state)" "name" "(mode, state)"))))

(ert-deftest hel-bare-point-after-name-parser ()
  "Rule 10 table, row `name| (mode state)', in Python."
  (hel-test-need 'python)
  (should (equal (hel-node-tests--bare-steps #'python-ts-mode "name| (mode, state)\n")
                 '("(mode, state)" "name" "name"))))

(ert-deftest hel-bare-point-in-parameter-parser ()
  "Rule 10 table, row `Callback |_callback,', in C."
  (hel-test-need 'c)
  (should (equal (hel-node-tests--bare-steps #'c-ts-mode
                                       "void f(Callback |_callback, int x);\n")
                 '("_callback" "Callback" "_callback"))))

(ert-deftest hel-bare-point-before-comma-parser ()
  "Rule 10 table, row `Callback _callback|,', in C."
  (hel-test-need 'c)
  (should (equal (hel-node-tests--bare-steps #'c-ts-mode
                                       "void f(Callback _callback|, int x);\n")
                 '("int x" "Callback _callback" "_callback"))))

(ert-deftest hel-bare-point-in-leaf-parser ()
  "Rule 10 table, row `_cal|lback': every command starts from the leaf."
  (hel-test-need 'python)
  (should (equal (hel-node-tests--bare-steps #'python-ts-mode "f(_cal|lback)\n")
                 '("_callback" "_callback" "_callback"))))

(ert-deftest hel-bare-point-between-name-and-call-parser ()
  "Rule 10 table, row `foo|(x)', in Python."
  (hel-test-need 'python)
  (should (equal (hel-node-tests--bare-steps #'python-ts-mode "foo|(x)\n")
                 '("(x)" "foo" "(x)"))))

(ert-deftest hel-bare-point-in-whitespace-parser ()
  "Rule 10 table, row `(a | b)', in a Python tuple: nothing touches point."
  (hel-test-need 'python)
  (should (equal (hel-node-tests--bare-steps #'python-ts-mode "(a, | b)\n")
                 '("b" "a" "(a,  b)"))))

;;; Select all siblings and children with a parser

(ert-deftest hel-select-all-siblings ()
  "M-a on abc puts cursors on abc and [1, 2]."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(a|bc, [1, 2])")
    (hel-test-run #'hel-select-all-siblings)
    (should (equal (hel-test-selections) '("abc" "[1, 2]")))
    (should (null mark-ring))))

(ert-deftest hel-select-all-children-list ()
  "M-I on [1, 2] puts cursors on 1 and 2."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, ^[1, 2]|)")
    (hel-test-run #'hel-select-all-children)
    (should (equal (hel-test-selections) '("1" "2")))
    (should (null mark-ring))))

(ert-deftest hel-select-all-children-skips-wrapper ()
  "M-I on `x = 1' puts cursors on x and 1.  The
`expression_statement' has one child with its own range, so it is skipped."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-node-tests--py "return g(abc, [1, 2])" "^x = 1|")
    (hel-test-run #'hel-select-all-children)
    (should (equal (hel-test-selections) '("x" "1")))))

(ert-deftest hel-select-all-children-leaf ()
  "M-I on a leaf node keeps the selection."
  (hel-test-need 'python)
  (let ((text (hel-node-tests--py "return g(^abc|, [1, 2])")))
    (hel-test-with #'python-ts-mode text
      (hel-test-run #'hel-select-all-children)
      (should (equal (hel-test-render) text))
      (should-not (hel-all-fake-cursors)))))

(ert-deftest hel-select-all-children-no-replay ()
  "M-I on [[1, 2], [3, 4]], run through the replay of Hel for all
cursors, puts cursors on [1, 2] and [3, 4] only (rule 6)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = ^[[1, 2], [3, 4]]|\n"
    (hel-test-run #'hel-select-all-children)
    (should (equal (hel-test-selections) '("[1, 2]" "[3, 4]")))))

;;; Rule 10: bare point, without a parser

(ert-deftest hel-bare-point-before-list-syntax ()
  "Rule 10 table, row `name |(mode state)', without a parser."
  (should (equal (hel-node-tests--bare-steps #'c-mode "g(name |(mode state));\n")
                 '("(mode state)" "name" "(mode state)"))))

(ert-deftest hel-bare-point-after-name-syntax ()
  "Rule 10 table, row `name| (mode state)', without a parser."
  (should (equal (hel-node-tests--bare-steps #'c-mode "g(name| (mode state));\n")
                 '("(mode state)" "name" "name"))))

(ert-deftest hel-bare-point-in-parameter-syntax ()
  "Rule 10 table, row `Callback |_callback,', without a parser."
  (should (equal (hel-node-tests--bare-steps #'c-mode "f(Callback |_callback, int x);\n")
                 '("_callback" "Callback" "_callback"))))

(ert-deftest hel-bare-point-before-comma-syntax ()
  "Rule 10 table, row `Callback _callback|,', without a parser.  The
children of the bracket pair are sexps, so the next one is `int'."
  (should (equal (hel-node-tests--bare-steps #'c-mode "f(Callback _callback|, int x);\n")
                 '("int" "_callback" "_callback"))))

(ert-deftest hel-bare-point-in-leaf-syntax ()
  "Rule 10 table, row `_cal|lback', without a parser.  The syntax table
gives expand steps smaller than a sexp, so M-o selects the word first
\(The syntax producer)."
  (should (equal (hel-node-tests--bare-steps #'c-mode "f(_cal|lback);\n")
                 '("_callback" "_callback" "callback"))))

(ert-deftest hel-bare-point-between-name-and-call-syntax ()
  "Rule 10 table, row `foo|(x)', without a parser: the children of the
buffer are the top-level sexps."
  (should (equal (hel-node-tests--bare-steps #'c-mode "foo|(x);\n")
                 '("(x)" "foo" "(x)"))))

(ert-deftest hel-bare-point-in-whitespace-syntax ()
  "Rule 10 table, row `(a | b)', without a parser.  The inside of a
bracket pair is an expand step of the syntax table, so M-o selects it
before the pair (The syntax producer)."
  (should (equal (hel-node-tests--bare-steps #'c-mode "(a | b);\n")
                 '("b" "a" "a  b"))))

;;; Decisions about extending, the side of point and the Lisp modes

(defmacro hel-node-tests--with-v (mode text &rest body)
  "Run BODY as `hel-test-with' does, with the selection extended, as after `v'."
  (declare (indent 2) (debug t))
  `(hel-test-with ,mode ,text
     (hel-extend-selection 1)
     ,@body))

(ert-deftest hel-v-all-siblings-stops-extending ()
  "After `v', M-a puts a cursor on each sibling and stops extending."
  (hel-node-tests--with-v #'emacs-lisp-mode "(a ^b| c)\n"
    (unwind-protect
        (progn
          (hel-test-run #'hel-select-all-siblings)
          (should-not hel--extend-selection)
          (should (= 2 (length (hel-all-fake-cursors)))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-v-sibling-extends ()
  "Rule 7: after `v', M-n grows the selection and keeps the mark."
  (dolist (mode '(emacs-lisp-mode c-mode))
    (hel-node-tests--with-v mode "(a ^b| c d)\n"
      (hel-test-run #'hel-select-next-sibling)
      (should (equal (hel-test-render) "(a ^b c| d)\n"))
      (hel-test-run #'hel-select-next-sibling)
      (should (equal (hel-test-render) "(a ^b c d|)\n"))
      (hel-test-run #'hel-select-next-sibling)
      (should (equal (hel-test-render) "(a ^b c d|)\n")))))

(ert-deftest hel-v-sibling-gives-back-and-turns ()
  "Rule 7: after `v', a sibling command against the selection gives back its
last node, and at the last node it keeps that node and turns around."
  (hel-node-tests--with-v #'emacs-lisp-mode "(a ^b c| d)\n"
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "(a ^b| c d)\n"))
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "(|a b^ c d)\n")))
  (hel-node-tests--with-v #'emacs-lisp-mode "(a b |c^ d)\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "(a b ^c d|)\n"))))

(ert-deftest hel-v-sibling-parser ()
  "Rule 7: after `v', M-n extends over the named siblings with a parser, and
a node with a dot in it moves as one node."
  (hel-test-need 'python)
  (hel-node-tests--with-v #'python-ts-mode "g(^a|, b.c, d)\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "g(^a, b.c|, d)\n"))
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "g(^a, b.c, d|)\n"))
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "g(^a, b.c|, d)\n"))))

(ert-deftest hel-v-sibling-bare-point ()
  "Rule 7: after `v' at a bare point, M-n extends from point over the node."
  (hel-node-tests--with-v #'emacs-lisp-mode "(a| b c)\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "(a^ b| c)\n"))))

(ert-deftest hel-v-object-keys-create-selections ()
  "Rule 7: `v' works with motions.  `mi' and `ma' create a selection, so they
replace the selection, as without `v'."
  (hel-node-tests--with-v #'text-mode "foo| bar^ baz\n"
    (hel-test-run #'hel-mark-inner-word)
    (should (equal (hel-test-render) "foo ^bar| baz\n")))
  (hel-node-tests--with-v #'emacs-lisp-mode "|(a b^ c)\n"
    (hel-test-run #'hel-mark-inner-paren)
    (should (equal (hel-test-render) "(^a b c|)\n"))))

(ert-deftest hel-expand-point-at-start ()
  "Rule 7: expand puts point at the start of the selection, from a bare point
and from a selection in either direction."
  (hel-test-with #'emacs-lisp-mode "(foo (bar| baz) qux)\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "(foo (|bar^ baz) qux)\n"))
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "(foo |(bar baz)^ qux)\n")))
  (hel-test-with #'emacs-lisp-mode "(foo (^bar| baz) qux)\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "(foo |(bar baz)^ qux)\n")))
  (hel-test-with #'c-mode "f(^ab|c);\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "f(|abc^);\n")))
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "g(^a|, b)\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "g|(a, b)^\n"))))

(ert-deftest hel-sibling-keeps-direction ()
  "Rule 7: M-n and M-p keep the direction of the selection, in both
directions of the motion, over a count, and at the last sibling."
  (hel-test-with #'c-mode "foo(alpha, |beta^, gamma, delta);\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "foo(alpha, beta, |gamma^, delta);\n"))
    (hel-test-run #'hel-select-next-sibling 5)
    (should (equal (hel-test-render) "foo(alpha, beta, gamma, |delta^);\n"))
    (hel-test-run #'hel-select-previous-sibling 2)
    (should (equal (hel-test-render) "foo(alpha, |beta^, gamma, delta);\n")))
  (hel-test-with #'c-mode "foo(alpha, ^beta|, gamma);\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "foo(alpha, beta, ^gamma|);\n"))))

(ert-deftest hel-sibling-after-function-object ()
  "Rule 7: after `mif' point is at the header, and M-n shows the header of
the next function."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "def f():\n    pa|ss\n\ndef g():\n    pass\n"
    (hel-test-run #'hel-mark-inner-function)
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render)
                   "def f():\n    pass\n\n|def g():\n    pass^\n"))))

(ert-deftest hel-sibling-bare-point-direction ()
  "Rule 7: from a bare point, the new selection has the direction of the move."
  (hel-test-with #'emacs-lisp-mode "(a |b c)\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "(a ^b| c)\n")))
  (hel-test-with #'emacs-lisp-mode "(a b| c)\n"
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "(a |b^ c)\n")))
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "g(a, |b)\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "g(a, ^b|)\n")))
  (hel-test-with #'python-ts-mode "g(a|, b)\n"
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "g(|a^, b)\n"))))

(ert-deftest hel-shrink-bare-point ()
  "Rule 3: at a bare point, M-i selects the first child of the node after
point, and does nothing when that node has no children."
  (hel-test-with #'emacs-lisp-mode "(foo |(bar baz) qux)\n"
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-render) "(foo (^bar| baz) qux)\n")))
  (should (hel-test-unchanged-p #'hel-shrink-selection "(|foo bar)\n" #'emacs-lisp-mode))
  (should (hel-test-unchanged-p #'hel-shrink-selection "(foo b|ar)\n" #'emacs-lisp-mode))
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = |[1, 2]\n"
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-render) "x = [^1|, 2]\n"))))

(ert-deftest hel-expand-directions ()
  "M-b expands with point at the start and M-e with point at the end, in
every kind of buffer."
  (hel-test-with #'emacs-lisp-mode "(a (b| c) d)\n"
    (hel-test-run #'hel-expand-selection-forward)
    (should (equal (hel-test-render) "(a (^b| c) d)\n"))
    (hel-test-run #'hel-expand-selection-forward)
    (should (equal (hel-test-render) "(a ^(b c)| d)\n"))
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "|(a (b c) d)^\n")))
  (hel-test-with #'c-mode "f(^abc|, x);\n"
    (hel-test-run #'hel-expand-selection-forward)
    (should (equal (hel-test-render) "f(^abc, x|);\n")))
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "g(^a|, b)\n"
    (hel-test-run #'hel-expand-selection-forward)
    (should (equal (hel-test-render) "g^(a, b)|\n"))))

(ert-deftest hel-lisp-modes ()
  "`hel-use-lisp-nodes': every Lisp mode takes its sexps as nodes, so expand has
no step for the inside of a list."
  (dolist (mode '(lisp-mode scheme-mode lisp-interaction-mode))
    (hel-test-with mode "(foo (bar| baz) qux)\n"
      (should (hel-tree-lisp-producer-p (hel--node-producer)))
      (should (equal (hel-test-steps #'hel-expand-selection-backward 2)
                     '("bar" "(bar baz)")))))
  (hel-test-with #'c-mode "f(a| b);\n"
    (should-not (hel-tree-lisp-producer-p (hel--node-producer)))))

(ert-deftest hel-comment-sibling-lisp ()
  "Rule 10: in Lisp a comment is a leaf, so after M-o selects it, M-n and
M-p go on to the sexps around it."
  (hel-test-with #'emacs-lisp-mode "(w)\n;; com|ment here\n(x)\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) ";; comment here"))
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-selection) "(x)")))
  (hel-test-with #'emacs-lisp-mode "(w)\n;; com|ment here\n(x)\n"
    (hel-test-run #'hel-expand-selection-backward)
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-selection) "(w)"))))

(ert-deftest hel-bare-point-one-space ()
  "Rule 10 table: the rows with one space and the rows with two.  A position
takes up no space, so `(a | b)' has two spaces.  In Lisp, M-o goes from the
node to the list.  The syntax table adds the inside of the brackets."
  (cl-flet ((steps (mode text)
              (hel-test-with mode text
                (hel-test-steps #'hel-expand-selection-backward 2))))
    (should (equal (hel-node-tests--bare-steps #'emacs-lisp-mode "(a| b)\n") '("b" "a" "a")))
    (should (equal (hel-node-tests--bare-steps #'emacs-lisp-mode "(a |b)\n") '("b" "a" "b")))
    (should (equal (steps #'emacs-lisp-mode "(a| b)\n") '("a" "(a b)")))
    (should (equal (steps #'emacs-lisp-mode "(a |b)\n") '("b" "(a b)")))
    (should (equal (hel-node-tests--bare-steps #'emacs-lisp-mode "(a | b)\n")
                   '("b" "a" "(a  b)")))
    (should (equal (steps #'c-mode "(a| b);\n") '("a" "a b")))
    (should (equal (steps #'c-mode "(a |b);\n") '("b" "a b")))
    (should (equal (steps #'c-mode "(a | b);\n") '("a  b" "(a  b)")))))

(ert-deftest hel-bare-point-in-string-syntax ()
  "Rule 10: without a parser, M-n in a string starts from the whole string."
  (hel-test-with #'c-mode "f(\"ab|c\", x);\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-selection) "\"abc\""))))

(ert-deftest hel-select-all-siblings-bare-whitespace ()
  "Rule 10: M-a in `(f a | b)' puts a cursor on f, a and b."
  (hel-test-with #'c-mode "(f a | b);\n"
    (hel-test-run #'hel-select-all-siblings)
    (should (equal (hel-test-selections) '("f" "a" "b")))))

;;; Node commands without a parser

(ert-deftest hel-syntax-next-sibling ()
  "Node commands without a parser: M-n steps over the sexps of the
argument list and stays at the last one."
  (hel-test-with #'c-mode "foo(^alpha|, beta, gamma);\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "foo(alpha, ^beta|, gamma);\n"))
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "foo(alpha, beta, ^gamma|);\n"))
    (hel-test-run #'hel-select-next-sibling)
    (should (equal (hel-test-render) "foo(alpha, beta, ^gamma|);\n"))))

(ert-deftest hel-syntax-previous-sibling ()
  "Node commands without a parser: M-p steps back and stays at the first
sexp."
  (hel-test-with #'c-mode "foo(alpha, ^beta|, gamma);\n"
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "foo(^alpha|, beta, gamma);\n"))
    (hel-test-run #'hel-select-previous-sibling)
    (should (equal (hel-test-render) "foo(^alpha|, beta, gamma);\n"))))

(ert-deftest hel-syntax-select-all-siblings ()
  "Node commands without a parser: M-a puts a cursor on each sexp inside
the argument list."
  (hel-test-with #'c-mode "foo(alpha, be|ta, gamma);\n"
    (hel-test-run #'hel-select-all-siblings)
    (should (equal (hel-test-selections) '("alpha" "beta" "gamma")))))

(ert-deftest hel-syntax-select-all-children ()
  "Node commands without a parser: M-I selects every sexp inside the
bracket pair under the selection."
  (hel-test-with #'c-mode "foo^(alpha, beta)|;\n"
    (hel-test-run #'hel-select-all-children)
    (should (equal (hel-test-selections) '("alpha" "beta")))))

;;; The syntax producer of expand

(defconst hel-node-tests--marked-prose
  "First sentence here. Second one, with (a paren|thetical remark) inside.\n"
  "The prose of section The syntax producer, with point on \"parenthetical\".")

(ert-deftest hel-syntax-expand-prose ()
  "Five M-o in text-mode give the five steps of section The syntax
producer, and a sixth M-o leaves the paragraph selected."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode hel-node-tests--marked-prose
      (should (equal (hel-test-steps #'hel-expand-selection-backward 6)
                     (list "parenthetical"
                           "a parenthetical remark"
                           "(a parenthetical remark)"
                           "Second one, with (a parenthetical remark) inside."
                           (hel-test-text hel-node-tests--marked-prose)
                           (hel-test-text hel-node-tests--marked-prose)))))))

(ert-deftest hel-syntax-expand-double-space ()
  "With `sentence-end-double-space' set to t, the sentence step
spans the whole line without its newline."
  (let ((sentence-end-double-space t))
    (hel-test-with #'text-mode hel-node-tests--marked-prose
      (should (equal (nth 3 (hel-test-steps #'hel-expand-selection-backward 4))
                     (string-trim-right
                      (hel-test-text hel-node-tests--marked-prose)))))))

(ert-deftest hel-syntax-expand-in-comment ()
  "In python-mode the same text in a comment gives no sentence step
and no bracket steps.  Expand goes from the word straight to the whole
comment without the newline."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'python-mode (concat "# " hel-node-tests--marked-prose)
      (let ((steps (hel-test-steps #'hel-expand-selection-backward 2)))
        (should (equal (nth 0 steps) "parenthetical"))
        (should (equal (nth 1 steps)
                       (string-trim-right
                        (concat "# " (hel-test-text hel-node-tests--marked-prose)))))
        (should-not (member "(a parenthetical remark)" steps))
        (should-not (member "a parenthetical remark" steps))))))

(ert-deftest hel-syntax-expand-string ()
  "With point inside a string, expand selects the inside of the
string before the whole string."
  (hel-test-with #'c-mode "f(\"some te|xt\");\n"
    (should (equal (hel-test-steps #'hel-expand-selection-backward 3)
                   '("text" "some text" "\"some text\"")))))

(ert-deftest hel-syntax-expand-on-open-paren ()
  "With the cursor on `(', the first M-o selects the whole form."
  (hel-test-with #'c-mode "f|(x, y);\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) "(x, y)"))))

(ert-deftest hel-syntax-expand-on-open-quote ()
  "With the cursor on an opening quote, the first M-o selects the
whole string."
  (hel-test-with #'c-mode "f(|\"str\");\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) "\"str\""))))

(ert-deftest hel-syntax-expand-elisp-comment ()
  "In emacs-lisp-mode the first M-o selects the whole comment without
its last newline."
  (hel-test-with #'emacs-lisp-mode "(foo)\n;; some wo|rd here\n(bar)\n"
    (should (equal (hel-test-steps #'hel-expand-selection-backward 1)
                   '(";; some word here")))))

(ert-deftest hel-syntax-expand-c-comment ()
  "A comment in c-mode gives the word and then the whole comment."
  (hel-test-with #'c-mode "int x;\n/* some wo|rd here */\nint y;\n"
    (should (equal (hel-test-steps #'hel-expand-selection-backward 2)
                   '("word" "/* some word here */")))))

(ert-deftest hel-syntax-expand-two-brackets ()
  "Expand from a selection that spans two brackets reaches the
smallest bracket pair that contains both, as a whole form."
  (hel-test-with #'c-mode "f((^b), (c|));\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selection) "((b), (c))"))))

(ert-deftest hel-syntax-expand-from-backward ()
  "Rule 7: expand keeps point at the start of a backward selection."
  (hel-test-with #'c-mode "f(|ab^c);\n"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-render) "f(|abc^);\n"))))

(ert-deftest hel-syntax-expand-shrink-roundtrip ()
  "`3 M-o' followed by `3 M-i' returns to the empty selection at the
starting point, and repeated M-i retraces the path of the expands."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode hel-node-tests--marked-prose
      (hel-test-run #'hel-expand-selection-backward 3)
      (should (equal (hel-test-selection) "(a parenthetical remark)"))
      (should (equal (hel-test-steps #'hel-shrink-selection 2)
                     '("a parenthetical remark" "parenthetical")))
      (hel-test-run #'hel-shrink-selection)
      (should-not (use-region-p))
      (should (equal (hel-test-render) hel-node-tests--marked-prose)))))

(ert-deftest hel-syntax-negative-counts ()
  "`-3 M-i' expands three times, and `-3 M-o' shrinks back (rule 8)."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode hel-node-tests--marked-prose
      (hel-test-run #'hel-shrink-selection -3)
      (should (equal (hel-test-selection) "(a parenthetical remark)"))
      (hel-test-run #'hel-expand-selection-backward -3)
      (should-not (use-region-p))
      (should (equal (hel-test-render) hel-node-tests--marked-prose)))))

(ert-deftest hel-syntax-shrink-without-history ()
  "Shrink without a history selects the first sexp inside the
region."
  (hel-test-with #'c-mode "x = ^foo(a) + bar|;\n"
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-render) "x = ^foo|(a) + bar;\n"))))

(ert-deftest hel-syntax-shrink-after-other-command ()
  "Shrink after another command changed the selection selects the
first sexp inside the region (rule 3: the region does not match the
history)."
  (hel-test-with #'c-mode "x = foo(a) + b|ar;\n"
    (hel-test-run #'hel-expand-selection-backward)
    (hel-select (cons 5 15))            ; "foo(a) + b"
    (setq last-command 'ignore)
    (hel-test-run #'hel-shrink-selection)
    (should (equal (hel-test-selection) "foo"))))

(ert-deftest hel-syntax-shrink-no-sexps ()
  "Shrink leaves a region without sexps inside as it is."
  (should (hel-test-unchanged-p #'hel-shrink-selection
                                "x = 1;^   |\ny = 2;\n" #'c-mode)))

(ert-deftest hel-fake-cursor-history ()
  "A fake cursor's expand does not change the history of the main
cursor (rule 4)."
  (hel-test-with #'text-mode "f|oo bar\nbaz qux\n"
    (hel-create-fake-cursor 14)         ; in "qux"
    (hel-test-run #'hel-expand-selection-backward)
    (should (equal (hel-test-selections) '("foo" "qux")))
    (hel-disable-multiple-cursors-mode)
    (hel-test-run #'hel-shrink-selection)
    (should-not (use-region-p))
    (should (= (point) 2))))

;;; Mark ring and command properties (rule 6)

(ert-deftest hel-expand-series-pushes-once ()
  "Rule 6: expand pushes point only at the start of a series of expands
and shrinks, so the series leaves one entry."
  (hel-test-with #'c-mode "f(a, (b|c));\n"
    (hel-test-run #'hel-expand-selection-backward)
    (hel-test-run #'hel-expand-selection-backward)
    (hel-test-run #'hel-shrink-selection)
    (hel-test-run #'hel-expand-selection-backward)
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-shrink-does-not-push ()
  "Rule 6: shrink does not push point to the mark ring."
  (hel-test-with #'c-mode "x = ^foo(a) + bar|;\n"
    (hel-test-run #'hel-shrink-selection)
    (should (null mark-ring))))

(ert-deftest hel-sibling-pushes-point ()
  "Rule 6: a sibling command pushes the old point when it changes the
selection."
  (hel-test-with #'c-mode "foo(^alpha|, beta);\n"
    (hel-test-run #'hel-select-next-sibling)
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-command-properties ()
  "Rule 6: the commands use `:multiple-cursors t', except select all
siblings and select all children, which use `:multiple-cursors nil'."
  (dolist (command '(hel-expand-selection-backward hel-shrink-selection
                                                   hel-expand-selection-forward
                                                   hel-select-next-sibling hel-select-previous-sibling
                                                   hel-mark-inner-function hel-mark-a-function
                                                   hel-mark-function-forward hel-mark-function-backward
                                                   hel-backward-word-end))
    (should (eq t (get command 'multiple-cursors))))
  (dolist (command '(hel-select-all-siblings hel-select-all-children))
    (should (plist-member (symbol-plist command) 'multiple-cursors))
    (should (null (get command 'multiple-cursors)))))

(ert-deftest hel-merge-properties ()
  "Section Selecting an object: mi and ma set `:merge-selections t'.
Section Commands are thin: ]f always merges, and ge merges only when the
selection extends."
  (dolist (command '(hel-mark-inner-function hel-mark-a-function
                                             hel-mark-inner-word hel-mark-a-argument
                                             hel-mark-function-forward))
    (should (eq t (get command 'merge-selections))))
  (should (eq 'hel--extend-selection
              (get 'hel-backward-word-end 'merge-selections))))

(ert-deftest hel-eldoc-registration ()
  "Rule 6: motion commands are registered with `eldoc-add-command'."
  (dolist (command '(hel-select-next-sibling hel-select-previous-sibling
                                             hel-expand-selection-backward
                                             hel-expand-selection-forward))
    (should (intern-soft (symbol-name command) eldoc-message-commands))))

;;; Key bindings

(ert-deftest hel-key-bindings ()
  "Sections Node selection, Text objects and motions, Key bindings: the
keys are plain bindings in `hel-normal-state-map'."
  (pcase-dolist (`(,key . ,command)
                 '(("M-o" . hel-expand-selection-backward)
                   ("M-i" . hel-shrink-selection)
                   ("M-n" . hel-select-next-sibling)
                   ("M-p" . hel-select-previous-sibling)
                   ("M-b" . hel-expand-selection-backward)
                   ("M-e" . hel-expand-selection-forward)
                   ("M-a" . hel-select-all-siblings)
                   ("M-I" . hel-select-all-children)
                   ("M-j" . hel-select-all-children)
                   ("m w" . hel-mark-inner-word)
                   ("m i w" . hel-mark-inner-word)
                   ("m a w" . hel-mark-a-word)
                   ("m W" . hel-mark-inner-WORD)
                   ("m i W" . hel-mark-inner-WORD)
                   ("m a W" . hel-mark-a-WORD)
                   ("m ." . hel-mark-inner-sentence)
                   ("m i s" . hel-mark-inner-sentence)
                   ("m a s" . hel-mark-a-sentence)
                   ("] s" . hel-mark-sentence-forward)
                   ("[ s" . hel-mark-sentence-backward)
                   ("m p" . hel-mark-inner-paragraph)
                   ("m i p" . hel-mark-inner-paragraph)
                   ("m a p" . hel-mark-a-paragraph)
                   ("] p" . hel-forward-paragraph)
                   ("[ p" . hel-backward-paragraph)
                   ("m f" . hel-mark-inner-function)
                   ("m i f" . hel-mark-inner-function)
                   ("m a f" . hel-mark-a-function)
                   ("] f" . hel-mark-function-forward)
                   ("[ f" . hel-mark-function-backward)
                   ("m i t" . hel-mark-inner-class)
                   ("m a t" . hel-mark-a-class)
                   ("] t" . hel-mark-class-forward)
                   ("[ t" . hel-mark-class-backward)
                   ("m i a" . hel-mark-inner-argument)
                   ("m a a" . hel-mark-a-argument)
                   ("] a" . hel-mark-argument-forward)
                   ("[ a" . hel-mark-argument-backward)
                   ("m i c" . hel-mark-inner-comment)
                   ("m a c" . hel-mark-a-comment)
                   ("] c" . hel-mark-comment-forward)
                   ("[ c" . hel-mark-comment-backward)
                   ("}" . hel-forward-paragraph)
                   ("{" . hel-backward-paragraph)))
    (should (eq command (keymap-lookup hel-normal-state-map key)))))

(ert-deftest hel-key-bindings-C-hjkl ()
  "Section Node selection: the C-hjkl keys are bound when
`hel-want-C-hjkl-keys' is non-nil."
  (skip-unless hel-want-C-hjkl-keys)
  (pcase-dolist (`(,key . ,command)
                 '(("C-k" . hel-expand-selection-backward)
                   ("C-j" . hel-shrink-selection)
                   ("C-l" . hel-select-next-sibling)
                   ("C-h" . hel-select-previous-sibling)))
    (should (eq command (keymap-lookup hel-normal-state-map key)))))

(ert-deftest hel-key-bindings-parens ()
  "Section Text objects and motions: `)' and `(' run the function motions
in `prog-mode-map' and the sentence motions in `text-mode-map'."
  (let ((prog (hel-get-nested-hel-keymap prog-mode-map 'normal))
        (text (hel-get-nested-hel-keymap text-mode-map 'normal)))
    (should (eq 'hel-mark-function-forward (keymap-lookup prog ")")))
    (should (eq 'hel-mark-function-backward (keymap-lookup prog "(")))
    (should (eq 'hel-mark-sentence-forward (keymap-lookup text ")")))
    (should (eq 'hel-mark-sentence-backward (keymap-lookup text "(")))))

;;; hel-node-tests.el ends here
