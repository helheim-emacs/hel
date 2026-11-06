;;; hel-structural-editing-tests.el --- Tests for selection by syntax -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The tests of `docs/design/structural-editing.org'. Each part has its own
;; helpers and its own prefix:
;;
;;   hel-spec-         the tests that come from the document alone, written
;;                     without the code. A failure shows a bug or a gap in
;;                     the document;
;;   hel-to-           the text objects, the motions and the laws of the
;;                     protocol;
;;   hel-word-motion-  `w', `e', `b' and their WORD forms;
;;   hel-              the other tests of the commands and the query files.
;;
;; Run from a running Emacs with Hel loaded. The tree-sitter tests skip
;; themselves when a grammar is missing.
;;
;;   (load-file "test/hel-structural-editing-tests.el")
;;   (ert "^hel-")
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

;;; Helpers

(defvar hel-structural-tests--queries
  (expand-file-name "../queries/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The queries directory of the repository.")

(defvar hel-structural-tests--query-cache (make-hash-table :test #'eq)
  "The compiled queries of the repository, shared by the tests.
Compiling a query takes much longer than a test.")

(defconst hel-structural-tests--python
  "def f():\n    return g(abc, [1, 2])\n\nx = 1\n")

(defmacro hel-structural-tests--with-python (start &rest body)
  "Run BODY in a `python-ts-mode' buffer, point at the first match of START.
The buffer holds `hel-structural-tests--python'. The window commands that
need a displayed buffer do nothing."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'python))
     (cl-letf (((symbol-function 'hel-reveal-point-when-on-top) #'ignore)
               ((symbol-function 'hel-push-point) #'ignore))
       (with-temp-buffer
         (let ((transient-mark-mode t))
           (python-ts-mode)
           (insert hel-structural-tests--python)
           (goto-char (point-min))
           (search-forward ,start)
           (goto-char (match-beginning 0))
           ,@body)))))

(defun hel-structural-tests--selection ()
  "Return the selected text, or :none without an active region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    :none))

(defun hel-structural-tests--select (text)
  "Select the first match of TEXT forward."
  (goto-char (point-min))
  (search-forward text)
  (set-mark (match-beginning 0)))

(defun hel-structural-tests--all-selections ()
  "Return the texts of the main and the fake selections, sorted."
  (sort (cons (hel-structural-tests--selection)
              (-map (lambda (cursor)
                      (let ((point (overlay-get cursor 'point))
                            (mark (overlay-get cursor 'mark)))
                        (buffer-substring-no-properties (min point mark)
                                                        (max point mark))))
                    (hel-all-fake-cursors)))
        #'string<))

(defmacro hel-structural-tests--with-buffer (mode text start &rest body)
  "Run BODY in a MODE buffer with TEXT, point at the first match of START.
The window commands that need a displayed buffer do nothing."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'hel-reveal-point-when-on-top) #'ignore)
             ((symbol-function 'hel-push-point) #'ignore))
     (with-temp-buffer
       (let ((transient-mark-mode t)
             (hel-treesit-queries-directory hel-structural-tests--queries)
             (hel-treesit--queries hel-structural-tests--query-cache))
         (funcall ,mode)
         (insert ,text)
         (goto-char (point-min))
         (search-forward ,start)
         (goto-char (match-beginning 0))
         ,@body))))

(defun hel-structural-tests--expand (n)
  "Expand N times and return the selection after each step."
  (let (result)
    (dotimes (_ n)
      (hel-expand-selection 1)
      (push (hel-structural-tests--selection) result))
    (nreverse result)))

(defconst hel-structural-tests--prose
  "First sentence here. Second one, with (a parenthetical remark) inside.\n")

;;; Expand and shrink: syntax table

(ert-deftest hel-expand-text-mode-steps ()
  (hel-structural-tests--with-buffer #'text-mode
      (concat hel-structural-tests--prose "\nNext paragraph.\n")
      "parenth"
    (let ((sentence-end-double-space nil))
      (should (equal (hel-structural-tests--expand 6)
                     '("parenthetical"
                       "a parenthetical remark"
                       "(a parenthetical remark)"
                       "Second one, with (a parenthetical remark) inside."
                       "First sentence here. Second one, with (a parenthetical remark) inside.\n"
                       "First sentence here. Second one, with (a parenthetical remark) inside.\n"))))))

(ert-deftest hel-expand-sentence-double-space ()
  "With `sentence-end-double-space', one space does not end the sentence."
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--prose "parenth"
    (let ((sentence-end-double-space t))
      (should (equal (nth 3 (hel-structural-tests--expand 4))
                     (string-trim-right hel-structural-tests--prose))))))

(ert-deftest hel-expand-comment-in-prog-mode ()
  "A comment gives no sentence step, and its brackets give no steps."
  (hel-structural-tests--with-buffer #'python-mode
      (concat "x = 1  # " hel-structural-tests--prose)
      "parenth"
    (let ((prose (string-trim-right hel-structural-tests--prose)))
      (should (equal (hel-structural-tests--expand 3)
                     (list "parenthetical"
                           prose
                           (concat "# " prose)))))))

(ert-deftest hel-expand-string-inside-first ()
  (hel-structural-tests--with-buffer #'c-mode
      "(g \"some text\" [y])\n" "text"
    (should (equal (hel-structural-tests--expand 4)
                   '("text" "some text" "\"some text\"" "g \"some text\" [y]")))))

(ert-deftest hel-expand-on-opening-delimiter ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      "(g \"some text\" (h x))\n" "\"some"
    (should (equal (hel-structural-tests--expand 1) '("\"some text\""))))
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      "(g \"some text\" (h x))\n" "(h"
    (should (equal (hel-structural-tests--expand 1) '("(h x)")))))

(ert-deftest hel-expand-comments ()
  (hel-structural-tests--with-buffer #'python-mode
      "# hello world\nx = 1\n" "world"
    (should (equal (hel-structural-tests--expand 3)
                   '("world" "hello world" "# hello world"))))
  (hel-structural-tests--with-buffer #'c-mode
      "int x; /* hello world */\n" "world"
    (should (equal (hel-structural-tests--expand 3)
                   '("world" "hello world" "/* hello world */")))))

(ert-deftest hel-expand-two-brackets ()
  "A selection over two bracket pairs expands to the pair around both."
  (hel-structural-tests--with-buffer #'c-mode "(f (a b) [c d])\n" "b)"
    (set-mark (point))
    (search-forward "[c")
    (should (equal (hel-structural-tests--expand 2)
                   '("f (a b) [c d]" "(f (a b) [c d])")))))

;;; Expand and shrink: driver

(ert-deftest hel-expand-keeps-direction ()
  (hel-structural-tests--with-buffer #'c-mode "(foo bar)" "foo"
    (set-mark 5)
    (hel-expand-selection 1)
    (should (equal (hel-structural-tests--selection) "foo bar"))
    (should (= (point) 2))))

(ert-deftest hel-expand-count-and-shrink ()
  (hel-structural-tests--with-buffer #'c-mode "(a (b (c d)))" " d"
    ;; Point touches `c', so the first step selects it.
    (hel-expand-selection 3)
    (should (equal (hel-structural-tests--selection) "(c d)"))
    (hel-shrink-selection 3)
    (should (eq (hel-structural-tests--selection) :none))
    (should (= (point) 9))))

(ert-deftest hel-expand-negative-counts ()
  "A negative count of shrink expands, and a negative count of expand
shrinks."
  (hel-structural-tests--with-buffer #'c-mode "(a (b (c d)))" " d"
    (hel-shrink-selection -3)
    (should (equal (hel-structural-tests--selection) "(c d)"))
    (hel-expand-selection -3)
    (should (eq (hel-structural-tests--selection) :none))
    (should (= (point) 9))))

(ert-deftest hel-expand-shrink-retraces-path ()
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--prose "parenth"
    (let ((sentence-end-double-space nil)
          (steps (hel-structural-tests--expand 4))
          back)
      (dotimes (_ 4)
        (hel-shrink-selection 1)
        (push (hel-structural-tests--selection) back))
      (should (equal (nreverse back)
                     (append (cdr (reverse steps)) '(:none)))))))

(ert-deftest hel-expand-shrink-without-history ()
  (hel-structural-tests--with-buffer #'text-mode "abc def" "abc"
    (set-mark 1)
    (goto-char 4)
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "abc"))))

(ert-deftest hel-expand-shrink-first-sexp ()
  "Without a history, shrink selects the first sexp inside the region."
  (hel-structural-tests--with-buffer #'text-mode "abc def" "abc"
    (set-mark 1)
    (goto-char 8)
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "abc")))
  (hel-structural-tests--with-buffer #'emacs-lisp-mode "(foo bar)" "foo"
    (set-mark 1)
    (goto-char 10)
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "foo"))
    (should (= (point) 5))))

(ert-deftest hel-expand-shrink-after-other-motion ()
  "Shrink does nothing once the selection differs from the last expand."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode "(foo bar)" "foo"
    (hel-expand-selection 1)
    (goto-char 6)
    (set-mark 9)
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "bar"))
    (should-not hel-selection-history)))

(ert-deftest hel-expand-history-per-cursor ()
  (should (memq 'hel-selection-history hel-fake-cursor-variables))
  (hel-structural-tests--with-buffer #'emacs-lisp-mode "(f (a x) (b y))" "a x"
    (hel-local-mode 1)
    (hel-normal-state)
    (let ((fake (hel-create-fake-cursor (save-excursion
                                          (search-forward "b y")
                                          (match-beginning 0)))))
      (unwind-protect
          (progn
            (hel-expand-selection 1)
            (hel-save-excursion
              (hel-with-fake-cursor fake
                (hel-expand-selection 1)))
            (hel-shrink-selection 1)
            (should (eq (hel-structural-tests--selection) :none))
            (should (equal (buffer-substring (point) (1+ (point))) "a")))
        (hel-remove-all-fake-cursors)))))

;;; Expand and shrink: tree-sitter

(ert-deftest hel-treesit-expand-steps ()
  (hel-structural-tests--with-python "abc"
    (let (steps)
      (dotimes (_ 4)
        (hel-expand-selection 1)
        (push (hel-structural-tests--selection) steps))
      (should (equal (nreverse steps)
                     '("abc"
                       "(abc, [1, 2])"
                       "g(abc, [1, 2])"
                       "return g(abc, [1, 2])"))))))

(ert-deftest hel-treesit-expand-from-region ()
  "Expand from a region that crosses node borders reaches the smallest node
that covers it."
  (hel-structural-tests--with-python "abc"
    (hel-structural-tests--select "abc, [1")
    (hel-expand-selection 1)
    (should (equal (hel-structural-tests--selection) "(abc, [1, 2])"))))

(ert-deftest hel-treesit-expand-on-bracket ()
  "With the cursor on a bracket, the first expand selects its node."
  (hel-structural-tests--with-python "[1"
    (hel-expand-selection 1)
    (should (equal (hel-structural-tests--selection) "[1, 2]"))))

(ert-deftest hel-treesit-shrink-retraces-path ()
  (hel-structural-tests--with-python "abc"
    (hel-expand-selection 3)
    (hel-shrink-selection 3)
    (should (eq (hel-structural-tests--selection) :none))
    (should (looking-at "abc"))))

(ert-deftest hel-treesit-shrink-without-history ()
  "Without a history, shrink selects the first named child."
  (hel-structural-tests--with-python "abc"
    (hel-structural-tests--select "(abc, [1, 2])")
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "abc"))
    (hel-structural-tests--select "[1, 2]")
    (hel-shrink-selection 2)
    (should (equal (hel-structural-tests--selection) "1"))))

;;; Siblings

(ert-deftest hel-treesit-siblings-skip-commas ()
  (hel-structural-tests--with-python "abc"
    ;; Without a selection, the first press selects the node after point.
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "abc"))
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "[1, 2]"))
    (should (= (point) (region-end)))
    (hel-select-previous-sibling 1)
    (should (equal (hel-structural-tests--selection) "abc"))
    (should (= (point) (region-end)))))

(ert-deftest hel-treesit-sibling-stays-in-parent ()
  "At the last argument, the selection does not change."
  (hel-structural-tests--with-python "abc"
    (hel-structural-tests--select "[1, 2]")
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "[1, 2]"))))

(ert-deftest hel-treesit-sibling-of-wrapper ()
  "The `assignment' of `x = 1' has no siblings, but its
`expression_statement' with the same range has."
  (hel-structural-tests--with-python "x = 1"
    (hel-structural-tests--select "x = 1")
    (hel-select-previous-sibling 1)
    (should (equal (hel-structural-tests--selection)
                   "def f():\n    return g(abc, [1, 2])"))))

(ert-deftest hel-treesit-sibling-from-word-with-space ()
  "A selection with a trailing space, as `w' makes it, starts from the
node of the word."
  (hel-structural-tests--with-python "x = 1"
    (hel-structural-tests--select "x ")
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "1"))))

(ert-deftest hel-treesit-sibling-count ()
  (hel-structural-tests--with-python "1,"
    (hel-select-next-sibling 2)
    (should (equal (hel-structural-tests--selection) "2"))
    (hel-select-previous-sibling 1)
    (should (equal (hel-structural-tests--selection) "1"))))

(ert-deftest hel-treesit-sibling-at-the-end ()
  "Without any next sibling, the selection does not change."
  (hel-structural-tests--with-python "x = 1"
    (hel-structural-tests--select "x = 1")
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "x = 1"))))

;;; Point without a selection

;; Rule 10 of the design: without a selection, point is a position. A
;; command with a direction takes the node on its side of point, and
;; expand takes the node that touches point, the one after point first.

(defun hel-structural-tests--bare (mode point-text command)
  "Run COMMAND once from a bare point in a MODE buffer, return the selection.
POINT-TEXT is the text of the buffer with a `|' at the position of point."
  (hel-structural-tests--with-buffer mode (string-replace "|" "" point-text) ""
    (goto-char (1+ (string-search "|" point-text)))
    (deactivate-mark)
    (funcall command 1)
    (hel-structural-tests--selection)))

(defconst hel-structural-tests--bare-lisp
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
  "Without a parser, the nodes are sexps, and a string is a token."
  (pcase-dolist (`(,text ,next ,previous ,expand) hel-structural-tests--bare-lisp)
    (should (equal (list text
                         (hel-structural-tests--bare #'emacs-lisp-mode text
                                                     #'hel-select-next-sibling)
                         (hel-structural-tests--bare #'emacs-lisp-mode text
                                                     #'hel-select-previous-sibling)
                         (hel-structural-tests--bare #'emacs-lisp-mode text
                                                     #'hel-expand-selection))
                   (list text next previous expand)))))

(ert-deftest hel-treesit-bare-point ()
  "With a parser, a bare point takes the node on the side of the command."
  (skip-unless (treesit-language-available-p 'python))
  (pcase-dolist (`(,text ,next ,previous ,expand)
                 '(("def g(a: int, b: str|, c): pass\n" "c" "b: str" "str")
                   ("def g(a: int, |b: str, c): pass\n" "b: str" "a: int" "b")
                   ("def g(a: int, b: s|tr, c): pass\n" "str" "str" "str")))
    (should (equal (list text
                         (hel-structural-tests--bare #'python-ts-mode text
                                                     #'hel-select-next-sibling)
                         (hel-structural-tests--bare #'python-ts-mode text
                                                     #'hel-select-previous-sibling)
                         (hel-structural-tests--bare #'python-ts-mode text
                                                     #'hel-expand-selection))
                   (list text next previous expand)))))

(ert-deftest hel-treesit-bare-point-parent-start ()
  "`M-b' at a bare point starts from the node before point."
  (skip-unless (treesit-language-available-p 'python))
  (should (equal (hel-structural-tests--bare #'python-ts-mode
                                             "g(abc|, [1, 2])\n"
                                             #'hel-parent-node-start)
                 "abc")))

(ert-deftest hel-treesit-bare-point-all-siblings ()
  "`M-a' at a bare point starts from the node that touches point."
  (skip-unless (treesit-language-available-p 'python))
  (hel-structural-tests--with-buffer #'python-ts-mode
      "def g(a: int, b: str, c): pass\n" ""
    ;; Point touches `str' on its left. The comma after it is not a node.
    (goto-char (point-min))
    (search-forward "str")
    (deactivate-mark)
    (hel-select-all-siblings)
    (should (equal (hel-structural-tests--all-selections) '("b" "str")))))

(ert-deftest hel-bare-point-all-siblings-between-nodes ()
  "`M-a' where no node touches point selects the nodes around point."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode "(f a  b)\n" "  b"
    (forward-char 1)
    (deactivate-mark)
    (hel-select-all-siblings)
    (should (equal (hel-structural-tests--all-selections) '("a" "b" "f")))))

(ert-deftest hel-bare-point-text-object-tie ()
  "When point touches two objects, `mi' takes the one after point."
  (hel-structural-tests--with-buffer #'text-mode "foo bar\n" " bar"
    (forward-char 1)
    (deactivate-mark)
    (hel-mark-inner-word 1)
    (should (equal (hel-structural-tests--selection) "bar")))
  (hel-structural-tests--with-buffer #'text-mode "foo.\n" "."
    (deactivate-mark)
    (hel-mark-inner-word 1)
    (should (equal (hel-structural-tests--selection) "."))))

;;; Lisp: sexps as nodes

;; In a Lisp buffer without a parser, a node is a whole sexp, as if
;; a parser gave the sexps as the nodes of the tree. So expand goes from
;; the atom or the string at point to the enclosing lists, whole, with no
;; step for the inside of a list or of a string.

(defconst hel-structural-tests--lisp
  "(defun f (x)\n  (let ((y (* x 2)))\n    (g \"some text\" [y] 'sym)))\n")

(defun hel-structural-tests--lisp-steps (start command n &optional select)
  "Run COMMAND N times in an Emacs Lisp buffer, return the selections.
Point starts at the first match of START. With SELECT non-nil, the match
is selected first."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--lisp start
    (if select
        (progn (set-mark (point))
               (goto-char (match-end 0)))
      (deactivate-mark))
    (cl-loop repeat n
             do (funcall command 1)
             collect (hel-structural-tests--selection))))

(ert-deftest hel-lisp-expand-whole-sexps ()
  "In Lisp, `M-o' goes from sexp to whole sexp."
  (should (equal (hel-structural-tests--lisp-steps "x 2" #'hel-expand-selection 3)
                 '("x" "(* x 2)" "(y (* x 2))")))
  (should (equal (hel-structural-tests--lisp-steps "g \"" #'hel-expand-selection 2)
                 '("g" "(g \"some text\" [y] 'sym)")))
  (should (equal (hel-structural-tests--lisp-steps
                  "(* x 2)" #'hel-expand-selection 2 t)
                 '("(y (* x 2))" "((y (* x 2)))"))))

(ert-deftest hel-lisp-expand-string ()
  "In Lisp, a string is one node: the first step selects it whole."
  (should (equal (hel-structural-tests--lisp-steps "text" #'hel-expand-selection 2)
                 '("\"some text\"" "(g \"some text\" [y] 'sym)"))))

(ert-deftest hel-lisp-expand-and-shrink ()
  "`M-i' shrinks back along the path of the Lisp expand."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--lisp "x 2"
    (deactivate-mark)
    (hel-expand-selection 3)
    (hel-shrink-selection 2)
    (should (equal (hel-structural-tests--selection) "x"))))

(ert-deftest hel-lisp-expand-backward-and-forward ()
  "`H' and `L' expand as `M-o' does, with point at the start or the end."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--lisp "x 2"
    (deactivate-mark)
    (hel-select-node-start 2)
    (should (equal (hel-structural-tests--selection) "(* x 2)"))
    (should (= (point) (region-beginning)))
    (hel-select-node-end 1)
    (should (equal (hel-structural-tests--selection) "(y (* x 2))"))
    (should (= (point) (region-end)))))

(ert-deftest hel-lisp-sibling-of-prefixed-atom ()
  "Point after the quote of `'sym' lies inside the sexp `'sym'."
  (should (equal (hel-structural-tests--lisp-steps
                  "sym" #'hel-select-next-sibling 1)
                 '("'sym"))))

(ert-deftest hel-expand-outside-lisp-keeps-inner-steps ()
  "Outside Lisp, `M-o' still selects the inside of the brackets first."
  (hel-structural-tests--with-buffer #'c-mode "f(a, (b + c));\n" "b +"
    (deactivate-mark)
    (should (equal (hel-structural-tests--expand 3)
                   '("b" "b + c" "(b + c)")))))

;;; Parent start and end

(ert-deftest hel-parent-node-end ()
  (hel-structural-tests--with-python "1,"
    (let (ends)
      (dotimes (_ 3)
        (hel-parent-node-end 1)
        (push (buffer-substring-no-properties (line-beginning-position) (point))
              ends))
      (should (equal (nreverse ends)
                     '("    return g(abc, [1"
                       "    return g(abc, [1, 2]"
                       "    return g(abc, [1, 2])"))))))

(ert-deftest hel-parent-node-start ()
  (hel-structural-tests--with-python "2]"
    (let (starts)
      (dotimes (_ 3)
        (hel-parent-node-start 1)
        (push (buffer-substring-no-properties (point) (line-end-position))
              starts))
      (should (equal (nreverse starts)
                     '("[1, 2])"
                       "(abc, [1, 2])"
                       "g(abc, [1, 2])"))))))

(ert-deftest hel-treesit-parent-node-from-forward-selection ()
  "The node under the cursor of a forward selection holds its last
character, not the character after point."
  (hel-structural-tests--with-python "abc"
    (hel-structural-tests--select "abc")
    (hel-parent-node-start 1)
    (should (equal (hel-structural-tests--selection) "abc"))
    (should (= (point) (region-beginning))))
  (hel-structural-tests--with-python "abc"
    (hel-structural-tests--select "abc")
    (hel-parent-node-end 1)
    (should (equal (hel-structural-tests--selection) ", [1, 2])"))))

(ert-deftest hel-treesit-parent-node-selects-from-point ()
  (hel-structural-tests--with-python "bc"
    (hel-parent-node-end 1)
    (should (equal (hel-structural-tests--selection) "bc"))))

;;; All siblings and all children

(ert-deftest hel-select-all-siblings ()
  (hel-structural-tests--with-python "abc"
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-select-all-siblings)
          (should (equal (hel-structural-tests--all-selections)
                         '("[1, 2]" "abc"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-select-all-children ()
  (hel-structural-tests--with-python "abc"
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-structural-tests--select "[1, 2]")
          (hel-select-all-children)
          (should (equal (hel-structural-tests--all-selections) '("1" "2"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-select-all-children-new-cursors-do-not-repeat ()
  "Hel runs the command once for each cursor that existed before it.
The cursors that `M-I' creates must not select their own children."
  (hel-structural-tests--with-buffer #'python-ts-mode
      "f([[1, 2], [3, 4]])\n" "[["
    (skip-unless (treesit-language-available-p 'python))
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-structural-tests--select "[[1, 2], [3, 4]]")
          (hel-execute-command-for-all-cursors 'hel-select-all-children)
          (should (equal (hel-structural-tests--all-selections)
                         '("[1, 2]" "[3, 4]"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-select-all-children-skips-wrapper ()
  "The `expression_statement' around `x = 1' has one child with its range.
`treesit-node-on' returns that child, so its children get selected."
  (hel-structural-tests--with-python "x = 1"
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-structural-tests--select "x = 1")
          (hel-select-all-children)
          (should (equal (hel-structural-tests--all-selections) '("1" "x"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-select-all-children-of-leaf ()
  "A node without named children keeps the selection."
  (hel-structural-tests--with-python "abc"
    (hel-local-mode 1)
    (hel-normal-state)
    (hel-structural-tests--select "abc")
    (hel-select-all-children)
    (should (equal (hel-structural-tests--selection) "abc"))
    (should-not (hel-all-fake-cursors))))

;;; Node commands without a parser

(defconst hel-structural-tests--c-call "int x = f(alpha, beta, gamma);\n")

(ert-deftest hel-sexp-siblings ()
  (hel-structural-tests--with-buffer #'c-mode hel-structural-tests--c-call "alpha"
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "alpha"))
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "beta"))
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "gamma"))
    (hel-select-next-sibling 1)
    (should (equal (hel-structural-tests--selection) "gamma"))
    (hel-select-previous-sibling 2)
    (should (equal (hel-structural-tests--selection) "alpha"))
    (should (= (point) (region-end)))
    (hel-select-previous-sibling 1)
    (should (equal (hel-structural-tests--selection) "alpha"))))

(ert-deftest hel-sexp-select-all-siblings ()
  (hel-structural-tests--with-buffer #'c-mode hel-structural-tests--c-call "beta"
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-select-all-siblings)
          (should (equal (hel-structural-tests--all-selections)
                         '("alpha" "beta" "gamma"))))
      (hel-remove-all-fake-cursors))))

(ert-deftest hel-sexp-select-all-children ()
  (hel-structural-tests--with-buffer #'c-mode hel-structural-tests--c-call "(alpha"
    (hel-local-mode 1)
    (hel-normal-state)
    (unwind-protect
        (progn
          (hel-structural-tests--select "(alpha, beta, gamma)")
          (hel-select-all-children)
          (should (equal (hel-structural-tests--all-selections)
                         '("alpha" "beta" "gamma"))))
      (hel-remove-all-fake-cursors))))

;;; Text objects

(defconst hel-structural-tests--objects
  "# first
# second

class A:
    def f(self, a, b):
        return g(a, b)

    def h(self):
        pass

# Header of top.
def top(x):
    return x

def last():
    pass
")

(defmacro hel-structural-tests--with-objects (mode text start &rest body)
  "Run BODY in a MODE buffer with TEXT, point at the first match of START.
Skip the test when the text objects do not work there."
  (declare (indent 3))
  `(hel-structural-tests--with-buffer ,mode ,text ,start
     (skip-unless (hel-treesit-text-objects-p))
     ,@body))

(defun hel-structural-tests--first-line ()
  "Return the first line of the selection."
  (car (split-string (hel-structural-tests--selection) "\n")))

(defun hel-structural-tests--goto (text)
  "Deactivate the mark and move point to the first match of TEXT."
  (deactivate-mark)
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(ert-deftest hel-function-object ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return g"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--selection)
                   "def f(self, a, b):\n        return g(a, b)"))
    (should (= (point) (region-beginning)))
    (hel-structural-tests--goto "return g")
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "def f(self, a, b):\n        return g(a, b)\n\n    "))
    (should (= (point) (region-beginning)))))

(ert-deftest hel-function-object-header-comment ()
  "The around range takes the header comment above the function, and
leaves out the header comment of the next one."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return x"
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "# Header of top.\ndef top(x):\n    return x\n\n")))
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "def h"
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "\n\n    def h(self):\n        pass")))
  (hel-structural-tests--with-objects #'python-ts-mode
      "# Header of f.\ndef f():\n    pass\n\n# Header of g.\ndef g():\n    pass\n"
      "pass"
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "# Header of f.\ndef f():\n    pass\n\n"))))

(defconst hel-structural-tests--decorated
  "# Header.
@dec
def f():
    def inner():
        pass

@dec
class C:
    pass
")

(ert-deftest hel-function-object-decorated ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--decorated "def f"
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "# Header.\n@dec\ndef f():\n    def inner():\n        pass"))
    (hel-structural-tests--goto "class C")
    (hel-mark-a-class 1)
    (should (equal (hel-structural-tests--selection) "@dec\nclass C:\n    pass"))))

(ert-deftest hel-function-object-nested ()
  "A nested function that ends where its outer function ends is a separate
object, also when the outer function is decorated."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--decorated "pass"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--selection)
                   "def inner():\n        pass")))
  (hel-structural-tests--with-objects #'python-ts-mode
      "def f():\n    def inner():\n        pass\n" "pass"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--selection)
                   "def inner():\n        pass"))))

(ert-deftest hel-object-in-gaps ()
  (hel-structural-tests--with-objects #'python-ts-mode "f(a, b)\n" ", b"
    (hel-mark-inner-argument 1)
    (should (equal (hel-structural-tests--selection) "a")))
  (hel-structural-tests--with-objects #'python-ts-mode "f(a, b, c)\n" ", c"
    (hel-mark-inner-argument 1)
    (should (equal (hel-structural-tests--selection) "b")))
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "\n\n    def h"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--first-line) "def f(self, a, b):"))))

(ert-deftest hel-object-on-header-comment ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "Header of top"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--first-line) "def top(x):"))
    (hel-structural-tests--goto "Header of top")
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection) "# Header of top.\n"))
    (hel-structural-tests--goto "Header of top")
    (hel-mark-inner-comment 1)
    (should (equal (hel-structural-tests--selection) "# Header of top.\n"))))

(ert-deftest hel-function-object-on-signature ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "def f"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--first-line) "def f(self, a, b):"))))

(ert-deftest hel-object-push-point ()
  "A text object pushes point only when it selects."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "class A"
    (let ((pushes 0))
      (cl-letf (((symbol-function 'hel-push-point)
                 (lambda (&rest _) (cl-incf pushes))))
        (hel-mark-inner-argument 1)
        (should (= pushes 0))
        (should (eq (hel-structural-tests--selection) :none))
        (hel-structural-tests--goto "self, a")
        (hel-mark-a-argument 1)
        (should (= pushes 1))
        (should (equal (hel-structural-tests--selection) "self, "))))))

(ert-deftest hel-class-object ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return g"
    (hel-mark-inner-class 1)
    (should (equal (hel-structural-tests--first-line) "class A:"))))

(ert-deftest hel-argument-object ()
  (hel-structural-tests--with-objects #'python-ts-mode "f(a, b, c)\n" "a,"
    (hel-mark-a-argument 1)
    (should (equal (hel-structural-tests--selection) "a, "))
    (hel-structural-tests--goto "c)")
    (hel-mark-a-argument 1)
    (should (equal (hel-structural-tests--selection) ", c"))
    (dolist (arg '("a" "b" "c"))
      (hel-structural-tests--goto (if (equal arg "c") "c)" (concat arg ",")))
      (hel-mark-inner-argument 1)
      (should (equal (hel-structural-tests--selection) arg)))))

(ert-deftest hel-argument-object-delete ()
  "`maa' and `d' leave a list without a stray comma."
  (dolist (spec '(("a," "f(b, c)\n") ("c)" "f(a, b)\n")))
    (hel-structural-tests--with-objects #'python-ts-mode "f(a, b, c)\n" (car spec)
      (hel-mark-a-argument 1)
      (delete-region (region-beginning) (region-end))
      (should (equal (buffer-string) (cadr spec))))))

(ert-deftest hel-comment-object ()
  "A run of comment lines covers whole lines, as a paragraph does."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "first"
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection) "# first\n# second\n"))
    (hel-structural-tests--goto "second")
    (hel-mark-inner-comment 1)
    (should (equal (hel-structural-tests--selection) "# first\n# second\n"))))

(ert-deftest hel-comment-object-gap ()
  "`mac' takes the blank lines up to the next comment run."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      ";; a\n;; b\n\n;; c\n" "a"
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection) ";; a\n;; b\n\n"))
    (hel-structural-tests--goto "c")
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection) "\n;; c\n"))))

(ert-deftest hel-comment-object-found-from-position ()
  "`mic' and `mac' find a comment from every position that belongs to it."
  (pcase-dolist (`(,mode ,text ,positions ,inner ,around)
                 `(;; The line ends of a run.
                   (emacs-lisp-mode ";; a\n;; b\n\n(c)\n" (5 10)
                                    ";; a\n;; b\n" ";; a\n;; b\n")
                   ;; The blank line at the end edge of a run.
                   (emacs-lisp-mode ";; a\n;; b\n\n(c)\n" (11)
                                    ";; a\n;; b\n" ";; a\n;; b\n")
                   ;; The blank lines of the gap between two runs.
                   (emacs-lisp-mode ";; a\n\n\n;; c\n" (5 6 7)
                                    ";; a\n" ";; a\n\n\n")
                   ;; The blanks after a comment.
                   (emacs-lisp-mode ";; a  \n(c)\n" (5 6 7)
                                    ";; a  \n" ";; a  \n")
                   ;; The line end of a comment after code.
                   (emacs-lisp-mode "(foo)  ; note\n(bar)\n" (14)
                                    "; note" "  ; note")
                   ;; No line end after the comment.
                   (emacs-lisp-mode ";; a" (1 3 5) ";; a" ";; a")
                   ;; An unterminated comment.
                   (c-mode "int x;\n/* open" (8 9 11 15) "/* open" "/* open")))
    (dolist (position positions)
      (ert-info ((format "%S at %d" text position))
        (hel-structural-tests--with-buffer mode text ""
          (goto-char position)
          (hel-mark-inner-comment 1)
          (should (equal (hel-structural-tests--selection) inner))
          (deactivate-mark)
          (goto-char position)
          (hel-mark-a-comment 1)
          (should (equal (hel-structural-tests--selection) around)))))))

(ert-deftest hel-comment-object-not-found ()
  "`mic' selects nothing where no comment or gap of a comment holds point."
  (pcase-dolist (`(,text ,position)
                 '(("(a)\n\n(c)\n" 4)
                   ("(a)\n\n(c)\n" 2)
                   (";; a\n(c)\n" 7)
                   ;; No comment run follows, so these blank lines are no gap.
                   (";; a\n\n\n(c)\n" 7)))
    (ert-info ((format "%S at %d" text position))
      (hel-structural-tests--with-buffer #'emacs-lisp-mode text ""
        (goto-char position)
        (hel-mark-inner-comment 1)
        (should-not (use-region-p))))))

(ert-deftest hel-comment-object-line-end-with-parser ()
  "The query producer finds a run from the line ends of its lines too."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "first"
    (end-of-line)
    (hel-mark-inner-comment 1)
    (should (equal (hel-structural-tests--selection) "# first\n# second\n"))))

(ert-deftest hel-comment-motion-from-line-end ()
  "`]c' at a line end of a run selects to the end of the run."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      ";; a\n;; b\n\n;; c\n" ""
    (goto-char 5)
    (hel-mark-comment-forward 1)
    (should (equal (hel-structural-tests--selection) "\n;; b\n"))))

(ert-deftest hel-comment-motion-to-unterminated ()
  "`]c' selects an unterminated comment."
  (hel-structural-tests--with-buffer #'c-mode "int x;\n/* open" "int"
    (hel-mark-comment-forward 1)
    (should (equal (hel-structural-tests--selection) "/* open"))))

(ert-deftest hel-function-jumps ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "class A"
    (let (forward backward)
      (dotimes (_ 3)
        (hel-mark-function-forward 1)
        (push (hel-structural-tests--first-line) forward))
      (should (equal (nreverse forward)
                     '("def f(self, a, b):" "def h(self):" "def top(x):")))
      (should (= (point) (region-end)))
      (dotimes (_ 2)
        (hel-mark-function-backward 1)
        (push (hel-structural-tests--first-line) backward))
      ;; The first [f finishes the selected function, from its end back to
      ;; its start, and the next one selects the function before it.
      (should (equal (nreverse backward)
                     '("def top(x):" "def h(self):")))
      (should (= (point) (region-beginning))))))

(ert-deftest hel-function-jump-count ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "class A"
    ;; Three whole functions, with everything between them.
    (hel-mark-function-forward 3)
    (should (equal (hel-structural-tests--first-line) "def f(self, a, b):"))
    (should (string-suffix-p "return x" (hel-structural-tests--selection)))
    ;; Fewer functions exist than the count: the motion stops at the last.
    (hel-mark-function-forward 5)
    (should (equal (hel-structural-tests--first-line) "def last():"))
    ;; No function is left: the selection stays.
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--first-line) "def last():"))))

(ert-deftest hel-function-jump-decorated ()
  "A motion selects a decorated function with its decorators."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--decorated "# Header"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--first-line) "@dec"))))

(ert-deftest hel-argument-jumps ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "self, a"
    (let (steps)
      (dotimes (_ 3)
        (hel-mark-argument-forward 1)
        (push (hel-structural-tests--selection) steps))
      ;; The first ]a finishes `self', and the next ones select whole
      ;; arguments without their commas.
      (should (equal (nreverse steps) '("self" "a" "b"))))))

(ert-deftest hel-backward-jump-to-adjacent-object ()
  "A backward motion reaches an object that ends right before the cursor."
  (hel-structural-tests--with-objects #'python-ts-mode "f(a,b)\n" "b"
    (hel-mark-argument-backward 1)
    (should (equal (hel-structural-tests--selection) "a"))))

(ert-deftest hel-object-count ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return g"
    (hel-mark-inner-function 2)
    (should (equal (hel-structural-tests--selection)
                   "def f(self, a, b):\n        return g(a, b)\n\n    def h(self):\n        pass"))
    (hel-structural-tests--goto "pass")
    (hel-mark-a-function 2)
    (should (equal (hel-structural-tests--first-line) ""))
    (should (equal (string-trim (hel-structural-tests--selection))
                   "def h(self):\n        pass")))
  (hel-structural-tests--with-objects #'python-ts-mode "f(a, b, c)\n" "a,"
    (hel-mark-inner-argument 2)
    (should (equal (hel-structural-tests--selection) "a, b"))
    (hel-structural-tests--goto "a,")
    (hel-mark-a-argument 2)
    (should (equal (hel-structural-tests--selection) "a, b, "))
    (hel-structural-tests--goto "b,")
    (hel-mark-inner-argument -2)
    (should (equal (hel-structural-tests--selection) "a, b"))))

(ert-deftest hel-object-repeated-key ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return g"
    (hel-mark-a-function 1)
    (let ((one (hel-structural-tests--selection)))
      (hel-mark-a-function 1)
      (let ((two (hel-structural-tests--selection)))
        (should (string-prefix-p one two))
        (should (string-match-p "def h(self):\n        pass\\'" two))
        (hel-mark-a-function 1)
        (should (equal (hel-structural-tests--selection) two))
        (hel-shrink-selection 1)
        (should (equal (hel-structural-tests--selection) one))))))

(ert-deftest hel-object-shrink-returns ()
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--objects "return g"
    (set-mark (point))
    (forward-char 6)
    (hel-mark-a-function 1)
    (hel-shrink-selection 1)
    (should (equal (hel-structural-tests--selection) "return"))))

(ert-deftest hel-object-inherited-queries ()
  "The TypeScript file inherits the function object from `ecma'."
  (hel-structural-tests--with-objects #'typescript-ts-mode
      "function f(a: number) {\n  return a;\n}\n" "return"
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--first-line) "function f(a: number) {"))))

(ert-deftest hel-object-other-languages ()
  (dolist (spec '((rust-ts-mode "fn f(a: i32) -> i32 {\n    a\n}\n" "fn f(a: i32) -> i32 {")
                  (c-ts-mode "int f(int a) {\n  return a;\n}\n" "int f(int a) {")
                  (js-ts-mode "function f(a) {\n  return a;\n}\n" "function f(a) {")
                  (go-ts-mode "package p\n\nfunc f(a int) int {\n\treturn a\n}\n" "func f(a int) int {")))
    (when (and (fboundp (car spec))
               (treesit-language-available-p
                (pcase (car spec)
                  ('rust-ts-mode 'rust) ('c-ts-mode 'c)
                  ('js-ts-mode 'javascript) ('go-ts-mode 'go))))
      (hel-structural-tests--with-objects (car spec) (nth 1 spec) "a"
        (goto-char (point-max))
        (search-backward "a")
        (hel-mark-a-function 1)
        (should (equal (hel-structural-tests--first-line) (nth 2 spec)))))))

(ert-deftest hel-object-language-at-cursor ()
  "In HTML, the cursors in a script and in the markup use their own query."
  (skip-unless (and (fboundp 'mhtml-ts-mode)
                    (treesit-language-available-p 'html)
                    (treesit-language-available-p 'javascript)))
  (hel-structural-tests--with-buffer #'mhtml-ts-mode
      "<p>text</p>\n<!-- note -->\n<script>\nfunction f(a) {\n  return a;\n}\n</script>\n"
      "return"
    (treesit-update-ranges)
    (skip-unless (hel-treesit-text-objects-p))
    (should (eq 'javascript (treesit-language-at (point))))
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--first-line) "function f(a) {"))
    (hel-structural-tests--goto "text")
    (should (eq 'html (treesit-language-at (point))))
    ;; The HTML query has no function object, so the markup keeps the
    ;; selection, and its comment object works.
    (hel-mark-inner-function 1)
    (should (eq (hel-structural-tests--selection) :none))
    (hel-structural-tests--goto "note")
    (should (eq 'html (treesit-language-at (point))))
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection) "<!-- note -->\n"))))

;;; Text objects without a parser

(defconst hel-structural-tests--elisp
  ";; Header of a.
;; Second line.
(defun a ()
  1)

;; Header of b.
(defun b ()
  2)

(defun c ()
  3)
")

(ert-deftest hel-elisp-function-object ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "1)"
    (hel-mark-inner-function 1)
    (should (equal (hel-structural-tests--selection) "(defun a ()\n  1)"))
    (hel-structural-tests--goto "1)")
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   ";; Header of a.\n;; Second line.\n(defun a ()\n  1)\n\n"))
    (hel-structural-tests--goto "3)")
    (hel-mark-a-function 1)
    (should (equal (hel-structural-tests--selection)
                   "\n\n(defun c ()\n  3)"))))

(ert-deftest hel-elisp-function-count ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "2)"
    (hel-mark-inner-function 2)
    (should (equal (hel-structural-tests--selection)
                   "(defun b ()\n  2)\n\n(defun c ()\n  3)"))
    (hel-structural-tests--goto "1)")
    (hel-mark-a-function 2)
    (should (equal (hel-structural-tests--selection)
                   ";; Header of a.\n;; Second line.\n(defun a ()\n  1)\n\n;; Header of b.\n(defun b ()\n  2)\n\n"))))

(ert-deftest hel-elisp-function-jumps ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "Header of a"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--first-line) "(defun a ()"))
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--first-line) "(defun b ()"))
    ;; The first [f finishes b, from its end back to its start.
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--first-line) "(defun b ()"))
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--first-line) "(defun a ()"))))

(ert-deftest hel-elisp-comment-object ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "Second"
    (hel-mark-a-comment 1)
    (should (equal (hel-structural-tests--selection)
                   ";; Header of a.\n;; Second line.\n"))
    (hel-structural-tests--goto "Second")
    (hel-mark-inner-comment 1)
    (should (equal (hel-structural-tests--selection)
                   ";; Header of a.\n;; Second line.\n"))
    (hel-structural-tests--goto "1)")
    (hel-mark-comment-forward 1)
    (should (equal (hel-structural-tests--selection) ";; Header of b.\n"))))

(ert-deftest hel-treesit-comment-jumps-select-runs ()
  "The Helix pattern `(comment)+' joins a comment after code with the
comment lines below it. A jump selects runs of comment lines instead."
  (hel-structural-tests--with-objects #'python-ts-mode
      "x = 1  # a\n# b\n# c\n\ny = 2\n# d\n" "x ="
    (should (equal (cl-loop repeat 3
                            do (hel-mark-comment-forward 1)
                            collect (hel-structural-tests--selection))
                   '("# a" "# b\n# c\n" "# d\n")))
    (hel-structural-tests--goto "y =")
    (should (equal (cl-loop repeat 2
                            do (hel-mark-comment-backward 1)
                            collect (hel-structural-tests--selection))
                   '("# b\n# c\n" "# a")))))

(ert-deftest hel-no-parser-class-and-argument ()
  "Without a query, the class and argument objects keep the selection."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "defun a"
    (let ((inhibit-message t))
      (hel-mark-inner-argument 1)
      (hel-mark-a-class 1))
    (should (eq (hel-structural-tests--selection) :none))))

;;; Prose objects

(defconst hel-structural-tests--paragraphs
  "Foo bar.  Baz qux.\nMore here.\n\n  Second para.\nline two.\n\n\nThird.\n")

(ert-deftest hel-word-object ()
  "The gap of a word holds only spaces and tabs."
  (hel-structural-tests--with-buffer #'text-mode
      "foo bar baz\n  qux" "bar"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) "bar "))
    (hel-structural-tests--goto "baz")
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) " baz"))
    (hel-structural-tests--goto "bar")
    (hel-mark-inner-word 2)
    (should (equal (hel-structural-tests--selection) "bar baz"))))

(ert-deftest hel-word-object-before-punctuation ()
  "A word right before punctuation takes the gap before it."
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--paragraphs "bar"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) " bar"))
    (hel-structural-tests--goto "bar")
    (hel-mark-a-WORD 1)
    (should (equal (hel-structural-tests--selection) "bar.  "))))

(ert-deftest hel-sentence-object ()
  "The gap of a sentence crosses lines but not paragraphs."
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--paragraphs "Baz"
    (hel-mark-inner-sentence 1)
    (should (equal (hel-structural-tests--selection) "Baz qux."))
    (hel-structural-tests--goto "Baz")
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) "Baz qux.\n"))
    (hel-structural-tests--goto "More")
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) "\nMore here."))))

(ert-deftest hel-sentence-count-stays-in-paragraph ()
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--paragraphs "Baz"
    (hel-mark-inner-sentence 5)
    (should (equal (hel-structural-tests--selection) "Baz qux.\nMore here."))))

(ert-deftest hel-sentence-motions ()
  "]s and [s select from point to the end or the start of the sentence."
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--paragraphs "qux"
    (hel-mark-sentence-forward 1)
    (should (equal (hel-structural-tests--selection) "qux."))
    (hel-structural-tests--goto "here")
    (hel-mark-sentence-backward 1)
    (should (equal (hel-structural-tests--selection) "More "))))

(ert-deftest hel-paragraph-object ()
  "A paragraph covers whole lines."
  (hel-structural-tests--with-buffer #'text-mode
      hel-structural-tests--paragraphs "two"
    (hel-mark-inner-paragraph 1)
    (should (equal (hel-structural-tests--selection)
                   "  Second para.\nline two.\n"))
    (hel-structural-tests--goto "two")
    (hel-mark-a-paragraph 1)
    (should (equal (hel-structural-tests--selection)
                   "  Second para.\nline two.\n\n\n"))
    (hel-structural-tests--goto "Third")
    (hel-mark-a-paragraph 1)
    (should (equal (hel-structural-tests--selection) "\n\nThird.\n"))))

(ert-deftest hel-word-object-no-header-comment ()
  "A comment line above a word stays out of the selection."
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      ";; note\nfoo bar\n" "foo"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) "foo "))))

;;; Search in windows

(defconst hel-structural-tests--search-text
  "# head
import os

@decorator
def first(a, b=[1, 2],
          c=None):
    # inside
    return a  # trailing


class Outer:
    \"\"\"Doc.\"\"\"

    def method(self, x):
        def inner(y):
            return y
        return inner(x)

    # between
    # methods
    def other(self): pass


def last(*args, **kwargs):
    pass
# tail
")

(defun hel-structural-tests--search-results ()
  "Return the answers of the text object searches at every position.
The list holds the next function, the next argument and the next comment
from each position in both directions, and the sibling of each function
and argument in both directions."
  (let* ((function-producer (hel-treesit-producer-create :capture "function"))
         (argument-producer (hel-treesit-producer-create :capture "parameter"))
         (known (->> `((function ,function-producer "function")
                       (argument ,argument-producer "parameter"))
                     (-mapcat
                      (-lambda ((thing producer capture))
                        (-map (lambda (instance) (list thing producer instance))
                              (hel-treesit--capture-things
                               capture (point-min) (point-max))))))))
    (append
     (cl-loop for pos from (point-min) to (point-max)
              append (cl-loop for dir in '(1 -1)
                              collect (list pos dir
                                            (hel-thing-next 'function function-producer pos dir)
                                            (hel-thing-next 'argument argument-producer pos dir)
                                            (hel-treesit--next-comment "comment" pos dir))))
     (cl-loop for (thing producer instance) in known
              append (cl-loop for dir in '(1 -1)
                              collect (-some-> (hel-thing-sibling thing producer instance dir)
                                        (plist-get :begin)))))))

(ert-deftest hel-treesit-search-in-windows ()
  "A search in small windows finds the same objects as one whole-buffer
scan, from every position and in both directions."
  (hel-structural-tests--with-objects #'python-ts-mode
      hel-structural-tests--search-text "def first"
    (let ((whole (let ((hel-treesit--search-window (buffer-size)))
                   (hel-structural-tests--search-results)))
          (windowed (let ((hel-treesit--search-window 16))
                      (hel-structural-tests--search-results))))
      ;; The text must give answers, or the comparison proves nothing.
      (should (cl-some #'caddr whole))
      (should (cl-some (lambda (result) (and (integerp result) result)) whole))
      (should (equal windowed whole)))))

;;; Query files

(ert-deftest hel-treesit-query-patterns ()
  (should (equal (hel-treesit--query-patterns
                  "; comment\n(a) @x\n\n((b)\n (#match? @y \"(\"))\n(c)+\n @z\n")
                 '("(a) @x" "((b)\n (#match? @y \"(\"))" "(c)+\n @z"))))

(defmacro hel-structural-tests--with-query-file (text &rest body)
  "Run BODY with a queries directory whose Python file holds TEXT."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'python))
     (let* ((dir (make-temp-file "hel-queries" t))
            (hel-structural-tests--queries (file-name-as-directory dir))
            (hel-structural-tests--query-cache (make-hash-table :test #'eq))
            (inhibit-message t))
       (unwind-protect
           (progn
             (make-directory (expand-file-name "python" dir))
             (with-temp-file (expand-file-name "python/textobjects.scm" dir)
               (insert ,text))
             ,@body)
         (delete-directory dir t)))))

(ert-deftest hel-treesit-broken-pattern ()
  "A query file with a broken pattern keeps its other patterns."
  (hel-structural-tests--with-query-file
      "(no_such_node) @function.around\n(function_definition) @function.around\n"
    (hel-structural-tests--with-objects #'python-ts-mode
        "def f():\n    return 1\n" "return"
      (hel-mark-a-function 1)
      (should (equal (hel-structural-tests--first-line) "def f():")))))

(ert-deftest hel-treesit-unbalanced-query-file ()
  "A query file that cannot be split gives no query and no error, and the
function object comes from `hel-function'."
  (hel-structural-tests--with-query-file
      "(function_definition) @function.around\n(class_definition\n"
    (hel-structural-tests--with-buffer #'python-ts-mode
        "def f():\n    return 1\n" "return"
      (should-not (hel-treesit-text-objects-p))
      (hel-mark-inner-function 1)
      (should (equal (hel-structural-tests--selection)
                     "def f():\n    return 1")))))

;;; Hel's own patterns

(defun hel-structural-tests--object-text (language text needle object)
  "Return the text of the innermost OBJECT at NEEDLE in TEXT, or nil.
TEXT is parsed as LANGUAGE, with the queries of the repository."
  (let ((hel-treesit-queries-directory hel-structural-tests--queries)
        (hel-treesit--queries (make-hash-table :test #'eq)))
    (with-temp-buffer
      (insert text)
      (treesit-parser-create language)
      (goto-char (point-min))
      (search-forward needle)
      (let ((pos (match-beginning 0)))
        (-some->> (hel-treesit--thing-matches object (point-min) (point-max))
          (-map #'hel-treesit--match-thing)
          (-filter (lambda (o)
                     (<= (plist-get o :begin) pos (plist-get o :end))))
          (-min-by (-on #'> (lambda (o)
                              (- (plist-get o :end) (plist-get o :begin)))))
          (funcall (lambda (o)
                     (buffer-substring-no-properties (plist-get o :begin)
                                                     (plist-get o :end)))))))))

(defmacro hel-structural-tests--objects-should (language &rest cases)
  "Check CASES of (TEXT NEEDLE OBJECT EXPECTED) in LANGUAGE.
Skip the test when LANGUAGE has no grammar or Emacs has no GROUPED
captures."
  (declare (indent 1))
  `(progn
     (skip-unless (and hel-treesit--grouped-capture-p
                       (treesit-language-available-p ',language)))
     ,@(mapcar (pcase-lambda (`(,text ,needle ,object ,expected))
                 `(should (equal (hel-structural-tests--object-text
                                  ',language ,text ,needle ,object)
                                 ,expected)))
               cases)))

(ert-deftest hel-treesit-emacs-regexps ()
  "The regex of `#match?' gets Emacs syntax, and plain text stays."
  (let* ((pattern (hel-treesit--emacs-regexps
                   "((identifier) @k (#match? @k \"^(def|defp)$\"))"))
         (regex (read (substring pattern (string-search "\"" pattern)))))
    (should (string-match-p regex "def"))
    (should (string-match-p regex "defp"))
    (should-not (string-match-p regex "define"))
    (should-not (string-match-p regex "(def|defp)")))
  (should (equal (hel-treesit--emacs-regexps "((x) @k (#match? @k \"^test_\"))")
                 "((x) @k (#match? @k \"^test_\"))")))

(ert-deftest hel-own-patterns-elixir ()
  "The Elixir patterns, whose regexes use alternation, find objects."
  (hel-structural-tests--objects-should elixir
    ("defmodule M do\n  def f() do\n    1\n  end\nend\n" "f()" "function"
     "def f() do\n    1\n  end")
    ("defmodule M do\n  def f() do\n    1\n  end\nend\n" "M " "class"
     "defmodule M do\n  def f() do\n    1\n  end\nend")))

(ert-deftest hel-own-patterns-javascript ()
  (hel-structural-tests--objects-should javascript
    ("export function f() {}\n" "f(" "function" "export function f() {}")
    ("export default class { m() {} }\n" "m(" "class"
     "export default class { m() {} }")
    ("const h = () => 1;\n" "=>" "function" "const h = () => 1;")
    ("export const e = () => 1;\n" "=>" "function" "export const e = () => 1;")
    ("foo(() => 1);\n" "=>" "function" "() => 1")))

(ert-deftest hel-own-patterns-typescript ()
  (hel-structural-tests--objects-should typescript
    ("export abstract class A { }\n" "A " "class" "export abstract class A { }")
    ("export interface I { x: number }\n" "I " "class"
     "export interface I { x: number }")
    ("declare function d(): void;\n" "d(" "function"
     "declare function d(): void;")))

(ert-deftest hel-own-patterns-rust ()
  (hel-structural-tests--objects-should rust
    ("#[derive(Debug)]\npub struct S { x: i32 }\n" "S " "class"
     "#[derive(Debug)]\npub struct S { x: i32 }")
    ("struct P { x: i32 }\n" "P " "class" "struct P { x: i32 }")))

(ert-deftest hel-own-patterns-c ()
  (hel-structural-tests--objects-should c
    ("struct S { int x; };\n" "S " "class" "struct S { int x; };")
    ("typedef struct S { int x; } S;\n" "S {" "class"
     "typedef struct S { int x; } S;")))

(ert-deftest hel-own-patterns-cpp ()
  "C++ reads Hel's C patterns through its inheritance, and its own."
  (hel-structural-tests--objects-should cpp
    ("struct P { int y; };\n" "P " "class" "struct P { int y; };")
    ("class C { int x; };\n" "C " "class" "class C { int x; };")
    ("template <typename T>\nclass D { T x; };\n" "D " "class"
     "template <typename T>\nclass D { T x; };")
    ("template <typename T>\nT f(T x) { return x; }\n" "f(" "function"
     "template <typename T>\nT f(T x) { return x; }")))

(ert-deftest hel-own-patterns-ruby ()
  (hel-structural-tests--objects-should ruby
    ("class K\n  private def m\n    1\n  end\nend\n" "m\n" "function"
     "private def m\n    1\n  end")))

;;; Bindings

(ert-deftest hel-structural-bindings ()
  "Every key is a plain binding to one command."
  (with-temp-buffer
    (fundamental-mode)
    (hel-local-mode 1)
    (hel-normal-state)
    (hel-update-active-keymaps)
    (should (equal (-map (lambda (key) (key-binding (kbd key)))
                         '("m f" "] f" "m i a" "M-n" "M-o" "M-a"))
                   '(hel-mark-inner-function hel-mark-function-forward
                     hel-mark-inner-argument hel-select-next-sibling
                     hel-expand-selection hel-select-all-siblings)))))

;;; Text objects: helpers

(defconst hel-to--python
  "# first
# second

class A:
    def f(self, a, b):
        return g(a, b)

    def h(self):
        pass

# Header of top.
def top(x):
    return x
"
  "A class with two methods, a top-level function and a two-line comment.")

(defconst hel-to--python-laws
  (concat hel-to--python
          "\n@dec\ndef last(y, z):\n    def inner():\n        pass\n")
  "The text of `hel-to--python' with a decorated function after it.
The decorated function holds a nested function that ends where it ends.")

(defconst hel-to--sentences
  "One two. Three four.\nFive six.\n\nSeven eight. Nine ten.\n"
  "Two paragraphs of prose. The first paragraph holds three sentences.")

(defconst hel-to--paragraphs
  "Foo bar.\nMore here.\n\nSecond para.\nline two.\n\nThird.\n"
  "Three paragraphs with one blank line between each two of them.")

(defconst hel-to--elisp-comments
  ";; a\n;; b\n(x) ; c\n\n;; d\n(y)\n"
  "Three runs of comments: two full lines, a comment after code, one line.")

(defmacro hel-to--with-python (text start &rest body)
  "Run BODY in a `python-ts-mode' buffer with TEXT, point at START.
Skip the test without the Python grammar or without the Helix query."
  (declare (indent 2))
  `(progn
     (skip-unless (treesit-language-available-p 'python))
     (hel-structural-tests--with-objects #'python-ts-mode ,text ,start
       ,@body)))

(defmacro hel-to--with-prose (text start &rest body)
  "Run BODY in a `text-mode' buffer with TEXT, point at the first match of
START. One space after a period ends a sentence."
  (declare (indent 2))
  `(let ((sentence-end-double-space nil))
     (hel-structural-tests--with-buffer #'text-mode ,text ,start
       ,@body)))

(defun hel-to--goto-blank-line (text)
  "Deactivate the mark and move point to the blank line before TEXT.
TEXT must follow exactly one blank line."
  (hel-structural-tests--goto (concat "\n\n" text))
  (forward-char 1))

(defun hel-to--producer (object)
  "Return the producer of OBJECT at point.
It is the result of the first source of OBJECT that applies at point."
  (-some #'funcall (get object 'hel-thing-sources)))

(defun hel-to--key (object)
  "Return the essential parts of OBJECT, which define its equality."
  (list (plist-get object :begin)
        (plist-get object :end)))

(defun hel-to--text (object)
  "Return the text of the range of OBJECT."
  (buffer-substring-no-properties (plist-get object :begin)
                                  (plist-get object :end)))

(defun hel-to--known-objects (thing producer)
  "Return every THING that PRODUCER reports somewhere in the buffer.
The list holds the results of `hel-thing-next' from every position in
both directions and the results of `hel-things-at' on every character.
Each object appears once."
  (let (objects)
    (dolist (pos (number-sequence (point-min) (point-max)))
      (dolist (dir '(1 -1))
        (when-let* ((object (hel-thing-next thing producer pos dir)))
          (push object objects)))
      (when (< pos (point-max))
        (setq objects (append (hel-things-at thing producer (cons pos (1+ pos)))
                              objects))))
    (cl-remove-duplicates objects :key #'hel-to--key :test #'equal)))

(defun hel-to--object-with-text (objects text)
  "Return the object of OBJECTS whose range holds exactly TEXT."
  (-first (lambda (object) (equal (hel-to--text object) text)) objects))

(defun hel-to--nearest (objects pos dir)
  "Return the object of OBJECTS that `hel-thing-next' must give at POS.
Forward, it is the object that starts first after POS. Backward, it is
the object that ends last at or before POS. Of two objects that start, or
end, at the same place, the larger one wins."
  (let ((forward (< 0 dir)))
    (car (sort (-filter (lambda (object)
                          (if forward
                              (< pos (plist-get object :begin))
                            (<= (plist-get object :end) pos)))
                        objects)
               (lambda (a b)
                 (let ((edge-a (if forward
                                   (plist-get a :begin)
                                 (- (plist-get a :end))))
                       (edge-b (if forward
                                   (plist-get b :begin)
                                 (- (plist-get b :end))))
                       (size-a (- (plist-get a :end) (plist-get a :begin)))
                       (size-b (- (plist-get b :end) (plist-get b :begin))))
                   (or (< edge-a edge-b)
                       (and (= edge-a edge-b) (> size-a size-b)))))))))

(defun hel-to--check-laws (thing)
  "Check the laws of the protocol for THING at point in the buffer.
Return the objects that the check used."
  (let* ((producer (hel-to--producer thing))
         (objects (hel-to--known-objects thing producer))
         (positions (number-sequence (point-min) (point-max)))
         (siblings 0))
    ;; The buffer must hold objects, or the laws prove nothing.
    (should objects)
    ;; An object has a non-empty range.
    (dolist (object objects)
      (should (< (plist-get object :begin) (plist-get object :end))))
    ;; Forward, `hel-thing-next' returns an object that starts after POS.
    ;; Backward, it returns an object that ends at or before POS. Of the
    ;; candidates, it returns the nearest one.
    ;; AMBIGUOUS: "starts after POS" may or may not include an object that
    ;; starts at POS. The backward law says "at or before" and the forward
    ;; law does not, so the test reads "after" as strictly after.
    (dolist (pos positions)
      (dolist (dir '(1 -1))
        (let ((next (hel-thing-next thing producer pos dir)))
          (when next
            (should (equal (list pos dir t)
                           (list pos dir
                                 (if (< 0 dir)
                                     (< pos (plist-get next :begin))
                                   (<= (plist-get next :end) pos))))))
          (should (equal (list pos dir (-some-> next hel-to--key))
                         (list pos dir (-some-> (hel-to--nearest objects pos dir)
                                         hel-to--key)))))))
    ;; Siblings are inverse in the two directions.
    (dolist (object objects)
      (when-let* ((sibling (hel-thing-sibling thing producer object 1)))
        (cl-incf siblings)
        (should (equal (list (hel-to--key object)
                             (-some-> (hel-thing-sibling thing producer sibling -1)
                               hel-to--key))
                       (list (hel-to--key object) (hel-to--key object))))))
    (should (< 0 siblings))
    ;; `hel-thing-skip-gap' never moves against DIR, and a second call
    ;; from its own result returns that result again.
    (dolist (pos positions)
      (dolist (dir '(1 -1))
        (when-let* ((end (hel-thing-skip-gap thing producer pos dir)))
          (should (equal (list pos dir t)
                         (list pos dir (if (< 0 dir) (<= pos end) (<= end pos)))))
          (should (equal (list pos dir end)
                         (list pos dir (hel-thing-skip-gap thing producer end dir)))))))
    objects))

;;; Moving to the edge of an object: functions

;; Section "Moving to the edge of an object": inside an object, `]f'
;; selects from point to the end of the object under the cursor. At that
;; edge it selects the whole next object. The object leaves out the header
;; comment (section "The ranges of an object").
(ert-deftest hel-to-function-forward-from-inside ()
  (hel-to--with-python hel-to--python "return g"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection) "return g(a, b)"))
    (should (= (point) (region-end)))
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection)
                   "def h(self):\n        pass"))
    (should (= (point) (region-end)))
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection)
                   "def top(x):\n    return x"))
    (should (= (point) (region-end)))))

;; Section "Moving to the edge of an object": `[f' inside a function
;; selects from point to its start, and at that edge it selects the whole
;; previous object. Point goes to the start after `['.
(ert-deftest hel-to-function-backward-from-inside ()
  (hel-to--with-python hel-to--python "pass"
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--selection) "def h(self):\n        "))
    (should (= (point) (region-beginning)))
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--selection)
                   "def f(self, a, b):\n        return g(a, b)"))
    (should (= (point) (region-beginning)))))

;; Section "Moving to the edge of an object": "Of nested objects, the
;; innermost one counts."
(ert-deftest hel-to-function-edge-innermost ()
  (hel-to--with-python "def f():\n    def inner():\n        pass\n    return 1\n"
      "pass"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection) "pass"))
    (hel-structural-tests--goto "pass")
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--selection) "def inner():\n        "))))

;; Section "Moving to the edge of an object": "3]f inside a function selects
;; to the end of the function and of the two functions after it, with
;; everything between them. When fewer objects exist, the motion stops at
;; the last one." Section "Tests": one more `]f' keeps the selection.
(ert-deftest hel-to-function-forward-count ()
  (hel-to--with-python hel-to--python "return g"
    (let ((expected (concat "return g(a, b)\n\n"
                            "    def h(self):\n        pass\n\n"
                            "# Header of top.\n"
                            "def top(x):\n    return x")))
      (hel-mark-function-forward 3)
      (should (equal (hel-structural-tests--selection) expected))
      (should (= (point) (region-end)))
      (hel-mark-function-forward 1)
      (should (equal (hel-structural-tests--selection) expected)))))

;; Section "Moving to the edge of an object": "]f selects a decorated
;; function with its decorators", because the motion uses the same objects
;; as `mi'. The header comment line is outside every object, so `]f'
;; selects the whole next object. From the decorator line the cursor is
;; inside the object, and `]f' selects from point to its end: the same text.
(ert-deftest hel-to-function-forward-decorated ()
  (hel-to--with-python hel-structural-tests--decorated "# Header"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection)
                   "@dec\ndef f():\n    def inner():\n        pass"))
    (hel-structural-tests--goto "@dec")
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection)
                   "@dec\ndef f():\n    def inner():\n        pass"))))

;; Section "Moving to the edge of an object" and "Tests": "]a from self
;; selects to the end of self, and the next two ]a select a and b without
;; their commas."
(ert-deftest hel-to-argument-forward ()
  (hel-to--with-python hel-to--python "self"
    (should (equal (cl-loop repeat 3
                            do (hel-mark-argument-forward 1)
                            collect (hel-structural-tests--selection))
                   '("self" "a" "b")))))

;; Section "Moving to the edge of an object": without a parser, `]f' runs
;; through `hel--forward-function'. Inside a `defun' it selects to its end,
;; and the next `]f' selects the next `defun' without its header comment.
(ert-deftest hel-to-elisp-function-edges ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "1)"
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection) "1)"))
    (hel-mark-function-forward 1)
    (should (equal (hel-structural-tests--selection) "(defun b ()\n  2)"))
    (hel-structural-tests--goto "2)")
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--selection) "(defun b ()\n  "))
    (should (= (point) (region-beginning)))
    (hel-mark-function-backward 1)
    (should (equal (hel-structural-tests--selection) "(defun a ()\n  1)"))))

;; Section "Moving to the edge of an object": "]c selects runs of comment
;; lines, so in the lines x = 1  # a, # b and # c, it selects # a first and
;; then the run of # b and # c." Section "Choosing a producer": without a
;; parser, the comment object uses the syntax table.
(ert-deftest hel-to-comment-forward-syntax-table ()
  (hel-structural-tests--with-buffer #'python-mode
      "x = 1  # a\n# b\n# c\n" "x ="
    (should (equal (cl-loop repeat 2
                            do (hel-mark-comment-forward 1)
                            collect (hel-structural-tests--selection))
                   '("# a" "# b\n# c\n")))))

;;; Moving to the edge of an object: prose

;; Section "Moving to the edge of an object": at the edge of a sentence,
;; `]s' selects the whole next sentence. "The motion follows the order of
;; the buffer and ignores parents", so it passes the end of the paragraph.
(ert-deftest hel-to-sentence-edges ()
  (hel-to--with-prose hel-to--sentences "Three"
    (should (equal (cl-loop repeat 3
                            do (hel-mark-sentence-forward 1)
                            collect (hel-structural-tests--selection))
                   '("Three four." "Five six." "Seven eight.")))
    (hel-structural-tests--goto "four")
    (should (equal (cl-loop repeat 2
                            do (hel-mark-sentence-backward 1)
                            collect (hel-structural-tests--selection))
                   '("Three " "One two.")))
    (should (= (point) (region-beginning)))))

;; Section "Moving to the edge of an object": "]s with a count passes the
;; end of a paragraph, while mis stops there." "When fewer objects exist,
;; the motion stops at the last one."
(ert-deftest hel-to-sentence-forward-count ()
  (hel-to--with-prose hel-to--sentences "Three"
    (hel-mark-sentence-forward 3)
    (should (equal (hel-structural-tests--selection)
                   "Three four.\nFive six.\n\nSeven eight."))
    (hel-structural-tests--goto "Three")
    (hel-mark-sentence-forward 10)
    (should (equal (hel-structural-tests--selection)
                   "Three four.\nFive six.\n\nSeven eight. Nine ten."))))

;; Section "Moving to the edge of an object": outside every object, `]s'
;; selects the whole next object and `[s' the whole previous one. A blank
;; line is outside every sentence, because an object has no whitespace at
;; its edges (section "The object").
(ert-deftest hel-to-sentence-edges-outside-objects ()
  (hel-to--with-prose hel-to--sentences "One"
    (hel-to--goto-blank-line "Seven")
    (hel-mark-sentence-forward 1)
    (should (equal (hel-structural-tests--selection) "Seven eight."))
    (hel-to--goto-blank-line "Seven")
    (hel-mark-sentence-backward 1)
    (should (equal (hel-structural-tests--selection) "Five six."))))

;; Section "Moving to the edge of an object" and "The ranges of an object":
;; a paragraph covers whole lines with its last line end, so `]p' selects
;; to the end of the paragraph with the line end. At that edge, `]p'
;; selects the whole next paragraph; `[p' works the same way backward.
(ert-deftest hel-to-paragraph-edges ()
  (hel-structural-tests--with-buffer #'text-mode hel-to--paragraphs "bar"
    (hel-mark-paragraph-forward 1)
    (should (equal (hel-structural-tests--selection) "bar.\nMore here.\n"))
    (should (= (point) (region-end)))
    (hel-mark-paragraph-forward 1)
    (should (equal (hel-structural-tests--selection) "Second para.\nline two.\n"))
    (hel-structural-tests--goto "two")
    (hel-mark-paragraph-backward 1)
    (should (equal (hel-structural-tests--selection) "Second para.\nline "))
    (should (= (point) (region-beginning)))
    (hel-mark-paragraph-backward 1)
    (should (equal (hel-structural-tests--selection) "Foo bar.\nMore here.\n"))))

;; Section "Moving to the edge of an object": a count adds whole objects,
;; and outside every object the key selects the whole next or previous
;; object. A blank line belongs to no paragraph.
(ert-deftest hel-to-paragraph-edges-count-and-gap ()
  (hel-structural-tests--with-buffer #'text-mode hel-to--paragraphs "bar"
    (hel-mark-paragraph-forward 2)
    (should (equal (hel-structural-tests--selection)
                   "bar.\nMore here.\n\nSecond para.\nline two.\n"))
    (hel-to--goto-blank-line "Second")
    (hel-mark-paragraph-forward 1)
    (should (equal (hel-structural-tests--selection) "Second para.\nline two.\n"))
    (hel-to--goto-blank-line "Second")
    (hel-mark-paragraph-backward 1)
    (should (equal (hel-structural-tests--selection) "Foo bar.\nMore here.\n"))))

;; Section "Moving to the edge of an object": `}' selects from point to the
;; start of the next paragraph. A second `}' from the start of a paragraph
;; goes to the start of the paragraph after it, and a count adds
;; paragraphs.
(ert-deftest hel-to-paragraph-forward-start ()
  (hel-structural-tests--with-buffer #'text-mode hel-to--paragraphs "bar"
    (hel-forward-paragraph 1)
    (should (equal (hel-structural-tests--selection) "bar.\nMore here.\n\n"))
    (should (= (point) (region-end)))
    (hel-forward-paragraph 1)
    (should (equal (hel-structural-tests--selection) "Second para.\nline two.\n\n"))
    (hel-structural-tests--goto "bar")
    (hel-forward-paragraph 2)
    (should (equal (hel-structural-tests--selection)
                   "bar.\nMore here.\n\nSecond para.\nline two.\n\n"))))

;; Section "Moving to the edge of an object": `{' selects from point to the
;; start of the paragraph under the cursor. When point is already there,
;; it selects to the start of the previous paragraph.
(ert-deftest hel-to-paragraph-backward-start ()
  (hel-structural-tests--with-buffer #'text-mode hel-to--paragraphs "two"
    (hel-backward-paragraph 1)
    (should (equal (hel-structural-tests--selection) "Second para.\nline "))
    (should (= (point) (region-beginning)))
    ;; From the start of the paragraph: to the start of the one before.
    (hel-backward-paragraph 1)
    (should (equal (hel-structural-tests--selection)
                   "Foo bar.\nMore here.\n\n"))
    (hel-structural-tests--goto "two")
    (hel-backward-paragraph 2)
    (should (equal (hel-structural-tests--selection)
                   "Foo bar.\nMore here.\n\nSecond para.\nline "))))

;;; Prose objects

;; Section "The ranges of an object": "A word's gap stays on its line."
;; Section "The around range": "When the object has neither gap, the around
;; range has no gap." A tab is a gap of a word, as a space is.
(ert-deftest hel-to-word-gap-stays-on-line ()
  (hel-structural-tests--with-buffer #'text-mode "foo\nbar baz\n" "foo"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) "foo")))
  (hel-structural-tests--with-buffer #'text-mode "one\ttwo\nthree\n" "two"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) "\ttwo"))))

;; Section "The ranges of an object": a word has no header comment, so
;; `maw' in an Emacs Lisp buffer leaves out a comment line above the word.
;; The word starts its line and ends it, so it has no gap either.
(ert-deftest hel-to-word-ignores-comment-above ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode ";; note\nfoo\n" "foo"
    (hel-mark-a-word 1)
    (should (equal (hel-structural-tests--selection) "foo"))))

;; Section "The ranges of an object": "A sentence's gap crosses line ends,
;; but not the end of the paragraph." `mas' takes the whitespace after a
;; sentence, across a line end, and the whitespace before the last
;; sentence of a paragraph.
(ert-deftest hel-to-sentence-around ()
  (hel-to--with-prose hel-to--sentences "One"
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) "One two. "))
    (should (= (point) (region-beginning)))
    (hel-structural-tests--goto "Three")
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) "Three four.\n"))
    (hel-structural-tests--goto "Five")
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) "\nFive six."))
    (hel-structural-tests--goto "Nine")
    (hel-mark-a-sentence 1)
    (should (equal (hel-structural-tests--selection) " Nine ten."))))

;; Section "The ranges of an object": "mas and d never join two
;; paragraphs."
(ert-deftest hel-to-sentence-around-delete ()
  (hel-to--with-prose hel-to--sentences "Five"
    (hel-mark-a-sentence 1)
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string)
                   "One two. Three four.\n\nSeven eight. Nine ten.\n"))))

;; Section "Selecting an object": "The parent of a sentence is its
;; paragraph, so 3 mis on the last sentence of a paragraph selects only that
;; sentence." "When fewer objects exist, the driver selects the ones that
;; exist."
(ert-deftest hel-to-sentence-count-stops-at-paragraph ()
  (hel-to--with-prose hel-to--sentences "Five"
    (hel-mark-inner-sentence 3)
    (should (equal (hel-structural-tests--selection) "Five six."))
    (hel-structural-tests--goto "Seven")
    (hel-mark-inner-sentence 3)
    (should (equal (hel-structural-tests--selection) "Seven eight. Nine ten."))))

;; Section "The ranges of an object": "A paragraph covers whole lines ...
;; So mip and d leave no empty line behind." `map' adds the blank lines
;; after the paragraph.
(ert-deftest hel-to-paragraph-delete ()
  (hel-structural-tests--with-buffer #'text-mode "A.\n\nB b.\nb2.\n\nC.\n" "b2"
    (hel-mark-inner-paragraph 1)
    (should (equal (hel-structural-tests--selection) "B b.\nb2.\n"))
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string) "A.\n\n\nC.\n")))
  (hel-structural-tests--with-buffer #'text-mode "A.\n\nB b.\nb2.\n\nC.\n" "b2"
    (hel-mark-a-paragraph 1)
    (should (equal (hel-structural-tests--selection) "B b.\nb2.\n\n"))
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string) "A.\n\nC.\n"))))

;; Section "Selecting an object": "A cursor in a gap finds the object that
;; the gap belongs to", and "when two around ranges contain the cursor, the
;; object before the cursor wins". Section "Several objects": N objects run
;; from the start of the first to the end of the last, with everything
;; between them.
(ert-deftest hel-to-paragraph-in-gap-and-count ()
  (hel-structural-tests--with-buffer #'text-mode "A.\n\nB.\n" "A."
    (hel-to--goto-blank-line "B.")
    (hel-mark-inner-paragraph 1)
    (should (equal (hel-structural-tests--selection) "A.\n"))
    (hel-structural-tests--goto "A.")
    (hel-mark-inner-paragraph 2)
    (should (equal (hel-structural-tests--selection) "A.\n\nB.\n"))))

;;; Sources and the definition of an object

;; Section "Text objects and motions" and "Choosing a producer": the
;; function object lists its query first and the motion second. Without a
;; parser, the query source returns nil and the motion source applies.
(ert-deftest hel-to-sources-without-parser ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "1)"
    (let ((sources (get 'function 'hel-thing-sources)))
      (should (= (length sources) 2))
      (should (-all? #'functionp sources))
      (should-not (funcall (car sources)))
      (should (funcall (cadr sources))))
    ;; The comment object has the query and the syntax table.
    (let ((sources (get 'comment 'hel-thing-sources)))
      (should (= (length sources) 2))
      (should-not (funcall (car sources)))
      (should (funcall (cadr sources))))
    ;; The class and the argument objects have only a query.
    (dolist (object '(class argument))
      (let ((sources (get object 'hel-thing-sources)))
        (should (= (length sources) 1))
        (should-not (-some #'funcall sources))))))

;; Section "Choosing a producer": the source of a Helix query applies when
;; `hel-treesit-text-objects-p' returns non-nil at the cursor.
(ert-deftest hel-to-sources-with-parser ()
  (hel-to--with-python hel-to--python "return g"
    (dolist (object '(function class argument comment))
      (should (funcall (car (get object 'hel-thing-sources)))))))

;; Section "Text objects and motions": word, WORD, sentence and paragraph
;; each have one motion source, and it applies in every buffer.
(ert-deftest hel-to-sources-of-prose ()
  (hel-to--with-prose hel-to--sentences "Three"
    (dolist (object '(word WORD sentence paragraph))
      (should (hel--producer object)))))

;; Section "Sources and the definition of an object": "A user adds a source
;; with (push SOURCE (get 'function 'hel-thing-sources))." "The first source
;; that returns a producer wins" (section "Choosing a producer").
(ert-deftest hel-to-pushed-source-wins ()
  (let ((sources (get 'function 'hel-thing-sources)))
    (unwind-protect
        (progn
          (push (hel-motion-source #'hel--forward-sentence)
                (get 'function 'hel-thing-sources))
          (hel-to--with-prose hel-to--sentences "Three"
            (hel-mark-inner-function 1)
            (should (equal (hel-structural-tests--selection) "Three four."))))
      (put 'function 'hel-thing-sources sources))))

;; Section "Choosing a producer": a source returns nil when it does not
;; apply, and the next source is tried.
(ert-deftest hel-to-nil-source-is-skipped ()
  (let ((sources (get 'word 'hel-thing-sources)))
    (unwind-protect
        (progn
          (push (lambda () nil) (get 'word 'hel-thing-sources))
          (hel-structural-tests--with-buffer #'text-mode "foo bar baz\n" "bar"
            (hel-mark-a-word 1)
            (should (equal (hel-structural-tests--selection) "bar "))))
      (put 'word 'hel-thing-sources sources))))

;;; The laws of the protocol

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; sentence motion. The parent of a sentence is its paragraph (section
;; "The models"), so the last sentence of a paragraph has no forward
;; sibling.
(ert-deftest hel-to-laws-sentence-motion ()
  (hel-to--with-prose hel-to--sentences "One"
    (let* ((thing 'sentence)
           (producer (hel-to--producer thing))
           (objects (hel-to--check-laws thing))
           (three (hel-to--object-with-text objects "Three four."))
           (five (hel-to--object-with-text objects "Five six.")))
      (should three)
      (should five)
      (should (equal (-some-> (hel-thing-sibling thing producer three 1) hel-to--key)
                     (hel-to--key five)))
      (should-not (hel-thing-sibling thing producer five 1)))))

;; Section "The ranges of an object": the gap of a sentence is whitespace,
;; across line ends. Section "The finding operations have two levels": `hel-thing-skip-gap'
;; returns the position where the gap from POS in direction DIR ends.
(ert-deftest hel-to-skip-gap-sentence ()
  (hel-to--with-prose hel-to--sentences "One"
    (let ((thing 'sentence)
          (producer (hel-to--producer 'sentence))
          (end-of-one (progn (search-forward "two.") (point)))
          (three (progn (hel-structural-tests--goto "Three") (point)))
          (end-of-three (progn (search-forward "four.") (point)))
          (five (progn (hel-structural-tests--goto "Five") (point))))
      (should (equal (hel-thing-skip-gap thing producer end-of-one 1) three))
      (should (equal (hel-thing-skip-gap thing producer three -1) end-of-one))
      (should (equal (hel-thing-skip-gap thing producer end-of-three 1) five)))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; Python query, for functions.
(ert-deftest hel-to-laws-treesit-function ()
  (hel-to--with-python hel-to--python-laws "return g"
    (let* ((thing 'function)
           (producer (hel-to--producer thing))
           (objects (hel-to--check-laws thing))
           (f (hel-to--object-with-text
               objects "def f(self, a, b):\n        return g(a, b)"))
           (h (hel-to--object-with-text objects "def h(self):\n        pass"))
           (top (hel-to--object-with-text objects "def top(x):\n    return x")))
      (should (and f h top))
      (should (equal (-some-> (hel-thing-sibling thing producer f 1) hel-to--key)
                     (hel-to--key h)))
      ;; The class holds no more methods, so h has no forward sibling.
      (should-not (hel-thing-sibling thing producer h 1))
      ;; The decorated function is the next sibling of top, with its
      ;; decorator (section "The object").
      (should (equal (-some-> (hel-thing-sibling thing producer top 1) hel-to--text)
                     "@dec\ndef last(y, z):\n    def inner():\n        pass")))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; Python query, for arguments.
(ert-deftest hel-to-laws-treesit-argument ()
  (hel-to--with-python hel-to--python-laws "return g"
    (hel-to--check-laws 'argument)))

;; Section "The finding operations have two levels": `hel-thing-next' from the end of a top-level
;; function finds the method of the class after it, while the sibling of
;; the function is the second top-level function.
(ert-deftest hel-to-treesit-sibling-skips-nested ()
  (hel-to--with-python
      "def a():\n    pass\n\nclass K:\n    def m(self):\n        pass\n\ndef b():\n    pass\n"
      "pass"
    (let* ((thing 'function)
           (producer (hel-to--producer thing))
           (objects (hel-to--known-objects thing producer))
           (a (hel-to--object-with-text objects "def a():\n    pass")))
      (should a)
      (should (equal (-some-> (hel-thing-next thing producer (plist-get a :end) 1)
                       hel-to--text)
                     "def m(self):\n        pass"))
      (should (equal (-some-> (hel-thing-sibling thing producer a 1) hel-to--text)
                     "def b():\n    pass")))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; comment runs of the syntax table. A run of comment lines covers whole
;; lines, and a comment after code covers only the comment.
(ert-deftest hel-to-laws-syntax-comments ()
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-to--elisp-comments "(x)"
    (let* ((thing 'comment)
           (producer (hel-to--producer thing))
           (objects (hel-to--check-laws thing)))
      (should (equal (-map #'hel-to--text
                           (-sort (lambda (a b)
                                    (< (plist-get a :begin)
                                       (plist-get b :begin)))
                                  objects))
                     '(";; a\n;; b\n" "; c" ";; d\n")))
      ;; Away from the start of a line, the gap stays on its line, so it
      ;; never joins a line of code to the next one.
      (dolist (pos (number-sequence (point-min) (point-max)))
        (unless (save-excursion (goto-char pos) (bolp))
          (dolist (dir '(1 -1))
            (let ((gap-end (hel-thing-skip-gap thing producer pos dir)))
              (should-not (string-search
                           "\n" (buffer-substring-no-properties
                                 (min pos gap-end) (max pos gap-end)))))))))))

;; Section "The ranges of an object", the table of objects: word, WORD,
;; sentence, paragraph and comment take no header comment. The function
;; motion takes one (section "Things and sources").
(ert-deftest hel-to-header-p-without-parser ()
  (hel-to--with-prose hel-to--sentences "Three"
    (dolist (object '(word WORD sentence paragraph))
      (should-not (hel-thing-header-p object))))
  (hel-structural-tests--with-buffer #'emacs-lisp-mode
      hel-structural-tests--elisp "1)"
    (should (hel-thing-header-p 'function))
    (should-not (hel-thing-header-p 'comment))))

;; Section "The ranges of an object", the table of objects: function, class
;; and argument have a header comment, and comment has none.
(ert-deftest hel-to-header-p-treesit ()
  (hel-to--with-python hel-to--python "return g"
    (dolist (object '(function class argument))
      (should (hel-thing-header-p object)))
    (should-not (hel-thing-header-p 'comment))))

;; Section "Five ways to extend": a new thing needs only its sources, and
;; its commands call the algorithms.
(ert-deftest hel-to-new-thing ()
  (unwind-protect
      (progn
        (put 'hel-to-probe 'hel-thing-sources
             (list (hel-motion-source #'hel--forward-sentence)))
        (hel-to--with-prose hel-to--sentences "Three"
          (hel--mark-text-object 'hel-to-probe :inner 1)
          (should (equal (hel-structural-tests--selection) "Three four."))
          (hel-structural-tests--goto "four")
          (hel--mark-to-thing-edge 'hel-to-probe 1 :end t)
          (should (equal (hel-structural-tests--selection) "four."))))
    (put 'hel-to-probe 'hel-thing-sources nil)))

;;; Word motions: helpers

(defun hel-word-motion-tests--render ()
  "Return the buffer text with the selection drawn into it."
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (p (- (point) (point-min)))
        (m (and (use-region-p) (- (mark t) (point-min)))))
    (if (not m)
        (concat (substring text 0 p) "|" (substring text p))
      (dolist (mk (sort (list (cons m "<") (cons p ">"))
                        (lambda (a b) (> (car a) (car b))))
                  text)
        (setq text (concat (substring text 0 (car mk))
                           (cdr mk)
                           (substring text (car mk))))))))

(defun hel-word-motion-tests--run (text start keys &optional extend)
  "Run KEYS from START in TEXT and return the text after each key.
START is the text at whose first match the cursor stands. A key is a
command, called with the count 1, or a list of a command and a count.
With EXTEND non-nil, the keys extend a selection that starts at the
cursor, as after `v'."
  (cl-letf (((symbol-function 'hel-reveal-point-when-on-top) #'ignore)
            ((symbol-function 'hel-push-point) #'ignore))
    (with-temp-buffer
      (let ((transient-mark-mode t)
            (hel--extend-selection extend))
        (insert text)
        (goto-char (point-min))
        (search-forward start)
        (goto-char (match-beginning 0))
        (if extend
            (progn (set-mark (point))
                   (activate-mark))
          (deactivate-mark))
        (mapcar (lambda (key)
                  (if (consp key)
                      (funcall (car key) (cadr key))
                    (funcall key 1))
                  (hel-word-motion-tests--render))
                keys)))))

;;; w and W

(ert-deftest hel-word-motion-w ()
  "`w' selects from point to the start of the next word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-start hel-forward-word-start
                    hel-forward-word-start))
                 '("<foo >bar  baz" "foo <bar  >baz" "foo bar  <baz>")))
  ;; From the middle of a word, and from the space between two words.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "oo " '(hel-forward-word-start))
                 '("f<oo >bar  baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" " bar" '(hel-forward-word-start))
                 '("foo< >bar  baz"))))

(ert-deftest hel-word-motion-w-end-of-buffer ()
  "`w' at the last word of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-start hel-forward-word-start))
                 '("foo bar  <baz>" "foo bar  baz|"))))

(ert-deftest hel-word-motion-w-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '((hel-forward-word-start 2)))
                 '("<foo bar  >baz"))))

(ert-deftest hel-word-motion-w-punctuation ()
  "A run of punctuation is a word of its own."
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-word-start hel-forward-word-start
                    hel-forward-word-start hel-forward-word-start
                    hel-forward-word-start))
                 '("<foo>.bar(baz) qux" "foo<.>bar(baz) qux"
                   "foo.<bar>(baz) qux" "foo.bar<(>baz) qux"
                   "foo.bar(<baz>) qux"))))

(ert-deftest hel-word-motion-w-line-ends ()
  "`w' stops at the end of a line, and takes the indentation as a step.
It skips an empty line."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "bar"
                  '(hel-forward-word-start hel-forward-word-start
                    hel-forward-word-start hel-forward-word-start
                    hel-forward-word-start))
                 '("foo <bar>\n  baz qux\n\nend"
                   "foo bar\n<  >baz qux\n\nend"
                   "foo bar\n  <baz >qux\n\nend"
                   "foo bar\n  baz <qux>\n\nend"
                   "foo bar\n  baz qux\n\n<end>"))))

(ert-deftest hel-word-motion-W ()
  "`W' treats every run of characters other than whitespace as one WORD."
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-WORD-start hel-forward-WORD-start))
                 '("<foo.bar(baz) >qux" "foo.bar(baz) <qux>"))))

;;; e and E

(ert-deftest hel-word-motion-e ()
  "`e' selects from point to the end of the word, then of the next word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-end hel-forward-word-end
                    hel-forward-word-end))
                 '("<foo> bar  baz" "foo< bar>  baz" "foo bar<  baz>"))))

(ert-deftest hel-word-motion-e-end-of-buffer ()
  "`e' at the end of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-end hel-forward-word-end))
                 '("foo bar  <baz>" "foo bar  baz|"))))

(ert-deftest hel-word-motion-e-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '((hel-forward-word-end 2)))
                 '("<foo bar>  baz"))))

(ert-deftest hel-word-motion-e-punctuation ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-word-end hel-forward-word-end
                    hel-forward-word-end))
                 '("<foo>.bar(baz) qux" "foo<.>bar(baz) qux"
                   "foo.<bar>(baz) qux"))))

(ert-deftest hel-word-motion-e-line-ends ()
  "`e' stops at the end of a line, and skips the line end and an empty line."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "bar"
                  '(hel-forward-word-end hel-forward-word-end
                    hel-forward-word-end hel-forward-word-end))
                 '("foo <bar>\n  baz qux\n\nend"
                   "foo bar\n<  baz> qux\n\nend"
                   "foo bar\n  baz< qux>\n\nend"
                   "foo bar\n  baz qux\n\n<end>"))))

(ert-deftest hel-word-motion-E ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-WORD-end hel-forward-WORD-end))
                 '("<foo.bar(baz)> qux" "foo.bar(baz)< qux>"))))

;;; b and B

(ert-deftest hel-word-motion-b ()
  "`b' selects from point back to the start of the previous word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-backward-word-start hel-backward-word-start))
                 '("foo >bar  <baz" ">foo <bar  baz")))
  ;; From the middle of a word: back to its start, without the character
  ;; under the cursor.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "az" '(hel-backward-word-start))
                 '("foo bar  >b<az"))))

(ert-deftest hel-word-motion-b-beginning-of-buffer ()
  "`b' at the start of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '(hel-backward-word-start))
                 '("|foo bar  baz"))))

(ert-deftest hel-word-motion-b-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz" '((hel-backward-word-start 2)))
                 '(">foo bar  <baz"))))

(ert-deftest hel-word-motion-b-punctuation ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "qux"
                  '(hel-backward-word-start hel-backward-word-start
                    hel-backward-word-start))
                 '("foo.bar(baz>) <qux" "foo.bar(>baz<) qux"
                   "foo.bar>(<baz) qux"))))

(ert-deftest hel-word-motion-b-line-ends ()
  "`b' skips the line end and an empty line before it starts.
It takes the indentation as a step."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "end"
                  '(hel-backward-word-start hel-backward-word-start
                    hel-backward-word-start hel-backward-word-start))
                 '("foo bar\n  baz >qux<\n\nend"
                   "foo bar\n  >baz <qux\n\nend"
                   "foo bar\n>  <baz qux\n\nend"
                   "foo >bar<\n  baz qux\n\nend"))))

(ert-deftest hel-word-motion-B ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "qux"
                  '(hel-backward-WORD-start hel-backward-WORD-start))
                 '(">foo.bar(baz) <qux" "|foo.bar(baz) qux"))))

;;; Extending the selection

(ert-deftest hel-word-motion-extend ()
  "With the selection extended, the mark stays and point moves."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-start hel-forward-word-start) t)
                 '("<foo >bar  baz" "<foo bar  >baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-backward-word-start hel-backward-word-start) t)
                 '("foo >bar  <baz" ">foo bar  <baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-end hel-forward-word-end) t)
                 '("<foo> bar  baz" "<foo bar>  baz"))))

;;; Tests from the document alone: helpers

(defvar hel-spec-queries-directory
  (let* ((here (or load-file-name buffer-file-name))
         (dir (and here
                   (expand-file-name "../queries/" (file-name-directory here)))))
    (if (and dir (file-directory-p dir))
        dir
      "/home/anuvyklack/code/helheim/hel/queries/"))
  "The query directory of this repository.")

(defun hel-spec--parse (text)
  "Return (CLEAN POINT MARK) for TEXT with the markers `|' and `^'."
  (let ((p (string-search "|" text))
        (m (string-search "^" text)))
    (when (and p m)
      (if (< m p) (cl-decf p) (cl-decf m)))
    (list (replace-regexp-in-string "[|^]" "" text)
          (if p (1+ p) 1)
          (and m (1+ m)))))

(defun hel-spec--parse-clean (text)
  "Return TEXT without its markers."
  (car (hel-spec--parse text)))

(defun hel-spec--setup (mode text)
  "Insert TEXT, turn on MODE and Hel, and place point and mark."
  (pcase-let ((`(,clean ,p ,m) (hel-spec--parse text)))
    (insert clean)
    (funcall mode)
    (unless hel-local-mode (hel-local-mode 1))
    (setq-local transient-mark-mode t)
    (setq mark-ring nil)
    (deactivate-mark)
    (if m
        (progn (set-mark m) (goto-char p))
      (goto-char p))))

(defmacro hel-spec--with (mode text &rest body)
  "Run BODY in a temporary buffer in MODE that holds TEXT.
TEXT marks point with `|' and the mark of a region with `^'."
  (declare (indent 2) (debug t))
  `(let ((hel-treesit-queries-directory hel-spec-queries-directory)
         (hel-treesit--queries hel-structural-tests--query-cache)
         (global-mark-ring nil)
         (last-command nil))
     ;; A temporary buffer has no window to recenter.
     (cl-letf (((symbol-function 'hel-reveal-point-when-on-top) #'ignore))
      (with-temp-buffer
       (hel-spec--setup ,mode ,text)
       ,@body))))

(defun hel-spec--render ()
  "Return the buffer text with the markers `|' and `^'."
  (let* ((s (buffer-substring-no-properties (point-min) (point-max)))
         (p (point))
         (m (and (use-region-p) (mark))))
    (if (null m)
        (concat (substring s 0 (1- p)) "|" (substring s (1- p)))
      (let* ((lo (min p m))
             (hi (max p m))
             (lo-mark (if (= lo p) "|" "^"))
             (hi-mark (if (= hi p) "|" "^")))
        (concat (substring s 0 (1- lo)) lo-mark
                (substring s (1- lo) (1- hi)) hi-mark
                (substring s (1- hi)))))))

(defun hel-spec--sel ()
  "Return the text of the active region, or nil."
  (and (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))))

(defun hel-spec--run (command &optional count)
  "Run COMMAND with COUNT for every cursor, as the command loop would."
  (let ((current-prefix-arg count)
        (this-command command))
    (hel-execute-command-for-all-cursors command))
  (setq last-command command))

(defun hel-spec--steps (command n &optional count)
  "Run COMMAND N times with COUNT and return the selected text after each."
  (cl-loop repeat n
           collect (progn (hel-spec--run command count)
                          (hel-spec--sel))))

(defun hel-spec--selections ()
  "Return the selected text of every cursor, in buffer order."
  (let ((all (cons (if (use-region-p)
                       (cons (region-beginning) (hel-spec--sel))
                     (cons (point) ""))
                   (mapcar (lambda (c)
                             (let ((p (marker-position (overlay-get c 'point)))
                                   (m (marker-position (overlay-get c 'mark))))
                               (cons (min p m)
                                     (buffer-substring-no-properties
                                      (min p m) (max p m)))))
                           (hel-all-fake-cursors)))))
    (mapcar #'cdr (sort all (lambda (a b) (< (car a) (car b)))))))

(defun hel-spec--unchanged (command text mode &optional count)
  "Return non-nil when COMMAND with COUNT leaves TEXT in MODE as it is.
The mark ring must stay as it is too."
  (hel-spec--with mode text
    (hel-spec--run command count)
    (and (equal (hel-spec--render) text)
         (null mark-ring))))

(defun hel-spec--grouped-p ()
  "Return non-nil when `treesit-query-capture' takes the GROUPED argument."
  (and (fboundp 'treesit-query-capture)
       (let ((arity (func-arity 'treesit-query-capture)))
         (and (numberp (cdr arity)) (<= 6 (cdr arity))))))

(defmacro hel-spec--need (&rest languages)
  "Skip the test unless every grammar in LANGUAGES is installed.
A macro, because `skip-unless' exists only inside `ert-deftest'."
  `(progn
     (skip-unless (fboundp 'treesit-language-available-p))
     ,@(mapcar (lambda (language)
                 `(skip-unless (treesit-language-available-p ,language)))
               languages)))

(defmacro hel-spec--need-queries (&rest languages)
  "Skip unless LANGUAGES have grammars and Emacs has the Helix producer."
  `(progn
     (hel-spec--need ,@languages)
     (skip-unless (hel-spec--grouped-p))))

(defun hel-spec--source-producer (thing)
  "Return the producer of the first source of THING that applies."
  (cl-some #'funcall (get thing 'hel-thing-sources)))

;;; Text of the tree command tests

(defconst hel-spec--py
  "def f():\n    return g(abc, [1, 2])\nx = 1\n"
  "The Python text of the tree command tests (section Tests).")

(defun hel-spec--py (line2 &optional line3)
  "Return `hel-spec--py' with LINE2 and LINE3 as the second and third lines."
  (concat "def f():\n    " line2 "\n" (or line3 "x = 1") "\n"))

;;; Expand and shrink with a parser

(ert-deftest hel-spec-expand-from-abc ()
  "Tests: expand from abc gives abc, the argument list, the call and the
return statement (The core picture; Selection rule 1)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(|abc, [1, 2])")
    (should (equal (hel-spec--steps #'hel-expand-selection 4)
                   '("abc" "(abc, [1, 2])" "g(abc, [1, 2])"
                     "return g(abc, [1, 2])")))))

(ert-deftest hel-spec-expand-from-partial-region ()
  "Tests: expand from the region `abc, [1' reaches `(abc, [1, 2])'.
Rule 1 and rule 7: expand keeps a forward selection forward."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(^abc, [1|, 2])")
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render)
                   (hel-spec--py "return g|(abc, [1, 2])^")))))

(ert-deftest hel-spec-expand-on-bracket ()
  "Tests: with the cursor on `[', the first expand selects `[1, 2]'.
Rule 10: M-o starts from the smallest named node that starts at point."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, |[1, 2])")
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--sel) "[1, 2]"))))

(ert-deftest hel-spec-expand-shrink-roundtrip-parser ()
  "Tests: shrink after three expands returns to the starting cursor (rule 3)."
  (hel-spec--need 'python)
  (let ((text (hel-spec--py "return g(|abc, [1, 2])")))
    (hel-spec--with #'python-ts-mode text
      (hel-spec--run #'hel-expand-selection 3)
      (should (equal (hel-spec--sel) "g(abc, [1, 2])"))
      (hel-spec--run #'hel-shrink-selection 3)
      (should-not (use-region-p))
      (should (equal (hel-spec--render) text)))))

(ert-deftest hel-spec-expand-shrink-retrace-parser ()
  "Rule 3: each M-i goes back one step along the exact path of M-o."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(|abc, [1, 2])")
    (hel-spec--steps #'hel-expand-selection 3)
    (should (equal (hel-spec--steps #'hel-shrink-selection 2)
                   '("(abc, [1, 2])" "abc")))))

(ert-deftest hel-spec-shrink-first-named-child ()
  "Tests: shrink without a history selects the first named child (rule 3).
Rule 7: shrink keeps the direction of the selection."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return ^g(abc, [1, 2])|")
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--render) (hel-spec--py "return ^g|(abc, [1, 2])")))))

(ert-deftest hel-spec-shrink-first-named-child-backward ()
  "Rule 7: shrink keeps a backward selection backward."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return |g(abc, [1, 2])^")
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--render) (hel-spec--py "return |g^(abc, [1, 2])")))))

(ert-deftest hel-spec-shrink-first-named-child-count ()
  "Tests: a count repeats shrink without a history (rules 3 and 8)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "^return g(abc, [1, 2])|")
    (hel-spec--run #'hel-shrink-selection 2)
    (should (equal (hel-spec--render) (hel-spec--py "return ^g|(abc, [1, 2])")))))

;;; Siblings with a parser

(ert-deftest hel-spec-next-sibling-skips-comma ()
  "Tests: M-n skips commas and puts point at the end (rules 2 and 7)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(^abc|, [1, 2])")
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc, ^[1, 2]|)")))))

(ert-deftest hel-spec-previous-sibling-skips-comma ()
  "Tests: M-p skips commas and keeps the direction (rules 2 and 7)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, ^[1, 2]|)")
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) (hel-spec--py "return g(^abc|, [1, 2])")))))

(ert-deftest hel-spec-next-sibling-count ()
  "Rule 8: `3 M-n' moves three siblings forward."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "f(^a|, b, c, d)\n"
    (hel-spec--run #'hel-select-next-sibling 3)
    (should (equal (hel-spec--render) "f(a, b, c, ^d|)\n"))))

(ert-deftest hel-spec-next-sibling-negative-count ()
  "Rule 8: a negative count reverses the command, so `-1 M-n' acts as M-p."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "f(a, b, c, ^d|)\n"
    (hel-spec--run #'hel-select-next-sibling -1)
    (should (equal (hel-spec--render) "f(a, b, ^c|, d)\n"))))

(ert-deftest hel-spec-next-sibling-at-last-argument ()
  "Tests: M-n at the last argument leaves the selection as it is.
Section Selection rules: a sibling command stays inside the parent."
  (hel-spec--need 'python)
  (should (hel-spec--unchanged #'hel-select-next-sibling
                               (hel-spec--py "return g(abc, ^[1, 2]|)")
                               #'python-ts-mode)))

(ert-deftest hel-spec-previous-sibling-through-wrapper ()
  "Tests: M-p on `x = 1' selects the function before it, through the
wrapper `expression_statement'."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, [1, 2])" "^x = 1|")
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render)
                   "^def f():\n    return g(abc, [1, 2])|\nx = 1\n"))))

(ert-deftest hel-spec-previous-sibling-through-wrapper-bare ()
  "Rule 10: from a bare point at `x', M-p takes the last child of the
module that ends before point: the function."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, [1, 2])" "|x = 1")
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render)
                   "|def f():\n    return g(abc, [1, 2])^\nx = 1\n"))))

(ert-deftest hel-spec-next-sibling-at-last-statement ()
  "Tests: M-n at the last statement leaves the selection as it is."
  (hel-spec--need 'python)
  (should (hel-spec--unchanged #'hel-select-next-sibling
                               (hel-spec--py "return g(abc, [1, 2])" "^x = 1|")
                               #'python-ts-mode)))

(ert-deftest hel-spec-sibling-trims-selection ()
  "Rule 10: the node at a selection ignores the whitespace at its edges.
The selection `x ' starts from the identifier x, whose next sibling is 1."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "^x |= 1\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "x = ^1|\n"))))

;;; Rule 10: bare point, with a parser

(defun hel-spec--bare (mode text)
  "Return the selections that M-n, M-p and M-o give from TEXT in MODE."
  (mapcar (lambda (command)
            (hel-spec--with mode text
              (hel-spec--run command)
              (hel-spec--sel)))
          (list #'hel-select-next-sibling
                #'hel-select-previous-sibling
                #'hel-expand-selection)))

(ert-deftest hel-spec-bare-point-before-list-parser ()
  "Rule 10 table, row `name |(mode state)', in Python."
  (hel-spec--need 'python)
  (should (equal (hel-spec--bare #'python-ts-mode "name |(mode, state)\n")
                 '("(mode, state)" "name" "(mode, state)"))))

(ert-deftest hel-spec-bare-point-after-name-parser ()
  "Rule 10 table, row `name| (mode state)', in Python."
  (hel-spec--need 'python)
  (should (equal (hel-spec--bare #'python-ts-mode "name| (mode, state)\n")
                 '("(mode, state)" "name" "name"))))

(ert-deftest hel-spec-bare-point-in-parameter-parser ()
  "Rule 10 table, row `Callback |_callback,', in C."
  (hel-spec--need 'c)
  (should (equal (hel-spec--bare #'c-ts-mode
                                 "void f(Callback |_callback, int x);\n")
                 '("_callback" "Callback" "_callback"))))

(ert-deftest hel-spec-bare-point-before-comma-parser ()
  "Rule 10 table, row `Callback _callback|,', in C."
  (hel-spec--need 'c)
  (should (equal (hel-spec--bare #'c-ts-mode
                                 "void f(Callback _callback|, int x);\n")
                 '("int x" "Callback _callback" "_callback"))))

(ert-deftest hel-spec-bare-point-in-token-parser ()
  "Rule 10 table, row `_cal|lback': every command starts from the token."
  (hel-spec--need 'python)
  (should (equal (hel-spec--bare #'python-ts-mode "f(_cal|lback)\n")
                 '("_callback" "_callback" "_callback"))))

(ert-deftest hel-spec-bare-point-between-name-and-call-parser ()
  "Rule 10 table, row `foo|(x)', in Python."
  (hel-spec--need 'python)
  (should (equal (hel-spec--bare #'python-ts-mode "foo|(x)\n")
                 '("(x)" "foo" "(x)"))))

(ert-deftest hel-spec-bare-point-in-whitespace-parser ()
  "Rule 10 table, row `(a | b)', in a Python tuple: nothing touches point."
  (hel-spec--need 'python)
  (should (equal (hel-spec--bare #'python-ts-mode "(a, | b)\n")
                 '("b" "a" "(a,  b)"))))

(ert-deftest hel-spec-parent-start-after-abc ()
  "Tests: M-b at `abc|,' selects abc (rule 10: M-b looks backward first)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc|, [1, 2])")
    (hel-spec--run #'hel-parent-node-start)
    (should (equal (hel-spec--render) (hel-spec--py "return g(|abc^, [1, 2])")))))

;;; Parent commands with a parser

(ert-deftest hel-spec-parent-end-climbs ()
  "Tests: three M-e reach the ends of 1, of [1, 2] and of the argument list.
Each M-e selects from point to the target."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, [|1, 2])")
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc, [^1|, 2])")))
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc, [1^, 2]|)")))
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc, [1, 2]^)|")))))

(ert-deftest hel-spec-parent-start-climbs ()
  "Tests: three M-b reach the starts of [1, 2], of the argument list and
of the call."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, [|1, 2])")
    (hel-spec--run #'hel-parent-node-start)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc, |[^1, 2])")))
    (hel-spec--run #'hel-parent-node-start)
    (should (equal (hel-spec--render) (hel-spec--py "return g|(abc, ^[1, 2])")))
    (hel-spec--run #'hel-parent-node-start)
    (should (equal (hel-spec--render) (hel-spec--py "return |g^(abc, [1, 2])")))))

(ert-deftest hel-spec-parent-end-from-point ()
  "Tests: M-e selects from point (the parent commands select, as gl does)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(ab|c, [1, 2])")
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) (hel-spec--py "return g(ab^c|, [1, 2])")))))

(ert-deftest hel-spec-parent-start-after-forward-abc ()
  "Tests: after a forward selection of abc, M-b selects abc backward."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(^abc|, [1, 2])")
    (hel-spec--run #'hel-parent-node-start)
    (should (equal (hel-spec--render) (hel-spec--py "return g(|abc^, [1, 2])")))))

(ert-deftest hel-spec-parent-end-after-forward-abc ()
  "Tests: after a forward selection of abc, M-e selects to the end of the
argument list, because the cursor is on c."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(^abc|, [1, 2])")
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) (hel-spec--py "return g(abc^, [1, 2])|")))))

;;; Select all siblings and children with a parser

(ert-deftest hel-spec-select-all-siblings ()
  "Tests: M-a on abc puts cursors on abc and [1, 2]."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(a|bc, [1, 2])")
    (hel-spec--run #'hel-select-all-siblings)
    (should (equal (hel-spec--selections) '("abc" "[1, 2]")))
    (should (null mark-ring))))

(ert-deftest hel-spec-select-all-children-list ()
  "Tests: M-I on [1, 2] puts cursors on 1 and 2."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, ^[1, 2]|)")
    (hel-spec--run #'hel-select-all-children)
    (should (equal (hel-spec--selections) '("1" "2")))
    (should (null mark-ring))))

(ert-deftest hel-spec-select-all-children-skips-wrapper ()
  "Tests: M-I on `x = 1' puts cursors on x and 1.  The
`expression_statement' has one child with its own range, so it is skipped."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--py "return g(abc, [1, 2])" "^x = 1|")
    (hel-spec--run #'hel-select-all-children)
    (should (equal (hel-spec--selections) '("x" "1")))))

(ert-deftest hel-spec-select-all-children-leaf ()
  "Tests: M-I on a leaf node keeps the selection."
  (hel-spec--need 'python)
  (let ((text (hel-spec--py "return g(^abc|, [1, 2])")))
    (hel-spec--with #'python-ts-mode text
      (hel-spec--run #'hel-select-all-children)
      (should (equal (hel-spec--render) text))
      (should-not (hel-all-fake-cursors)))))

(ert-deftest hel-spec-select-all-children-no-replay ()
  "Tests: M-I on [[1, 2], [3, 4]], run through the replay of Hel for all
cursors, puts cursors on [1, 2] and [3, 4] only (rule 6)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = ^[[1, 2], [3, 4]]|\n"
    (hel-spec--run #'hel-select-all-children)
    (should (equal (hel-spec--selections) '("[1, 2]" "[3, 4]")))))

;;; Rule 10: bare point, without a parser

(ert-deftest hel-spec-bare-point-before-list-syntax ()
  "Rule 10 table, row `name |(mode state)', without a parser."
  (should (equal (hel-spec--bare #'c-mode "g(name |(mode state));\n")
                 '("(mode state)" "name" "(mode state)"))))

(ert-deftest hel-spec-bare-point-after-name-syntax ()
  "Rule 10 table, row `name| (mode state)', without a parser."
  (should (equal (hel-spec--bare #'c-mode "g(name| (mode state));\n")
                 '("(mode state)" "name" "name"))))

(ert-deftest hel-spec-bare-point-in-parameter-syntax ()
  "Rule 10 table, row `Callback |_callback,', without a parser."
  (should (equal (hel-spec--bare #'c-mode "f(Callback |_callback, int x);\n")
                 '("_callback" "Callback" "_callback"))))

(ert-deftest hel-spec-bare-point-before-comma-syntax ()
  "Rule 10 table, row `Callback _callback|,', without a parser.  The
children of the bracket pair are sexps, so the next one is `int'."
  (should (equal (hel-spec--bare #'c-mode "f(Callback _callback|, int x);\n")
                 '("int" "_callback" "_callback"))))

(ert-deftest hel-spec-bare-point-in-token-syntax ()
  "Rule 10 table, row `_cal|lback', without a parser.  The syntax table
gives expand steps smaller than a sexp, so M-o selects the word first
\(The syntax producer)."
  (should (equal (hel-spec--bare #'c-mode "f(_cal|lback);\n")
                 '("_callback" "_callback" "callback"))))

(ert-deftest hel-spec-bare-point-between-name-and-call-syntax ()
  "Rule 10 table, row `foo|(x)', without a parser: the children of the
buffer are the top-level sexps."
  (should (equal (hel-spec--bare #'c-mode "foo|(x);\n")
                 '("(x)" "foo" "(x)"))))

(ert-deftest hel-spec-bare-point-in-whitespace-syntax ()
  "Rule 10 table, row `(a | b)', without a parser.  The inside of a
bracket pair is an expand step of the syntax table, so M-o selects it
before the pair (The syntax producer)."
  (should (equal (hel-spec--bare #'c-mode "(a | b);\n")
                 '("b" "a" "a  b"))))

;;; Decisions about extending, the side of point and the Lisp modes

(defmacro hel-spec--with-v (mode text &rest body)
  "Run BODY as `hel-spec--with' does, with the selection extended, as after `v'."
  (declare (indent 2) (debug t))
  `(hel-spec--with ,mode ,text
     (hel-extend-selection 1)
     ,@body))

(ert-deftest hel-spec-v-sibling-extends ()
  "Rule 7: after `v', M-n grows the selection and keeps the mark."
  (dolist (mode '(emacs-lisp-mode c-mode))
    (hel-spec--with-v mode "(a ^b| c d)\n"
      (hel-spec--run #'hel-select-next-sibling)
      (should (equal (hel-spec--render) "(a ^b c| d)\n"))
      (hel-spec--run #'hel-select-next-sibling)
      (should (equal (hel-spec--render) "(a ^b c d|)\n"))
      (hel-spec--run #'hel-select-next-sibling)
      (should (equal (hel-spec--render) "(a ^b c d|)\n")))))

(ert-deftest hel-spec-v-sibling-gives-back-and-turns ()
  "Rule 7: after `v', a sibling command against the selection gives back its
last node, and at the last node it keeps that node and turns around."
  (hel-spec--with-v #'emacs-lisp-mode "(a ^b c| d)\n"
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "(a ^b| c d)\n"))
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "(|a b^ c d)\n")))
  (hel-spec--with-v #'emacs-lisp-mode "(a b |c^ d)\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "(a b ^c d|)\n"))))

(ert-deftest hel-spec-v-sibling-parser ()
  "Rule 7: after `v', M-n extends over the named siblings with a parser, and
a node with a dot in it moves as one node."
  (hel-spec--need 'python)
  (hel-spec--with-v #'python-ts-mode "g(^a|, b.c, d)\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "g(^a, b.c|, d)\n"))
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "g(^a, b.c, d|)\n"))
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "g(^a, b.c|, d)\n"))))

(ert-deftest hel-spec-v-sibling-bare-point ()
  "Rule 7: after `v' at a bare point, M-n extends from point over the node."
  (hel-spec--with-v #'emacs-lisp-mode "(a| b c)\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "(a^ b| c)\n"))))

(ert-deftest hel-spec-v-object-keys-create-selections ()
  "Rule 7: `v' works with motions.  `mi' and `ma' create a selection, so they
replace the selection, as without `v'."
  (hel-spec--with-v #'text-mode "foo^ bar| baz\n"
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "foo |bar^ baz\n")))
  (hel-spec--with-v #'emacs-lisp-mode "^(a b| c)\n"
    (hel-spec--run #'hel-mark-inner-paren)
    (should (equal (hel-spec--render) "(^a b c|)\n"))))

(ert-deftest hel-spec-expand-point-at-start ()
  "Rule 7: expand puts point at the start of the selection, from a bare point
and from a selection in either direction."
  (hel-spec--with #'emacs-lisp-mode "(foo (bar| baz) qux)\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "(foo (|bar^ baz) qux)\n"))
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "(foo |(bar baz)^ qux)\n")))
  (hel-spec--with #'emacs-lisp-mode "(foo (^bar| baz) qux)\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "(foo |(bar baz)^ qux)\n")))
  (hel-spec--with #'c-mode "f(^ab|c);\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "f(|abc^);\n")))
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "g(^a|, b)\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "g|(a, b)^\n"))))

(ert-deftest hel-spec-sibling-keeps-direction ()
  "Rule 7: M-n and M-p keep the direction of the selection, in both
directions of the motion, over a count, and at the last sibling."
  (hel-spec--with #'c-mode "foo(alpha, |beta^, gamma, delta);\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "foo(alpha, beta, |gamma^, delta);\n"))
    (hel-spec--run #'hel-select-next-sibling 5)
    (should (equal (hel-spec--render) "foo(alpha, beta, gamma, |delta^);\n"))
    (hel-spec--run #'hel-select-previous-sibling 2)
    (should (equal (hel-spec--render) "foo(alpha, |beta^, gamma, delta);\n")))
  (hel-spec--with #'c-mode "foo(alpha, ^beta|, gamma);\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "foo(alpha, beta, ^gamma|);\n"))))

(ert-deftest hel-spec-sibling-after-function-object ()
  "Rule 7: after `mif' point is at the header, and M-n shows the header of
the next function."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "def f():\n    pa|ss\n\ndef g():\n    pass\n"
    (hel-spec--run #'hel-mark-inner-function)
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render)
                   "def f():\n    pass\n\n|def g():\n    pass^\n"))))

(ert-deftest hel-spec-sibling-bare-point-direction ()
  "Rule 7: from a bare point, the new selection has the direction of the move."
  (hel-spec--with #'emacs-lisp-mode "(a |b c)\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "(a ^b| c)\n")))
  (hel-spec--with #'emacs-lisp-mode "(a b| c)\n"
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "(a |b^ c)\n")))
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "g(a, |b)\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "g(a, ^b|)\n")))
  (hel-spec--with #'python-ts-mode "g(a|, b)\n"
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "g(|a^, b)\n"))))

(ert-deftest hel-spec-shrink-bare-point ()
  "Rule 3: at a bare point, M-i selects the first child of the node after
point, and does nothing when that node has no children."
  (hel-spec--with #'emacs-lisp-mode "(foo |(bar baz) qux)\n"
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--render) "(foo (^bar| baz) qux)\n")))
  (should (hel-spec--unchanged #'hel-shrink-selection "(|foo bar)\n" #'emacs-lisp-mode))
  (should (hel-spec--unchanged #'hel-shrink-selection "(foo b|ar)\n" #'emacs-lisp-mode))
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = |[1, 2]\n"
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--render) "x = [^1|, 2]\n"))))

(ert-deftest hel-spec-side-keys ()
  "H and L: in Lisp they expand with point on their side.  Elsewhere they
select to the edge of the node or of its parent."
  (hel-spec--with #'emacs-lisp-mode "(a (b| c) d)\n"
    (hel-spec--run #'hel-select-node-end)
    (should (equal (hel-spec--render) "(a (^b| c) d)\n"))
    (hel-spec--run #'hel-select-node-end)
    (should (equal (hel-spec--render) "(a ^(b c)| d)\n"))
    (hel-spec--run #'hel-select-node-start)
    (should (equal (hel-spec--render) "|(a (b c) d)^\n")))
  (hel-spec--with #'c-mode "f(^abc|, x);\n"
    (hel-spec--run #'hel-select-node-end)
    (should (equal (hel-spec--render) "f(abc^, x)|;\n"))))

(ert-deftest hel-spec-lisp-modes ()
  "`hel-lisp-modes': every Lisp mode takes its sexps as nodes, so expand has no
step for the inside of a list."
  (dolist (mode '(lisp-mode scheme-mode lisp-interaction-mode))
    (hel-spec--with mode "(foo (bar| baz) qux)\n"
      (should (hel-lisp-node-producer-p (hel--producer 'node)))
      (should (equal (hel-spec--steps #'hel-expand-selection 2)
                     '("bar" "(bar baz)")))))
  (hel-spec--with #'c-mode "f(a| b);\n"
    (should-not (hel-lisp-node-producer-p (hel--producer 'node)))))

(ert-deftest hel-spec-comment-sibling-lisp ()
  "Rule 10: in Lisp a comment is a token, so after M-o selects it, M-n and
M-p go on to the sexps around it."
  (hel-spec--with #'emacs-lisp-mode "(w)\n;; com|ment here\n(x)\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--sel) ";; comment here"))
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--sel) "(x)")))
  (hel-spec--with #'emacs-lisp-mode "(w)\n;; com|ment here\n(x)\n"
    (hel-spec--run #'hel-expand-selection)
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--sel) "(w)"))))

(ert-deftest hel-spec-bare-point-one-space ()
  "Rule 10 table: the rows with one space and the rows with two.  A position
takes up no space, so `(a | b)' has two spaces.  In Lisp, M-o goes from the
node to the list.  The syntax table adds the inside of the brackets."
  (cl-flet ((steps (mode text)
              (hel-spec--with mode text
                (hel-spec--steps #'hel-expand-selection 2))))
    (should (equal (hel-spec--bare #'emacs-lisp-mode "(a| b)\n") '("b" "a" "a")))
    (should (equal (hel-spec--bare #'emacs-lisp-mode "(a |b)\n") '("b" "a" "b")))
    (should (equal (steps #'emacs-lisp-mode "(a| b)\n") '("a" "(a b)")))
    (should (equal (steps #'emacs-lisp-mode "(a |b)\n") '("b" "(a b)")))
    (should (equal (hel-spec--bare #'emacs-lisp-mode "(a | b)\n")
                   '("b" "a" "(a  b)")))
    (should (equal (steps #'c-mode "(a| b);\n") '("a" "a b")))
    (should (equal (steps #'c-mode "(a |b);\n") '("b" "a b")))
    (should (equal (steps #'c-mode "(a | b);\n") '("a  b" "(a  b)")))))

(ert-deftest hel-spec-bare-point-in-string-syntax ()
  "Rule 10: without a parser, M-n in a string starts from the whole string."
  (hel-spec--with #'c-mode "f(\"ab|c\", x);\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--sel) "\"abc\""))))

(ert-deftest hel-spec-select-all-siblings-bare-whitespace ()
  "Rule 10: M-a in `(f a | b)' puts a cursor on f, a and b."
  (hel-spec--with #'c-mode "(f a | b);\n"
    (hel-spec--run #'hel-select-all-siblings)
    (should (equal (hel-spec--selections) '("f" "a" "b")))))

;;; Node commands without a parser

(ert-deftest hel-spec-syntax-next-sibling ()
  "Node commands without a parser: M-n steps over the sexps of the
argument list and stays at the last one."
  (hel-spec--with #'c-mode "foo(^alpha|, beta, gamma);\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "foo(alpha, ^beta|, gamma);\n"))
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "foo(alpha, beta, ^gamma|);\n"))
    (hel-spec--run #'hel-select-next-sibling)
    (should (equal (hel-spec--render) "foo(alpha, beta, ^gamma|);\n"))))

(ert-deftest hel-spec-syntax-previous-sibling ()
  "Node commands without a parser: M-p steps back and stays at the first
sexp."
  (hel-spec--with #'c-mode "foo(alpha, ^beta|, gamma);\n"
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "foo(^alpha|, beta, gamma);\n"))
    (hel-spec--run #'hel-select-previous-sibling)
    (should (equal (hel-spec--render) "foo(^alpha|, beta, gamma);\n"))))

(ert-deftest hel-spec-syntax-parent-end-climbs ()
  "Node commands without a parser: M-e selects to the end of the argument
list, and a second M-e climbs to the next bracket pair."
  (hel-spec--with #'c-mode "bar(foo(^alpha|, beta, gamma));\n"
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) "bar(foo(alpha^, beta, gamma)|);\n"))
    (hel-spec--run #'hel-parent-node-end)
    (should (equal (hel-spec--render) "bar(foo(alpha, beta, gamma)^)|;\n"))))

(ert-deftest hel-spec-syntax-select-all-siblings ()
  "Node commands without a parser: M-a puts a cursor on each sexp inside
the argument list."
  (hel-spec--with #'c-mode "foo(alpha, be|ta, gamma);\n"
    (hel-spec--run #'hel-select-all-siblings)
    (should (equal (hel-spec--selections) '("alpha" "beta" "gamma")))))

(ert-deftest hel-spec-syntax-select-all-children ()
  "Node commands without a parser: M-I selects every sexp inside the
bracket pair under the selection."
  (hel-spec--with #'c-mode "foo^(alpha, beta)|;\n"
    (hel-spec--run #'hel-select-all-children)
    (should (equal (hel-spec--selections) '("alpha" "beta")))))

;;; The syntax producer of expand

(defconst hel-spec--prose
  "First sentence here. Second one, with (a paren|thetical remark) inside.\n"
  "The prose of section The syntax producer, with point on \"parenthetical\".")

(ert-deftest hel-spec-syntax-expand-prose ()
  "Tests: five M-o in text-mode give the five steps of section The syntax
producer, and a sixth M-o leaves the paragraph selected."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode hel-spec--prose
      (should (equal (hel-spec--steps #'hel-expand-selection 6)
                     (list "parenthetical"
                           "a parenthetical remark"
                           "(a parenthetical remark)"
                           "Second one, with (a parenthetical remark) inside."
                           (hel-spec--parse-clean hel-spec--prose)
                           (hel-spec--parse-clean hel-spec--prose)))))))

(ert-deftest hel-spec-syntax-expand-double-space ()
  "Tests: with `sentence-end-double-space' set to t, the sentence step
spans the whole line without its newline."
  (let ((sentence-end-double-space t))
    (hel-spec--with #'text-mode hel-spec--prose
      (should (equal (nth 3 (hel-spec--steps #'hel-expand-selection 4))
                     (string-trim-right
                      (hel-spec--parse-clean hel-spec--prose)))))))

(ert-deftest hel-spec-syntax-expand-in-comment ()
  "Tests: in python-mode the same text in a comment gives no sentence step
and no bracket steps.  Expand goes from the word to the inside of the
comment, then to the whole comment without the newline."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'python-mode (concat "# " hel-spec--prose)
      (let ((steps (hel-spec--steps #'hel-expand-selection 3)))
        (should (equal (nth 0 steps) "parenthetical"))
        (should (string-prefix-p "First" (string-trim (nth 1 steps))))
        (should (string-suffix-p "inside." (nth 1 steps)))
        (should (equal (nth 2 steps)
                       (string-trim-right
                        (concat "# " (hel-spec--parse-clean hel-spec--prose)))))
        (should-not (member "(a parenthetical remark)" steps))
        (should-not (member "a parenthetical remark" steps))))))

(ert-deftest hel-spec-syntax-expand-string ()
  "Tests: with point inside a string, expand selects the inside of the
string before the whole string."
  (hel-spec--with #'c-mode "f(\"some te|xt\");\n"
    (should (equal (hel-spec--steps #'hel-expand-selection 3)
                   '("text" "some text" "\"some text\"")))))

(ert-deftest hel-spec-syntax-expand-on-open-paren ()
  "Tests: with the cursor on `(', the first M-o selects the whole form."
  (hel-spec--with #'c-mode "f|(x, y);\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--sel) "(x, y)"))))

(ert-deftest hel-spec-syntax-expand-on-open-quote ()
  "Tests: with the cursor on an opening quote, the first M-o selects the
whole string."
  (hel-spec--with #'c-mode "f(|\"str\");\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--sel) "\"str\""))))

(ert-deftest hel-spec-syntax-expand-elisp-comment ()
  "Tests: in emacs-lisp-mode a comment is a token, as in a tree-sitter
buffer, so the first M-o selects the whole comment without its newline."
  (hel-spec--with #'emacs-lisp-mode "(foo)\n;; some wo|rd here\n(bar)\n"
    (should (equal (hel-spec--steps #'hel-expand-selection 1)
                   '(";; some word here")))))

(ert-deftest hel-spec-syntax-expand-c-comment ()
  "Tests: a comment in c-mode gives the word, the inside and the whole
comment."
  (hel-spec--with #'c-mode "int x;\n/* some wo|rd here */\nint y;\n"
    (should (equal (hel-spec--steps #'hel-expand-selection 3)
                   '("word" "some word here" "/* some word here */")))))

(ert-deftest hel-spec-syntax-expand-two-brackets ()
  "Tests: expand from a selection that spans two brackets reaches the
smallest bracket pair that contains both (its inside, by rule 1)."
  (hel-spec--with #'c-mode "f((^b), (c|));\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--sel) "(b), (c)"))))

(ert-deftest hel-spec-syntax-expand-keeps-direction ()
  "Rule 7: expand puts point at the start of a backward selection too."
  (hel-spec--with #'c-mode "f(|ab^c);\n"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--render) "f(|abc^);\n"))))

(ert-deftest hel-spec-syntax-expand-shrink-roundtrip ()
  "Tests: `3 M-o' followed by `3 M-i' returns to the empty selection at the
starting point, and repeated M-i retraces the path of the expands."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode hel-spec--prose
      (hel-spec--run #'hel-expand-selection 3)
      (should (equal (hel-spec--sel) "(a parenthetical remark)"))
      (should (equal (hel-spec--steps #'hel-shrink-selection 2)
                     '("a parenthetical remark" "parenthetical")))
      (hel-spec--run #'hel-shrink-selection)
      (should-not (use-region-p))
      (should (equal (hel-spec--render) hel-spec--prose)))))

(ert-deftest hel-spec-syntax-negative-counts ()
  "Tests: `-3 M-i' expands three times, and `-3 M-o' shrinks back (rule 8)."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode hel-spec--prose
      (hel-spec--run #'hel-shrink-selection -3)
      (should (equal (hel-spec--sel) "(a parenthetical remark)"))
      (hel-spec--run #'hel-expand-selection -3)
      (should-not (use-region-p))
      (should (equal (hel-spec--render) hel-spec--prose)))))

(ert-deftest hel-spec-syntax-shrink-without-history ()
  "Tests: shrink without a history selects the first sexp inside the
region."
  (hel-spec--with #'c-mode "x = ^foo(a) + bar|;\n"
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--render) "x = ^foo|(a) + bar;\n"))))

(ert-deftest hel-spec-syntax-shrink-after-other-command ()
  "Tests: shrink after another command changed the selection selects the
first sexp inside the region (rule 3: the region does not match the
history)."
  (hel-spec--with #'c-mode "x = foo(a) + b|ar;\n"
    (hel-spec--run #'hel-expand-selection)
    (hel-select (cons 5 15))            ; "foo(a) + b"
    (setq last-command 'ignore)
    (hel-spec--run #'hel-shrink-selection)
    (should (equal (hel-spec--sel) "foo"))))

(ert-deftest hel-spec-syntax-shrink-no-sexps ()
  "Tests: shrink leaves a region without sexps inside as it is."
  (should (hel-spec--unchanged #'hel-shrink-selection
                               "x = 1;^   |\ny = 2;\n" #'c-mode)))

(ert-deftest hel-spec-fake-cursor-history ()
  "Tests: a fake cursor's expand does not change the history of the main
cursor (rule 4)."
  (hel-spec--with #'text-mode "f|oo bar\nbaz qux\n"
    (hel-create-fake-cursor 14)         ; in "qux"
    (hel-spec--run #'hel-expand-selection)
    (should (equal (hel-spec--selections) '("foo" "qux")))
    (hel-disable-multiple-cursors-mode)
    (hel-spec--run #'hel-shrink-selection)
    (should-not (use-region-p))
    (should (= (point) 2))))

;;; Mark ring and command properties (rule 6)

(ert-deftest hel-spec-expand-series-pushes-once ()
  "Rule 6: expand pushes point only at the start of a series of expands
and shrinks, so the series leaves one entry."
  (hel-spec--with #'c-mode "f(a, (b|c));\n"
    (hel-spec--run #'hel-expand-selection)
    (hel-spec--run #'hel-expand-selection)
    (hel-spec--run #'hel-shrink-selection)
    (hel-spec--run #'hel-expand-selection)
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-spec-shrink-does-not-push ()
  "Rule 6: shrink does not push point to the mark ring."
  (hel-spec--with #'c-mode "x = ^foo(a) + bar|;\n"
    (hel-spec--run #'hel-shrink-selection)
    (should (null mark-ring))))

(ert-deftest hel-spec-sibling-pushes-point ()
  "Rule 6: a sibling command pushes the old point when it changes the
selection."
  (hel-spec--with #'c-mode "foo(^alpha|, beta);\n"
    (hel-spec--run #'hel-select-next-sibling)
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-spec-command-properties ()
  "Rule 6: the commands use `:multiple-cursors t', except select all
siblings and select all children, which use `:multiple-cursors nil'."
  (dolist (command '(hel-expand-selection hel-shrink-selection
                     hel-select-next-sibling hel-select-previous-sibling
                     hel-parent-node-start hel-parent-node-end
                     hel-mark-inner-function hel-mark-a-function
                     hel-mark-function-forward hel-mark-function-backward
                     hel-backward-word-end))
    (should (eq t (get command 'multiple-cursors))))
  (dolist (command '(hel-select-all-siblings hel-select-all-children))
    (should (plist-member (symbol-plist command) 'multiple-cursors))
    (should (null (get command 'multiple-cursors)))))

(ert-deftest hel-spec-merge-properties ()
  "Section Selecting an object: mi and ma set `:merge-selections t'.
Section Commands are thin: ]f always merges, and ge merges only when the
selection extends."
  (dolist (command '(hel-mark-inner-function hel-mark-a-function
                     hel-mark-inner-word hel-mark-a-argument
                     hel-mark-function-forward))
    (should (eq t (get command 'merge-selections))))
  (should (eq 'hel--extend-selection
              (get 'hel-backward-word-end 'merge-selections))))

(ert-deftest hel-spec-eldoc-registration ()
  "Rule 6: motion commands are registered with `eldoc-add-command'."
  (dolist (command '(hel-select-next-sibling hel-select-previous-sibling
                     hel-parent-node-start hel-parent-node-end))
    (should (intern-soft (symbol-name command) eldoc-message-commands))))

;;; Key bindings

(ert-deftest hel-spec-key-bindings ()
  "Sections Node selection, Text objects and motions, Key bindings: the
keys are plain bindings in `hel-normal-state-map'."
  (pcase-dolist (`(,key . ,command)
                 '(("M-o" . hel-expand-selection)
                   ("M-i" . hel-shrink-selection)
                   ("M-n" . hel-select-next-sibling)
                   ("M-p" . hel-select-previous-sibling)
                   ("M-b" . hel-parent-node-start)
                   ("M-e" . hel-parent-node-end)
                   ("H" . hel-select-node-start)
                   ("L" . hel-select-node-end)
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
                   ("] p" . hel-mark-paragraph-forward)
                   ("[ p" . hel-mark-paragraph-backward)
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

(ert-deftest hel-spec-key-bindings-C-hjkl ()
  "Section Node selection: the C-hjkl keys are bound when
`hel-want-C-hjkl-keys' is non-nil."
  (skip-unless hel-want-C-hjkl-keys)
  (pcase-dolist (`(,key . ,command)
                 '(("C-k" . hel-expand-selection)
                   ("C-j" . hel-shrink-selection)
                   ("C-l" . hel-select-next-sibling)
                   ("C-h" . hel-select-previous-sibling)))
    (should (eq command (keymap-lookup hel-normal-state-map key)))))

(ert-deftest hel-spec-key-bindings-parens ()
  "Section Text objects and motions: `)' and `(' run the function motions
in `prog-mode-map' and the sentence motions in `text-mode-map'."
  (let ((prog (hel-get-nested-hel-keymap prog-mode-map 'normal))
        (text (hel-get-nested-hel-keymap text-mode-map 'normal)))
    (should (eq 'hel-mark-function-forward (keymap-lookup prog ")")))
    (should (eq 'hel-mark-function-backward (keymap-lookup prog "(")))
    (should (eq 'hel-mark-sentence-forward (keymap-lookup text ")")))
    (should (eq 'hel-mark-sentence-backward (keymap-lookup text "(")))))

;;; Things and sources

(ert-deftest hel-spec-thing-sources ()
  "Section Things and sources: function, comment and node have more than
one source; node has three.  Section Choosing a producer: class and
argument have only a query.  An extension may add a source of WORD, as
hel-paredit does, so its count is not checked."
  (should (= 2 (length (get 'function 'hel-thing-sources))))
  (should (= 2 (length (get 'comment 'hel-thing-sources))))
  (should (= 3 (length (get 'node 'hel-thing-sources))))
  (dolist (thing '(word sentence paragraph class argument))
    (should (= 1 (length (get thing 'hel-thing-sources))))))

(ert-deftest hel-spec-class-has-no-source-in-text-mode ()
  "Section Choosing a producer: when no source applies, the class command
leaves the selection as it is."
  (hel-spec--with #'text-mode "foo |bar\n"
    (should-not (hel-spec--source-producer 'class))
    (hel-spec--run #'hel-mark-inner-class)
    (should (equal (hel-spec--render) "foo |bar\n"))
    (should (null mark-ring))))

(ert-deftest hel-spec-treesit-text-objects-p ()
  "Section Choosing a producer: `hel-treesit-text-objects-p' is non-nil
in a buffer with a parser and a query file on Emacs 31, and nil in
text-mode."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "x = |1\n"
    (should (hel-treesit-text-objects-p)))
  (hel-spec--with #'text-mode "x = |1\n"
    (should-not (hel-treesit-text-objects-p))))

(ert-deftest hel-spec-thing-rules ()
  "Section Two questions, two arguments: header-p and linewise-p are
rules of the thing (table of section The ranges of an object)."
  (should (hel-thing-header-p 'function))
  (should (hel-thing-header-p 'class))
  (should (hel-thing-header-p 'argument))
  (should-not (hel-thing-header-p 'word))
  (should-not (hel-thing-header-p 'sentence))
  (should-not (hel-thing-header-p 'comment))
  (should (hel-thing-linewise-p 'paragraph))
  (should-not (hel-thing-linewise-p 'word)))

;;; Laws of the protocol

(defun hel-spec--skip-gap-idempotent (thing pos)
  "Check that a second skip-gap of THING from its result returns it."
  (let ((producer (hel-spec--source-producer thing)))
    (should producer)
    (dolist (dir '(1 -1))
      (let ((once (hel-thing-skip-gap thing producer pos dir)))
        (should (equal once (hel-thing-skip-gap thing producer once dir)))))))

(ert-deftest hel-spec-law-skip-gap-sentence ()
  "Tests of the protocol: a second skip-gap from its own result returns
that result (the sentence motion)."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode "One two.   Three four.\n"
      (hel-spec--skip-gap-idempotent 'sentence 9))))

(ert-deftest hel-spec-law-skip-gap-python-query ()
  "Tests of the protocol: skip-gap idempotence on the Python query."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a,   b)\n"
    (goto-char 4)
    (hel-spec--skip-gap-idempotent 'argument 4)))

(ert-deftest hel-spec-law-skip-gap-comment-runs ()
  "Tests of the protocol: skip-gap idempotence on the comment runs of the
syntax table."
  (hel-spec--with #'emacs-lisp-mode ";; a\n;; b\n\n\n;; c\n(foo)\n"
    (hel-spec--skip-gap-idempotent 'comment 12)))

(ert-deftest hel-spec-law-comment-gap-on-line ()
  "Tests of the protocol: away from the start of a line, the gap of a
comment holds no line end."
  (hel-spec--with #'emacs-lisp-mode "(foo)  ; note\n(bar)\n"
    (let* ((producer (hel-spec--source-producer 'comment))
           (end 14)                     ; after "; note"
           (start 8))                   ; before "; note"
      (should producer)
      (let ((after (hel-thing-skip-gap 'comment producer end 1))
            (before (hel-thing-skip-gap 'comment producer start -1)))
        (should-not (string-search "\n" (buffer-substring end after)))
        (should-not (string-search "\n" (buffer-substring before start)))))))

;;; Python text objects

(defconst hel-spec--class
  (concat "class A:\n"
          "    def m1(self, a, b):\n"
          "        return a\n"
          "\n"
          "    def m2(self):\n"
          "        return 2\n"
          "\n"
          "\n"
          "def top():\n"
          "    pass\n")
  "A Python class with two methods and a top-level function.")

(defun hel-spec--class-at (needle marker &optional needle2 marker2)
  "Return `hel-spec--class' with MARKER before NEEDLE.
With NEEDLE2, also put MARKER2 before it.  NEEDLE2 must come after NEEDLE."
  (let* ((s hel-spec--class)
         (i (string-search needle s))
         (j (and needle2 (string-search needle2 s))))
    (if j
        (concat (substring s 0 i) marker (substring s i j) marker2
                (substring s j))
      (concat (substring s 0 i) marker (substring s i)))))

(defun hel-spec--class-pos (needle)
  "Return the buffer position of NEEDLE in `hel-spec--class'."
  (1+ (string-search needle hel-spec--class)))

(ert-deftest hel-spec-mif-method ()
  "Tests: mif selects the whole method with its def line, point at the
start (sections One rule for every object, rule 7)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "|" "\n\n    def m2" "^")))))

(ert-deftest hel-spec-maf-method ()
  "Tests: maf selects the method and the blank lines after it, with point
at the start.  The gap runs up to the next object."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "|" "def m2" "^")))))

(ert-deftest hel-spec-mif-def-line ()
  "Tests: mif on the def line selects that function."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "f m1" "|")
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "|" "\n\n    def m2" "^")))))

(ert-deftest hel-spec-2mif-two-methods ()
  "Tests: `2 mif' on the first method selects both methods."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-inner-function 2)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "|" "\n\n\ndef top" "^")))))

(ert-deftest hel-spec-2maf-last-method ()
  "Tests: `2 maf' on the last method selects only that method: the
objects stay inside the parent of the first one."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "2\n" "|")
    (hel-spec--run #'hel-mark-a-function 2)
    (should (use-region-p))
    (should (string-search "def m2(self):\n        return 2" (hel-spec--sel)))
    (should (<= (hel-spec--class-pos "\n\n    def m2") (region-beginning)))
    (should (<= (region-end) (hel-spec--class-pos "def top")))))

(ert-deftest hel-spec-maf-repeated ()
  "Tests: a second maf on the first method selects both methods, and a
third one adds nothing, because the class holds no more methods."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-a-function)
    (hel-spec--run #'hel-mark-a-function)
    (should (= (region-beginning) (hel-spec--class-pos "def m1")))
    (should (string-search "return 2" (hel-spec--sel)))
    (should (<= (region-end) (hel-spec--class-pos "def top")))
    (let ((second (hel-spec--render)))
      (hel-spec--run #'hel-mark-a-function)
      (should (equal (hel-spec--render) second)))))

(ert-deftest hel-spec-maf-repeated-then-shrink ()
  "Tests: M-i after the second maf returns to the first method."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-a-function)
    (let ((first (hel-spec--render)))
      (hel-spec--run #'hel-mark-a-function)
      (hel-spec--run #'hel-shrink-selection)
      (should (equal (hel-spec--render) first)))))

(ert-deftest hel-spec-maf-then-shrink ()
  "Tests: M-i after maf returns to the selection before it (rule 3)."
  (hel-spec--need 'python)
  (let ((text (hel-spec--class-at "a\n\n" "|")))
    (hel-spec--with #'python-ts-mode text
      (hel-spec--run #'hel-mark-a-function)
      (hel-spec--run #'hel-shrink-selection)
      (should-not (use-region-p))
      (should (equal (hel-spec--render) text)))))

(ert-deftest hel-spec-maf-pushes-point ()
  "Section Selecting an object: the driver pushes the old point to the
mark ring."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "a\n\n" "|")
    (hel-spec--run #'hel-mark-a-function)
    (should (= 1 (length mark-ring)))))

(defconst hel-spec--headers
  (concat "x = 0\n\n\n"
          "# Header of f.\n"
          "def f():\n"
          "    return 1\n"
          "\n\n"
          "# Header of g.\n"
          "def g():\n"
          "    return 2\n")
  "Two Python functions with header comments.")

(defun hel-spec--headers-at (&rest pairs)
  "Return `hel-spec--headers' with markers inserted.
PAIRS is a list of (NEEDLE MARKER OFFSET) in buffer order.  OFFSET is
added to the index of NEEDLE."
  (let ((s hel-spec--headers)
        (shift 0))
    (pcase-dolist (`(,needle ,marker ,offset) pairs)
      (let ((i (+ (string-search needle hel-spec--headers) (or offset 0) shift)))
        (setq s (concat (substring s 0 i) marker (substring s i)))
        (cl-incf shift (length marker))))
    s))

(ert-deftest hel-spec-maf-header-comment ()
  "Tests: maf on a function with a comment directly above it takes that
header comment, and leaves out the header comment of the next function."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--headers-at '("1\n" "|" 0))
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   (hel-spec--headers-at '("# Header of f" "|" 0)
                                         '("# Header of g" "^" 0))))))

(ert-deftest hel-spec-mif-on-header-comment ()
  "Tests: mif on a header comment selects the function under it."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--headers-at '("Header of f" "|" 3))
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   (hel-spec--headers-at '("def f" "|" 0)
                                         '("1\n" "^" 1))))))

(ert-deftest hel-spec-mic-on-header-comment ()
  "Tests: mic on the line of a header comment selects the comment line
with its line end."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--headers-at '("Header of f" "|" 3))
    (hel-spec--run #'hel-mark-inner-comment)
    (should (equal (hel-spec--render)
                   (hel-spec--headers-at '("# Header of f" "|" 0)
                                         '("def f" "^" 0))))))

(ert-deftest hel-spec-mif-on-blank-line ()
  "Tests: mif on a blank line between two functions selects the function
above it."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--headers-at '("1\n\n" "|" 3))
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   (hel-spec--headers-at '("def f" "|" 0)
                                         '("1\n" "^" 1))))))

(ert-deftest hel-spec-maf-decorated-function ()
  "Tests: maf on a decorated function selects its header comment, its
decorators and the function (section The object; Hel's own patterns)."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode
      "# Header of f.\n@deco\ndef f():\n    pa|ss\n\n\ndef g():\n    pass\n"
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   "|# Header of f.\n@deco\ndef f():\n    pass\n\n\n^def g():\n    pass\n"))))

(ert-deftest hel-spec-mat-decorated-class ()
  "Tests: mat on a decorated class selects its header comment, its
decorators and the class."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode
      "# Header of K.\n@deco\nclass K:\n    pa|ss\n\n\nclass L:\n    pass\n"
    (hel-spec--run #'hel-mark-a-class)
    (should (equal (hel-spec--render)
                   "|# Header of K.\n@deco\nclass K:\n    pass\n\n\n^class L:\n    pass\n"))))

(ert-deftest hel-spec-nested-function ()
  "Tests: a nested function that ends where its outer function ends stays
a separate object."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode
      "def outer():\n    def inner():\n        return |1\n"
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   "def outer():\n    |def inner():\n        return 1^\n"))))

(ert-deftest hel-spec-nested-function-decorated-outer ()
  "Tests: the nested function stays separate also when the outer function
is decorated (the replacement compares nodes, not positions)."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode
      "@deco\ndef outer():\n    def inner():\n        return |1\n"
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   "@deco\ndef outer():\n    |def inner():\n        return 1^\n"))))

;;; Arguments

(ert-deftest hel-spec-mia-on-comma ()
  "Tests: on the comma in f(a, b), mia selects a (a cursor in a gap finds
the object that the gap belongs to)."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a|, b)\n"
    (hel-spec--run #'hel-mark-inner-argument)
    (should (equal (hel-spec--render) "f(|a^, b)\n"))))

(ert-deftest hel-spec-mia-on-comma-after-b ()
  "Tests: on the comma after b in f(a, b, c), mia selects b (when two
around ranges contain the cursor, the object before it wins)."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a, b|, c)\n"
    (hel-spec--run #'hel-mark-inner-argument)
    (should (equal (hel-spec--render) "f(a, |b^, c)\n"))))

(ert-deftest hel-spec-mia-nothing-here ()
  "Tests: mia with no argument around the cursor leaves the selection and
the mark ring as they are."
  (hel-spec--need-queries 'python)
  (should (hel-spec--unchanged #'hel-mark-inner-argument "|x = 1\n"
                               #'python-ts-mode)))

(ert-deftest hel-spec-maa-on-self-pushes-once ()
  "Tests: maa on self pushes point once."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "def m(se|lf, a):\n    pass\n"
    (hel-spec--run #'hel-mark-a-argument)
    (should (equal (hel-spec--render) "def m(|self, ^a):\n    pass\n"))
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-spec-maa-first ()
  "Tests and section The around range: maa on a selects a and the gap
`, ' after it."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(|a, b, c)\n"
    (hel-spec--run #'hel-mark-a-argument)
    (should (equal (hel-spec--render) "f(|a, ^b, c)\n"))))

(ert-deftest hel-spec-maa-last ()
  "Tests: maa on the last argument selects the gap before it and the
argument."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a, b, |c)\n"
    (hel-spec--run #'hel-mark-a-argument)
    (should (equal (hel-spec--render) "f(a, b|, c^)\n"))))

(ert-deftest hel-spec-mia-each-argument ()
  "Tests: mia on any argument selects the argument alone."
  (hel-spec--need-queries 'python)
  (dolist (case '(("f(|a, b, c)\n" . "f(|a^, b, c)\n")
                  ("f(a, |b, c)\n" . "f(a, |b^, c)\n")
                  ("f(a, b, |c)\n" . "f(a, b, |c^)\n")))
    (hel-spec--with #'python-ts-mode (car case)
      (hel-spec--run #'hel-mark-inner-argument)
      (should (equal (hel-spec--render) (cdr case))))))

(ert-deftest hel-spec-maa-delete-first-and-last ()
  "Tests: maa and d on the first and on the last argument leave a list
without a stray comma."
  (hel-spec--need-queries 'python)
  (dolist (case '(("f(|a, b, c)\n" . "f(b, c)\n")
                  ("f(a, b, |c)\n" . "f(a, b)\n")))
    (hel-spec--with #'python-ts-mode (car case)
      (hel-spec--run #'hel-mark-a-argument)
      (delete-region (region-beginning) (region-end))
      (should (equal (buffer-string) (cdr case))))))

(ert-deftest hel-spec-2mia ()
  "Tests: on a in f(a, b, c), `2 mia' selects `a, b'."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(|a, b, c)\n"
    (hel-spec--run #'hel-mark-inner-argument 2)
    (should (equal (hel-spec--render) "f(|a, b^, c)\n"))))

(ert-deftest hel-spec-2maa ()
  "Tests: on a in f(a, b, c), `2 maa' selects `a, b' and the gap after b."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(|a, b, c)\n"
    (hel-spec--run #'hel-mark-a-argument 2)
    (should (equal (hel-spec--render) "f(|a, b, ^c)\n"))))

(ert-deftest hel-spec-negative-2mia ()
  "Tests: on b in f(a, b, c), `-2 mia' selects `a, b'."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a, |b, c)\n"
    (hel-spec--run #'hel-mark-inner-argument -2)
    (should (equal (hel-spec--render) "f(|a, b^, c)\n"))))

(ert-deftest hel-spec-count-more-than-exist ()
  "Section Selecting an object: when fewer objects exist, the driver
selects the ones that exist."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "f(a, |b, c)\n"
    (hel-spec--run #'hel-mark-inner-argument 5)
    (should (equal (hel-spec--render) "f(a, |b, c^)\n"))))

;;; Comments

(ert-deftest hel-spec-mic-run ()
  "Tests: mic selects both comment lines with their # markers and the
line end."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = 1\n\n\n# one\n# t|wo\n\n# three\n"
    (hel-spec--run #'hel-mark-inner-comment)
    (should (equal (hel-spec--render) "x = 1\n\n\n|# one\n# two\n^\n# three\n"))))

(ert-deftest hel-spec-mac-run ()
  "Tests: mac adds the blank lines up to the next comment run."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = 1\n\n\n# one\n# t|wo\n\n# three\n"
    (hel-spec--run #'hel-mark-a-comment)
    (should (equal (hel-spec--render) "x = 1\n\n\n|# one\n# two\n\n^# three\n"))))

(ert-deftest hel-spec-mic-after-code ()
  "Section The comment object: a comment after code covers only the
comment."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = 1  # no|te\ny = 2\n"
    (hel-spec--run #'hel-mark-inner-comment)
    (should (equal (hel-spec--render) "x = 1  |# note^\ny = 2\n"))))

(ert-deftest hel-spec-mac-after-code ()
  "Tests: mac on a comment after code selects only the comment, and its
gap stays on the line: here the spaces before it."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "x = 1  # no|te\ny = 2\n"
    (hel-spec--run #'hel-mark-a-comment)
    (should (equal (hel-spec--render) "x = 1|  # note^\ny = 2\n"))))

;;; Other languages and embedded languages

(defun hel-spec--maf-check (mode text expected)
  "Check that maf in MODE turns TEXT into EXPECTED."
  (hel-spec--with mode text
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render) expected))))

(ert-deftest hel-spec-maf-typescript ()
  "Tests: maf works in TypeScript through the inherited ecma file."
  (hel-spec--need-queries 'typescript)
  (hel-spec--maf-check
   #'typescript-ts-mode
   "function a() {\n  return |1;\n}\n\nfunction b() {\n  return 2;\n}\n"
   "|function a() {\n  return 1;\n}\n\n^function b() {\n  return 2;\n}\n"))

(ert-deftest hel-spec-maf-javascript ()
  "Tests: maf works in JavaScript."
  (hel-spec--need-queries 'javascript)
  (hel-spec--maf-check
   #'js-ts-mode
   "function a() {\n  return |1;\n}\n\nfunction b() {\n  return 2;\n}\n"
   "|function a() {\n  return 1;\n}\n\n^function b() {\n  return 2;\n}\n"))

(ert-deftest hel-spec-maf-rust ()
  "Tests: maf works in Rust."
  (hel-spec--need-queries 'rust)
  (hel-spec--maf-check
   #'rust-ts-mode
   "fn a() {\n    |1\n}\n\nfn b() {\n    2\n}\n"
   "|fn a() {\n    1\n}\n\n^fn b() {\n    2\n}\n"))

(ert-deftest hel-spec-maf-c ()
  "Tests: maf works in C."
  (hel-spec--need-queries 'c)
  (hel-spec--maf-check
   #'c-ts-mode
   "int a(void) {\n  return |1;\n}\n\nint b(void) {\n  return 2;\n}\n"
   "|int a(void) {\n  return 1;\n}\n\n^int b(void) {\n  return 2;\n}\n"))

(ert-deftest hel-spec-maf-go ()
  "Tests: maf works in Go."
  (hel-spec--need-queries 'go)
  (hel-spec--maf-check
   #'go-ts-mode
   "package main\n\nfunc a() int {\n\treturn |1\n}\n\nfunc b() int {\n\treturn 2\n}\n"
   "package main\n\n|func a() int {\n\treturn 1\n}\n\n^func b() int {\n\treturn 2\n}\n"))

(ert-deftest hel-spec-html-script ()
  "Tests: in an HTML buffer, a cursor in a <script> block uses the
JavaScript query, and a cursor in the markup uses the HTML query (rule 5,
section Choosing a producer)."
  (hel-spec--need-queries 'html 'javascript 'css)
  (skip-unless (fboundp 'mhtml-ts-mode))
  (let ((text (concat "<!-- note -->\n<p>text</p>\n<script>\n"
                      "function foo() { return 1; }\n</script>\n")))
    (hel-spec--with #'mhtml-ts-mode
        (replace-regexp-in-string "return" "re|turn" text)
      (treesit-update-ranges)
      (hel-spec--run #'hel-mark-inner-function)
      (should (equal (hel-spec--sel) "function foo() { return 1; }")))
    (hel-spec--with #'mhtml-ts-mode
        (replace-regexp-in-string "note" "no|te" text)
      (treesit-update-ranges)
      (hel-spec--run #'hel-mark-inner-comment)
      (should (string-prefix-p "<!-- note -->" (hel-spec--sel))))))

;;; The ] and [ motions

(ert-deftest hel-spec-forward-function-chain ()
  "Tests: ]f inside a method selects from point to the end of the method,
the next ]f selects the second method whole, and the next one the
top-level function (section Moving to the edge of an object)."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "urn a" "|")
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "urn a" "^" "\n\n    def m2" "|")))
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m2" "^" "\n\n\ndef top" "|")))
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   (concat "class A:\n"
                           "    def m1(self, a, b):\n"
                           "        return a\n\n"
                           "    def m2(self):\n"
                           "        return 2\n\n\n"
                           "^def top():\n"
                           "    pass|\n")))))

(ert-deftest hel-spec-backward-function-chain ()
  "Tests: [f inside a method selects from point to its start, and the next
[f selects the method before it whole."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at " 2\n" "|")
    (hel-spec--run #'hel-mark-function-backward)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m2" "|" " 2\n" "^")))
    (hel-spec--run #'hel-mark-function-backward)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "|" "\n\n    def m2" "^")))))

(ert-deftest hel-spec-forward-function-on-class-line ()
  "Tests: ]f on the class line selects the first method."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (concat "cl|" (substring hel-spec--class 2))
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   (hel-spec--class-at "def m1" "^" "\n\n    def m2" "|")))))

(ert-deftest hel-spec-forward-function-count ()
  "Tests: `3]f' inside the first method reaches the end of the top-level
function, and one more ]f keeps the selection."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode (hel-spec--class-at "urn a" "|")
    (hel-spec--run #'hel-mark-function-forward 3)
    (let ((expected (concat "class A:\n"
                            "    def m1(self, a, b):\n"
                            "        ret^urn a\n\n"
                            "    def m2(self):\n"
                            "        return 2\n\n\n"
                            "def top():\n"
                            "    pass|\n")))
      (should (equal (hel-spec--render) expected))
      (hel-spec--run #'hel-mark-function-forward)
      (should (equal (hel-spec--render) expected)))))

(ert-deftest hel-spec-forward-function-decorated ()
  "Tests: ]f at a decorated function selects it with its decorators."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "x = 1|\n\n\n@deco\ndef f():\n    pass\n"
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   "x = 1\n\n\n^@deco\ndef f():\n    pass|\n"))))

(ert-deftest hel-spec-forward-argument-chain ()
  "Tests: ]a from self selects to the end of self, and the next two ]a
select a and b without their commas."
  (hel-spec--need-queries 'python)
  (hel-spec--with #'python-ts-mode "def m(|self, a, b):\n    pass\n"
    (hel-spec--run #'hel-mark-argument-forward)
    (should (equal (hel-spec--render) "def m(^self|, a, b):\n    pass\n"))
    (hel-spec--run #'hel-mark-argument-forward)
    (should (equal (hel-spec--render) "def m(self, ^a|, b):\n    pass\n"))
    (hel-spec--run #'hel-mark-argument-forward)
    (should (equal (hel-spec--render) "def m(self, a, ^b|):\n    pass\n"))))

(ert-deftest hel-spec-forward-comment-chain ()
  "Tests: in `x = 1  # a' followed by `# b' and `# c', ]c from x selects
`# a', and the next ]c selects the run of `# b' and `# c'."
  (hel-spec--need 'python)
  (hel-spec--with #'python-ts-mode "|x = 1  # a\n# b\n# c\n"
    (hel-spec--run #'hel-mark-comment-forward)
    (should (equal (hel-spec--render) "x = 1  ^# a|\n# b\n# c\n"))
    (hel-spec--run #'hel-mark-comment-forward)
    (should (equal (hel-spec--render) "x = 1  # a\n^# b\n# c\n|"))))

(ert-deftest hel-spec-forward-sentence ()
  "Tests: ]s inside a sentence selects to its end."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode
        "One two. Th|ree four. Five six.\n\nSeven eight. Nine.\n"
      (hel-spec--run #'hel-mark-sentence-forward)
      (should (equal (hel-spec--render)
                     "One two. Th^ree four.| Five six.\n\nSeven eight. Nine.\n")))))

(ert-deftest hel-spec-forward-sentence-count-passes-paragraph ()
  "Tests: ]s with a count passes the end of the paragraph (the motion
ignores parents)."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode
        "One two. Th|ree four. Five six.\n\nSeven eight. Nine.\n"
      (hel-spec--run #'hel-mark-sentence-forward 3)
      (should (equal (hel-spec--render)
                     "One two. Th^ree four. Five six.\n\nSeven eight.| Nine.\n")))))

(ert-deftest hel-spec-forward-paragraph-edge ()
  "Tests: ]p selects to the end of the paragraph with its last line end."
  (hel-spec--with #'text-mode "a|aa\nbbb\n\nccc\n"
    (hel-spec--run #'hel-mark-paragraph-forward)
    (should (equal (hel-spec--render) "a^aa\nbbb\n|\nccc\n"))))

(ert-deftest hel-spec-backward-paragraph-edge ()
  "Section Moving to the edge of an object: [p selects from point to the
start of the paragraph."
  (hel-spec--with #'text-mode "x\n\naaa\nb|bb\n"
    (hel-spec--run #'hel-mark-paragraph-backward)
    (should (equal (hel-spec--render) "x\n\n|aaa\nb^bb\n"))))

(ert-deftest hel-spec-forward-paragraph-brace ()
  "Tests: } selects from point to the start of the next paragraph, with
the blank lines before it."
  (hel-spec--with #'text-mode "a|aa\nbbb\n\nccc\n"
    (hel-spec--run #'hel-forward-paragraph)
    (should (equal (hel-spec--render) "a^aa\nbbb\n\n|ccc\n"))))

(ert-deftest hel-spec-backward-paragraph-brace ()
  "Section Moving to the edge of an object: { selects from point to the
start of the paragraph under the cursor, and when point is already there,
to the start of the previous paragraph."
  (hel-spec--with #'text-mode "aaa\n\nccc\nd|dd\n"
    (hel-spec--run #'hel-backward-paragraph)
    (should (equal (hel-spec--render) "aaa\n\n|ccc\nd^dd\n"))
    (hel-spec--run #'hel-backward-paragraph)
    (should (equal (hel-spec--render) "|aaa\n\n^ccc\nddd\n"))))

;;; Prose objects

(ert-deftest hel-spec-maw-spaces-after ()
  "Tests: maw selects a word with the spaces after it."
  (hel-spec--with #'text-mode "foo |bar  baz\n"
    (hel-spec--run #'hel-mark-a-word)
    (should (equal (hel-spec--render) "foo |bar  ^baz\n"))))

(ert-deftest hel-spec-maw-last-word-of-line ()
  "Tests: maw on the last word of a line takes the spaces before it, not
the line end."
  (hel-spec--with #'text-mode "foo  b|ar\nbaz\n"
    (hel-spec--run #'hel-mark-a-word)
    (should (equal (hel-spec--render) "foo|  bar^\nbaz\n"))))

(ert-deftest hel-spec-maw-before-period ()
  "Tests: maw on a word before a period takes the spaces before it."
  (hel-spec--with #'text-mode "foo b|ar.\n"
    (hel-spec--run #'hel-mark-a-word)
    (should (equal (hel-spec--render) "foo| bar^.\n"))))

(ert-deftest hel-spec-miw-in-gap ()
  "Section Selecting an object: on the space between two words, miw
selects the word before it."
  (hel-spec--with #'text-mode "foo | bar\n"
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "|foo^  bar\n"))))

(ert-deftest hel-spec-miw-bare-point-touches-two ()
  "Section Selecting an object: when a bare point touches two objects, the
object after point wins.  In `foo|.' miw selects the period."
  (hel-spec--with #'text-mode "foo|.\n"
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "foo|.^\n"))))

(ert-deftest hel-spec-miw-bare-point-before-space ()
  "Section Selecting an object: in `foo| bar' miw selects foo, because
nothing else touches point."
  (hel-spec--with #'text-mode "foo| bar\n"
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "|foo^ bar\n"))))

(ert-deftest hel-spec-miw-repeated ()
  "Section Selecting an object: a repeated key adds the next objects."
  (hel-spec--with #'text-mode "|foo bar baz\n"
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "|foo^ bar baz\n"))
    (hel-spec--run #'hel-mark-inner-word)
    (should (equal (hel-spec--render) "|foo bar^ baz\n"))))

(ert-deftest hel-spec-2miw ()
  "Rule 8: in mi, a count is the number of objects."
  (hel-spec--with #'text-mode "|foo bar baz\n"
    (hel-spec--run #'hel-mark-inner-word 2)
    (should (equal (hel-spec--render) "|foo bar^ baz\n"))))

(ert-deftest hel-spec-mas-whitespace-after ()
  "Tests: mas selects a sentence with the whitespace after it."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode "One. Tw|o three. Four.\n"
      (hel-spec--run #'hel-mark-a-sentence)
      (should (equal (hel-spec--render) "One. |Two three. ^Four.\n")))))

(ert-deftest hel-spec-mas-across-line-end ()
  "Tests: the gap of a sentence crosses a line end."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode "One. Tw|o three.\nFour five.\n"
      (hel-spec--run #'hel-mark-a-sentence)
      (should (equal (hel-spec--render) "One. |Two three.\n^Four five.\n")))))

(ert-deftest hel-spec-mas-last-sentence ()
  "Tests: mas on the last sentence of a paragraph takes the whitespace
before it, so it never joins two paragraphs."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode "One. Tw|o.\n\nThree.\n"
      (hel-spec--run #'hel-mark-a-sentence)
      (should (equal (hel-spec--render) "One.| Two.^\n\nThree.\n")))))

(ert-deftest hel-spec-3mis-last-sentence ()
  "Tests: `3 mis' on the last sentence of a paragraph selects only that
sentence (the paragraph is the parent of its sentences)."
  (let ((sentence-end-double-space nil))
    (hel-spec--with #'text-mode "One. Tw|o.\n\nThree. Four.\n"
      (hel-spec--run #'hel-mark-inner-sentence 3)
      (should (equal (hel-spec--render) "One. |Two.^\n\nThree. Four.\n")))))

(ert-deftest hel-spec-mip ()
  "Tests: mip selects whole lines with the indentation of the first line
and the last line end."
  (hel-spec--with #'text-mode "x\n\n  first line\nsec|ond\n\nnext\n"
    (hel-spec--run #'hel-mark-inner-paragraph)
    (should (equal (hel-spec--render) "x\n\n|  first line\nsecond\n^\nnext\n"))))

(ert-deftest hel-spec-map ()
  "Tests: map adds the blank lines after the paragraph."
  (hel-spec--with #'text-mode "x\n\n  first line\nsec|ond\n\nnext\n"
    (hel-spec--run #'hel-mark-a-paragraph)
    (should (equal (hel-spec--render) "x\n\n|  first line\nsecond\n\n^next\n"))))

(ert-deftest hel-spec-map-last-paragraph ()
  "Tests: map on the last paragraph adds the blank lines before it."
  (hel-spec--with #'text-mode "aaa\n\n\nbb|b\n"
    (hel-spec--run #'hel-mark-a-paragraph)
    (should (equal (hel-spec--render) "aaa\n|\n\nbbb\n^"))))

(ert-deftest hel-spec-maw-elisp-comment-above ()
  "Tests: maw in an Emacs Lisp buffer leaves out a comment line above the
word."
  (hel-spec--with #'emacs-lisp-mode ";; comment\n|foo\n"
    (hel-spec--run #'hel-mark-a-word)
    (should (equal (hel-spec--render) ";; comment\n|foo^\n"))))

;;; Emacs Lisp: functions and comments without a parser

(defconst hel-spec--elisp
  (concat ";; Header of a.\n(defun a ()\n  1)\n\n"
          ";; Header of b.\n(defun b ()\n  2)\n")
  "Two Emacs Lisp functions with header comments.")

(ert-deftest hel-spec-elisp-mif ()
  "Tests: in an Emacs Lisp buffer, mif selects the defun without its
header comment."
  (hel-spec--with #'emacs-lisp-mode
      (replace-regexp-in-string "  1)" "  |1)" hel-spec--elisp)
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   (concat ";; Header of a.\n|(defun a ()\n  1)^\n\n"
                           ";; Header of b.\n(defun b ()\n  2)\n")))))

(ert-deftest hel-spec-elisp-maf ()
  "Tests: maf selects the header comment, the defun and the blank lines
after it, and leaves out the header comment of the next defun."
  (hel-spec--with #'emacs-lisp-mode
      (replace-regexp-in-string "  1)" "  |1)" hel-spec--elisp)
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   (concat "|;; Header of a.\n(defun a ()\n  1)\n\n"
                           "^;; Header of b.\n(defun b ()\n  2)\n")))))

(ert-deftest hel-spec-elisp-maf-last ()
  "Tests: maf on the last defun of the buffer selects the blank lines
before it, its header comment and the defun."
  (hel-spec--with #'emacs-lisp-mode
      (replace-regexp-in-string "  2)" "  |2)" hel-spec--elisp)
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   (concat ";; Header of a.\n(defun a ()\n  1)|\n\n"
                           ";; Header of b.\n(defun b ()\n  2)^\n")))))

(ert-deftest hel-spec-elisp-2mf ()
  "Tests: in an Emacs Lisp buffer, `2 mf' selects two neighbouring defun
forms and the blank line between them."
  (hel-spec--with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
    (hel-spec--run #'hel-mark-inner-function 2)
    (should (equal (hel-spec--render)
                   "|(defun a ()\n  1)\n\n(defun b ()\n  2)^\n"))))

(ert-deftest hel-spec-elisp-2maf ()
  "Tests: `2 maf' in an Emacs Lisp buffer selects two defun forms and the
gap after the second."
  (hel-spec--with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n\n(defun c ()\n  3)\n"
    (hel-spec--run #'hel-mark-a-function 2)
    (should (equal (hel-spec--render)
                   "|(defun a ()\n  1)\n\n(defun b ()\n  2)\n\n^(defun c ()\n  3)\n"))))

(ert-deftest hel-spec-elisp-maf-backward-repeat ()
  "Section Selecting an object: each `-1 maf' adds the function before
the selection."
  (hel-spec--with #'emacs-lisp-mode
      "(defun a ()\n  1)\n\n(defun b ()\n  |2)\n\n(defun c ()\n  3)\n"
    (hel-spec--run #'hel-mark-a-function)
    (should (equal (hel-spec--render)
                   "(defun a ()\n  1)\n\n|(defun b ()\n  2)\n\n^(defun c ()\n  3)\n"))
    (hel-spec--run #'hel-mark-a-function -1)
    (should (equal (hel-spec--render)
                   "|(defun a ()\n  1)\n\n(defun b ()\n  2)\n\n^(defun c ()\n  3)\n"))))

(ert-deftest hel-spec-elisp-forward-function ()
  "Tests: in a buffer without a parser, ]f selects functions through
`hel--forward-function'."
  (hel-spec--with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   "(defun a ()\n  ^1)|\n\n(defun b ()\n  2)\n"))
    (hel-spec--run #'hel-mark-function-forward)
    (should (equal (hel-spec--render)
                   "(defun a ()\n  1)\n\n^(defun b ()\n  2)|\n"))))

(ert-deftest hel-spec-elisp-mic-run ()
  "Tests: in a buffer without a parser, mic selects a block of `;;' lines
as one run."
  (hel-spec--with #'emacs-lisp-mode ";; one\n;; t|wo\n(foo)\n"
    (hel-spec--run #'hel-mark-inner-comment)
    (should (equal (hel-spec--render) "|;; one\n;; two\n^(foo)\n"))))

(ert-deftest hel-spec-elisp-mia-nothing ()
  "Tests: in a buffer without a parser, mia leaves the selection as it is."
  (should (hel-spec--unchanged #'hel-mark-inner-argument
                               "(foo |a b)\n" #'emacs-lisp-mode)))

(ert-deftest hel-spec-c-mode-mif ()
  "Tests: in a buffer without a parser, mif selects a function through
`hel--forward-function'."
  (hel-spec--with #'c-mode
      "int a(void)\n{\n  return |1;\n}\n\nint b(void)\n{\n  return 2;\n}\n"
    (hel-spec--run #'hel-mark-inner-function)
    (should (equal (hel-spec--render)
                   "|int a(void)\n{\n  return 1;\n}^\n\nint b(void)\n{\n  return 2;\n}\n"))))

;;; Query files

(defmacro hel-spec--with-query-file (content &rest body)
  "Run BODY with a query directory whose Python file holds CONTENT."
  (declare (indent 1) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (make-temp-file "hel-spec-queries" t)))
       (unwind-protect
           (progn
             (make-directory (expand-file-name "python" ,dir))
             (with-temp-file (expand-file-name "python/textobjects.scm" ,dir)
               (insert ,content))
             (let ((hel-spec-queries-directory (file-name-as-directory ,dir))
                   (hel-structural-tests--query-cache
                    (make-hash-table :test #'eq)))
               ,@body))
         (delete-directory ,dir t)))))

(ert-deftest hel-spec-query-broken-pattern ()
  "Tests: a query file with one broken pattern still yields the objects of
its other pattern (section Query files and grammar versions)."
  (hel-spec--need-queries 'python)
  (hel-spec--with-query-file
      (concat "(no_such_node_type) @parameter.around\n\n"
              "(argument_list\n"
              "  ((_) @parameter.inside . \",\"? @parameter.around)"
              " @parameter.around)\n")
    (hel-spec--with #'python-ts-mode "f(|a, b)\n"
      (hel-spec--run #'hel-mark-inner-argument)
      (should (equal (hel-spec--render) "f(|a^, b)\n")))))

(ert-deftest hel-spec-query-unbalanced ()
  "Tests: a query file with unbalanced brackets gives no query and signals
no error, and mif then selects the function through
`hel--forward-function'.  The argument object has no other source."
  (hel-spec--need-queries 'python)
  (hel-spec--with-query-file "(function_definition\n  body: (block\n"
    (hel-spec--with #'python-ts-mode "def f(a, b):\n    return |1\n"
      (hel-spec--run #'hel-mark-inner-function)
      (should (equal (hel-spec--render) "|def f(a, b):\n    return 1^\n")))
    (should (hel-spec--unchanged #'hel-mark-inner-argument
                                 "def f(|a, b):\n    return 1\n"
                                 #'python-ts-mode))))

;;; Word motions

(ert-deftest hel-spec-word-w ()
  "Section Selection rules, rule 10: w selects a word with the space
after it."
  (hel-spec--with #'text-mode "|foo bar\n"
    (hel-spec--run #'hel-forward-word-start)
    (should (equal (hel-spec--render) "^foo |bar\n"))))

(ert-deftest hel-spec-word-w-line-end ()
  "Section The protocol: a word motion stops at a line end."
  (hel-spec--with #'text-mode "|foo\nbar\n"
    (hel-spec--run #'hel-forward-word-start)
    (should (equal (hel-spec--render) "^foo|\nbar\n"))))

(ert-deftest hel-spec-word-e ()
  "Word motions: e selects to the end of the word."
  (hel-spec--with #'text-mode "|foo bar\n"
    (hel-spec--run #'hel-forward-word-end)
    (should (equal (hel-spec--render) "^foo| bar\n"))))

(ert-deftest hel-spec-word-b ()
  "Word motions: b selects back to the start of the word."
  (hel-spec--with #'text-mode "foo bar|\n"
    (hel-spec--run #'hel-backward-word-start)
    (should (equal (hel-spec--render) "foo |bar^\n"))))

(ert-deftest hel-spec-WORD-W ()
  "Word motions: W selects a WORD with the space after it."
  (hel-spec--with #'text-mode "|foo-bar baz\n"
    (hel-spec--run #'hel-forward-WORD-start)
    (should (equal (hel-spec--render) "^foo-bar |baz\n"))))

(ert-deftest hel-spec-WORD-E ()
  "Word motions: E selects to the end of the WORD."
  (hel-spec--with #'text-mode "|foo-bar baz\n"
    (hel-spec--run #'hel-forward-WORD-end)
    (should (equal (hel-spec--render) "^foo-bar| baz\n"))))

(ert-deftest hel-spec-WORD-B ()
  "Word motions: B selects back to the start of the WORD."
  (hel-spec--with #'text-mode "foo-bar baz|\n"
    (hel-spec--run #'hel-backward-WORD-start)
    (should (equal (hel-spec--render) "foo-bar |baz^\n"))))

(ert-deftest hel-spec-ge ()
  "Section Commands are thin: ge (`hel-backward-word-end') selects from
point back to the end of the previous word."
  (hel-spec--with #'text-mode "foo bar b|az\n"
    (hel-spec--run #'hel-backward-word-end)
    (should (equal (hel-spec--render) "foo bar| b^az\n"))))

(ert-deftest hel-spec-ge-count ()
  "Section Commands are thin: `2 ge' goes back to the end of the second
previous word."
  (hel-spec--with #'text-mode "foo bar b|az\n"
    (hel-spec--run #'hel-backward-word-end 2)
    (should (equal (hel-spec--render) "foo| bar b^az\n"))))

;;; Blanks

(defun hel-blank-tests--stops (motion)
  "Return the positions where MOTION stops from the start of the buffer."
  (goto-char (point-min))
  (let (stops)
    (while (and (zerop (funcall motion 1))
                (not (memql (point) stops)))
      (push (point) stops))
    (nreverse stops)))

(ert-deftest hel-WORD-stops-at-every-blank ()
  (dolist (blank (list ?\s ?\t ?\f #xA0 #x3000))
    (with-temp-buffer
      (text-mode)
      (insert "foo" blank "bar")
      (should (equal (hel-blank-tests--stops #'hel--forward-WORD) '(4 8))))))

(ert-deftest hel-word-motions-cross-a-page-break ()
  (dolist (motion '(hel--forward-word hel--forward-WORD))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo\n\f\nbar")
      (should (= (-last-item (hel-blank-tests--stops motion)) (point-max))))))

(ert-deftest hel-word-motion-stays-backward-symmetric ()
  (with-temp-buffer
    (text-mode)
    (insert "foo" #x3000 "bar")
    (goto-char (point-max))
    (hel--forward-WORD -1)
    (should (= (point) 5))
    (hel--forward-WORD -1)
    (should (= (point) 1))))

(ert-deftest hel-trim-takes-every-blank ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\f\n \tab\n\f" (string #xA0))
    (should (equal (hel-trim-whitespace-from-range (cons 1 (point-max)))
                   (cons 5 7)))))

(ert-deftest hel-blank-gap-stays-on-its-line ()
  "In `text-mode' a line end has whitespace syntax, and a gap still stops there."
  (with-temp-buffer
    (text-mode)
    (insert "a \f\nb")
    (should (= (hel--skip-blanks 2 1) 4))
    (should (= (hel--skip-blanks 4 -1) 2))))

;;; Line edges

(ert-deftest hel-line-edges-leave-point-alone ()
  (with-temp-buffer
    (insert "one\ntwo words\nthree")
    (goto-char 7)
    (should (= (hel-line-begin) 5))
    (should (= (hel-line-end) 14))
    (should (= (hel-line-begin 16) 15))
    (should (= (hel-line-end 16) 20))
    (should (= (point) 7))))

(ert-deftest hel-line-begin-skips-invisible-start ()
  (with-temp-buffer
    (insert "one\ntwo words")
    (put-text-property 5 9 'invisible t)
    (should (= (hel-line-begin 12) 9))))

(ert-deftest hel-line-edges-use-visual-lines ()
  (with-temp-buffer
    (insert "one two three")
    (goto-char 7)
    (let ((visual-line-mode t))
      (cl-letf (((symbol-function 'beginning-of-visual-line)
                 (lambda (&rest _) (goto-char 5)))
                ((symbol-function 'end-of-visual-line)
                 (lambda (&rest _) (goto-char 9))))
        (should (= (hel-line-begin) 5))
        (should (= (hel-line-end) 9))
        (should-not (hel-bolp))
        (goto-char 5)
        (should (hel-bolp))))))

(ert-deftest hel-bolp-uses-bolp-on-logical-lines ()
  (with-temp-buffer
    (insert "one\ntwo words")
    (put-text-property 5 9 'invisible t)
    (let ((visual-line-mode nil))
      (goto-char 5)
      (should (hel-bolp))
      (goto-char 9)
      (should-not (hel-bolp)))))

(ert-deftest hel-visual-lines-p-checks-both-edges ()
  (with-temp-buffer
    (insert "one two three four")
    (let ((transient-mark-mode t)
          (visual-line-mode t))
      (cl-letf (((symbol-function 'beginning-of-visual-line)
                 (lambda (&rest _)
                   (goto-char (if (< (point) 9) 1 9)))))
        (hel-select (cons 1 9))
        (should (hel-visual-lines-p))
        (hel-select (cons 2 9))
        (should-not (hel-visual-lines-p))
        (hel-select (cons 1 12))
        (should-not (hel-visual-lines-p))))))

;;; The range of `hel-thing-edge'

(ert-deftest hel-thing-edge-returns-a-range ()
  "A backward edge gives a range with its begin before its end."
  (with-temp-buffer
    (insert "one two three")
    (let ((word (hel--producer 'word)))
      (should (equal (hel-thing-edge 'word word 9 1 1 :begin nil) (cons 9 14)))
      (should (equal (hel-thing-edge 'word word 9 -1 1 :begin nil) (cons 5 9)))
      (should (equal (hel-thing-edge 'word word 9 -1 1 :end nil) (cons 8 9))))))

(ert-deftest hel-thing-edge-default-method-returns-a-range ()
  (with-temp-buffer
    (insert "One. Two. Three.")
    (let ((sentence-end-double-space nil)
          (sentence (hel--producer 'sentence)))
      (should (equal (hel-thing-edge 'sentence sentence 7 1 1 :end nil)
                     (cons 7 10)))
      (should (equal (hel-thing-edge 'sentence sentence 7 -1 1 :begin nil)
                     (cons 6 7))))))

;;; Setting the selection

(defun hel-select-tests--state ()
  "Return the mark and point, and whether the region is active."
  (list (mark t) (point) (and (region-active-p) t)))

(defmacro hel-select-tests--with-buffer (&rest body)
  "Run BODY in a buffer of 20 characters with point at 10."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((transient-mark-mode t)
           (hel--extend-selection nil))
       (insert (make-string 20 ?x))
       (goto-char 10)
       ,@body)))

(ert-deftest hel-select-direction ()
  (hel-select-tests--with-buffer
    (hel-select (cons 3 7))
    (should (equal (hel-select-tests--state) '(3 7 t)))
    (hel-select (cons 3 7) 1)
    (should (equal (hel-select-tests--state) '(3 7 t)))
    (hel-select (cons 3 7) -1)
    (should (equal (hel-select-tests--state) '(7 3 t)))))

(ert-deftest hel-select-ignores-extending ()
  (hel-select-tests--with-buffer
    (set-mark 15)
    (let ((hel--extend-selection t))
      (hel-select (cons 3 7))
      (should (equal (hel-select-tests--state) '(3 7 t))))))

(ert-deftest hel-select-motion-new-selection ()
  (hel-select-tests--with-buffer
    (hel-select-motion 10 15)
    (should (equal (hel-select-tests--state) '(10 15 t)))
    (hel-select-motion 15 10)
    (should (equal (hel-select-tests--state) '(15 10 t)))))

(ert-deftest hel-select-motion-keeps-mark-when-extending ()
  (hel-select-tests--with-buffer
    (hel-select (cons 5 10))
    (let ((hel--extend-selection t))
      (hel-select-motion 10 15)
      (should (equal (hel-select-tests--state) '(5 15 t)))
      (hel-select-motion 15 2)
      (should (equal (hel-select-tests--state) '(5 2 t))))))

(ert-deftest hel-selection-shape ()
  (hel-select-tests--with-buffer
    (should-not (hel-selection))
    (hel-select (cons 3 7) -1)
    (should (equal (hel-selection) (list (cons 3 7) -1)))))

(ert-deftest hel-trim-whitespace-takes-a-range ()
  (with-temp-buffer
    (insert "  ab \n")
    (should (equal (hel-trim-whitespace-from-range (cons 1 7)) (cons 3 5)))
    (should-not (hel-trim-whitespace-from-range (cons 1 3)))))

(ert-deftest hel-invert-case-takes-a-range ()
  (with-temp-buffer
    (insert "abcd")
    (hel-invert-case-in-range (cons 2 4))
    (should (equal (buffer-string) "aBCd"))))

(ert-deftest hel-replace-chars-takes-a-range ()
  (with-temp-buffer
    (insert "ab\ncd")
    (hel-replace-chars (cons 2 5) ?x)
    (should (equal (buffer-string) "ax\nxd"))))

(ert-deftest hel-range-any-order ()
  (should (equal (hel-range 3 7) (hel-range 7 3)))
  (should (= 3 (car (hel-range 7 3))))
  (should (= 7 (cdr (hel-range 7 3))))
  (should (= 3 (car (hel-range 3 3))))
  (should (= 3 (cdr (hel-range 3 3)))))

(ert-deftest hel-selection-round-trip ()
  (hel-select-tests--with-buffer
    (dolist (dir '(1 -1))
      (hel-select (cons 3 7) dir)
      (let ((region (hel-selection))
            (state (hel-select-tests--state)))
        (hel-select (cons 11 13))
        (apply #'hel-select region)
        (should (equal (hel-select-tests--state) state))))))

(provide 'hel-structural-editing-tests)
;;; hel-structural-editing-tests.el ends here
