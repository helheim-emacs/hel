;;; hel-text-object-tests.el --- Tests of the text objects -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The text objects and their producers, the query files, the motions to
;; the edges of objects, and the laws of the thing protocol.
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

;;; Text objects

(defconst hel-to--objects
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

(defun hel-to--first-line ()
  "Return the first line of the selection."
  (string-trim-left (car (split-string (hel-test-selection) "\n"))))

(ert-deftest hel-function-object ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-test-selection)
                   "    def f(self, a, b):\n        return g(a, b)\n"))
    (should (= (point) (region-beginning)))
    (hel-test-goto "return g")
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "    def f(self, a, b):\n        return g(a, b)\n\n"))
    (should (= (point) (region-beginning)))))

(ert-deftest hel-function-object-header-comment ()
  "The around range takes the header comment above the function, and
leaves out the header comment of the next one."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return x")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "# Header of top.\ndef top(x):\n    return x\n\n")))
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "def h")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "    def h(self):\n        pass\n\n")))
  (hel-test-with #'python-ts-mode (hel-test-mark "# Header of f.\ndef f():\n    pass\n\n# Header of g.\ndef g():\n    pass\n" "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "# Header of f.\ndef f():\n    pass\n\n"))))

(defconst hel-to--decorated
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
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--decorated "def f")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "# Header.\n@dec\ndef f():\n    def inner():\n        pass\n\n"))
    (hel-test-goto "class C")
    (hel-mark-around-class 1)
    (should (equal (hel-test-selection) "\n@dec\nclass C:\n    pass\n"))))

(ert-deftest hel-function-object-nested ()
  "A nested function that ends where its outer function ends is a separate
object, also when the outer function is decorated."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--decorated "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-test-selection)
                   "    def inner():\n        pass\n")))
  (hel-test-with #'python-ts-mode (hel-test-mark "def f():\n    def inner():\n        pass\n" "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-test-selection)
                   "    def inner():\n        pass\n"))))

(ert-deftest hel-object-in-gaps ()
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a, b)\n" ", b")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-argument 1)
    (should (equal (hel-test-selection) "a")))
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a, b, c)\n" ", c")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-argument 1)
    (should (equal (hel-test-selection) "b")))
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "\n\n    def h")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-to--first-line) "def f(self, a, b):"))))

(ert-deftest hel-object-on-header-comment ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "Header of top")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-to--first-line) "def top(x):"))
    (hel-test-goto "Header of top")
    ;; No blank line follows the header comment, so `mac' takes the one
    ;; before it.
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) "\n# Header of top.\n"))
    (hel-test-goto "Header of top")
    (hel-mark-comment 1)
    (should (equal (hel-test-selection) "# Header of top.\n"))))

(ert-deftest hel-function-object-on-signature ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "def f")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 1)
    (should (equal (hel-to--first-line) "def f(self, a, b):"))))

(ert-deftest hel-object-push-point ()
  "A text object pushes point only when it selects."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "class A")
    (skip-unless (hel-treesit-text-objects-p))
    (let ((pushes 0))
      (cl-letf (((symbol-function 'hel-push-point)
                 (lambda (&rest _) (cl-incf pushes))))
        (hel-mark-argument 1)
        (should (= pushes 0))
        (should (eq (hel-test-selection) nil))
        (hel-test-goto "self, a")
        (hel-mark-around-argument 1)
        (should (= pushes 1))
        (should (equal (hel-test-selection) "self, "))))))

(ert-deftest hel-class-object ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-class 1)
    (should (equal (hel-to--first-line) "class A:"))))

(ert-deftest hel-argument-object ()
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a, b, c)\n" "a,")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) "a, "))
    (hel-test-goto "c)")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) ", c"))
    (dolist (arg '("a" "b" "c"))
      (hel-test-goto (if (equal arg "c") "c)" (concat arg ",")))
      (hel-mark-argument 1)
      (should (equal (hel-test-selection) arg)))))

(ert-deftest hel-comment-object ()
  "A comment of whole lines covers them as a paragraph does."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "first")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) "# first\n# second\n\n"))
    (hel-test-goto "second")
    (hel-mark-comment 1)
    (should (equal (hel-test-selection) "# first\n# second\n"))))

(ert-deftest hel-comment-object-gap ()
  "`mac' takes the blank lines up to the next comment."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark ";; a\n;; b\n\n;; c\n" "a")
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) ";; a\n;; b\n\n"))
    (hel-test-goto "c")
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) "\n;; c\n"))))

(ert-deftest hel-comment-object-found-from-position ()
  "`mic' and `mac' find a comment from every position that belongs to it."
  (pcase-dolist (`(,mode ,text ,positions ,inner ,around)
                 `(;; The line ends of a run.
                   (emacs-lisp-mode ";; a\n;; b\n\n(c)\n" (5 10)
                                    ";; a\n;; b\n" ";; a\n;; b\n\n")
                   ;; The blank line at the end edge of a run.
                   (emacs-lisp-mode ";; a\n;; b\n\n(c)\n" (11)
                                    ";; a\n;; b\n" ";; a\n;; b\n\n")
                   ;; The blank lines between a comment and code.
                   (emacs-lisp-mode ";; a\n\n\n(c)\n" (6 7)
                                    ";; a\n" ";; a\n\n\n")
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
        (hel-test-with mode (hel-test-mark text "")
          (goto-char position)
          (hel-mark-comment 1)
          (should (equal (hel-test-selection) inner))
          (deactivate-mark)
          (goto-char position)
          (hel-mark-around-comment 1)
          (should (equal (hel-test-selection) around)))))))

(ert-deftest hel-comment-object-not-found ()
  "`mic' selects nothing where no comment or gap of a comment holds point."
  (pcase-dolist (`(,text ,position)
                 '(("(a)\n\n(c)\n" 4)
                   ("(a)\n\n(c)\n" 2)
                   (";; a\n(c)\n" 7)))
    (ert-info ((format "%S at %d" text position))
      (hel-test-with #'emacs-lisp-mode (hel-test-mark text "")
        (goto-char position)
        (hel-mark-comment 1)
        (should-not (use-region-p))))))

(ert-deftest hel-comment-object-line-end-with-parser ()
  "The query producer finds a run from the line ends of its lines too."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "first")
    (skip-unless (hel-treesit-text-objects-p))
    (end-of-line)
    (hel-mark-comment 1)
    (should (equal (hel-test-selection) "# first\n# second\n"))))

(ert-deftest hel-comment-motion-from-line-end ()
  "`]c' at a line end of a run selects to the end of the run."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark ";; a\n;; b\n\n;; c\n" "")
    (goto-char 5)
    (hel-mark-comment-forward 1)
    (should (equal (hel-test-selection) "\n;; b\n"))))

(ert-deftest hel-syntax-comment-under ()
  "One comment holds its delimiters, and not the whitespace after it."
  (pcase-dolist (`(,mode ,text ,cases)
                 '((emacs-lisp-mode "(a) ;; b\n;; c\n"
                                    ((3 nil) (4 nil) (5 (5 . 9)) (6 (5 . 9))
                                     (9 nil) (10 (10 . 14))))
                   (emacs-lisp-mode "(s \"; no\")\n" ((5 nil)))
                   (python-mode "# b  \n"
                                ((1 (1 . 4)) (3 (1 . 4)) (4 nil) (6 nil)))
                   (c-mode "int x; /* a */ y;\n"
                           ((7 nil) (8 (8 . 15)) (9 (8 . 15)) (14 (8 . 15))
                            (15 nil)))
                   (c-mode "/* open" ((1 (1 . 8)) (4 (1 . 8)) (8 nil)))
                   (emacs-lisp-mode ";; a" ((1 (1 . 5)) (2 (1 . 5)) (5 nil)))))
    (hel-test-with mode (hel-test-mark text "")
      (let ((producer (hel-syntax-comment-source)))
        (pcase-dolist (`(,position ,range) cases)
          (ert-info ((format "%S at %d" text position))
            (should (equal (hel--comment-piece-under producer position)
                           range))))))))

(ert-deftest hel-syntax-comment-piece-next-backward-skips-piece-at-pos ()
  "Backward, the next piece ends at or before POS, so the piece that holds
POS is skipped."
  (hel-test-with #'python-mode (hel-test-mark "x = 1  # first\ny = 2  # second\n" "")
    (let ((producer (hel-syntax-comment-source)))
      (should (equal (hel--comment-piece-next producer 26 -1) '(8 . 15)))
      (should (equal (hel--comment-piece-next producer 31 -1) '(23 . 31))))))

(ert-deftest hel-comment-count-selects-one-comment ()
  "`2 mic' and a repeated `mic' select the same comment as `mic'."
  (pcase-dolist (`(,text ,selection)
                 '(("(defun f ()\n  ;; a\n  x)\n;; b\n" "  ;; a\n")
                   (";; a\n(x)\n;; b\n" ";; a\n")
                   (";; a\n\n;; b\n" ";; a\n")))
    (ert-info (text)
      (hel-test-with #'emacs-lisp-mode (hel-test-mark text ";; a")
        (hel-mark-comment 2)
        (should (equal (hel-test-selection) selection)))
      (hel-test-with #'emacs-lisp-mode (hel-test-mark text ";; a")
        (should (equal (hel-test-steps #'hel-mark-comment 2)
                       (list selection selection)))))))

(ert-deftest hel-comment-joins-a-multiline-piece ()
  "A `/* */' piece over two lines and a `//' line below it form one comment.
`mic' finds that comment from each line, by the syntax table and by the
tree-sitter query."
  (let ((text "int a;\n\n/* one\n   two */\n// three\n\nint b;\n"))
    (dolist (mode (list #'c-mode
                        (and (fboundp 'c-ts-mode)
                             (treesit-language-available-p 'c)
                             #'c-ts-mode)))
      (when mode
        (dolist (start '("one" "two" "three"))
          (hel-test-with mode (hel-test-mark text start)
            (ert-info ((format "%s from %s" mode start))
              (hel-mark-comment 1)
              (should (equal (hel-test-selection)
                             "/* one\n   two */\n// three\n")))))))))

(ert-deftest hel-join-line-uncomments ()
  "`J' on a comment line removes the comment marker of the joined line."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark ";; one\n;; two\n" "one")
    (hel-join-line)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   ";; one two\n"))))

(ert-deftest hel-join-line-uncomments-embedded ()
  "In a script of `mhtml-ts-mode', `J' removes the `//' of the joined line."
  (skip-unless (and (fboundp 'mhtml-ts-mode)
                    (treesit-language-available-p 'html)
                    (treesit-language-available-p 'javascript)))
  (hel-test-with #'mhtml-ts-mode (hel-test-mark "<script>\n// one\n// two\n</script>\n" "one")
    (treesit-update-ranges)
    (skip-unless (hel-treesit-text-objects-p))
    (should (eq 'javascript (treesit-language-at (point))))
    (hel-join-line)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "<script>\n// one two\n</script>\n"))))

(ert-deftest hel-comment-motion-to-unterminated ()
  "`]c' selects an unterminated comment."
  (hel-test-with #'c-mode (hel-test-mark "int x;\n/* open" "int")
    (hel-mark-comment-forward 1)
    (should (equal (hel-test-selection) "/* open"))))

(ert-deftest hel-text-object-direction ()
  "Each kind of thing has its own direction of selection."
  (pcase-dolist (`(,dir ,mode ,text ,start ,commands)
                 `((1 text-mode "foo bar baz\n" "bar"
                      (hel-mark-word hel-mark-around-word
                                           hel-mark-WORD hel-mark-around-WORD))
                   (1 text-mode "One two.  Three four.\n" "two"
                      (hel-mark-sentence hel-mark-around-sentence))
                   (-1 text-mode "aaa\nbbb\n\nccc\n" "bbb"
                       (hel-mark-paragraph hel-mark-around-paragraph))
                   (-1 emacs-lisp-mode "(defun a ()\n  1)\n\n(defun b ()\n  2)\n" "1"
                       (hel-mark-function hel-mark-around-function))
                   (-1 emacs-lisp-mode ";; a\n;; b\n\n;; c\n" "b"
                       (hel-mark-comment hel-mark-around-comment))))
    (dolist (command commands)
      (ert-info ((symbol-name command))
        (hel-test-with mode (hel-test-mark text start)
          (funcall command 1)
          (should (use-region-p))
          (should (= dir (hel-region-direction))))))))

(ert-deftest hel-text-object-from-cursor ()
  "`mi' and `ma' find the object at the cursor, not around the selection."
  (let ((sentence-end-double-space nil))
    (pcase-dolist (`(,mode ,text ,mark ,point ,command ,expected)
                   '(;; Forward: the cursor is the last character.
                     (text-mode "one two three four\n" 5 14 hel-mark-word "three")
                     ;; Backward: the cursor is the first character.
                     (text-mode "one two three four\n" 14 5 hel-mark-word "two")
                     ;; The whitespace at the edges does not count.
                     (text-mode "one two three\n" 1 5 hel-mark-word "one")
                     (text-mode "One two. Three four.\n" 1 15 hel-mark-sentence
                                "Three four.")
                     (text-mode "aaa bbb\nccc\n\nddd eee\n\nfff\n" 5 18
                                hel-mark-paragraph "ddd eee\n")
                     (emacs-lisp-mode "(defun a ()\n  1)\n\n(defun b ()\n  2)\n" 3 30
                                      hel-mark-function "(defun b ()\n  2)\n")))
      (ert-info ((format "%S on %d..%d" command mark point))
        (hel-test-with mode (hel-test-mark text "")
          (set-mark mark)
          (goto-char point)
          (activate-mark)
          (funcall command 1)
          (should (equal (hel-test-selection) expected))
          ;; `M-i' returns to the selection before the command.
          (hel-shrink-selection 1)
          (should (= (point) point))
          (should (= (mark) mark)))))))

(ert-deftest hel-visible-line-bounds ()
  "A visible line holds the invisible lines that follow it."
  (hel-test-with #'fundamental-mode "a\nb\nc\nd\n|"
    (should (equal (hel-visible-line-bounds 3) '(3 . 5)))
    (should (equal (hel-visible-line-bounds 4) '(3 . 5)))
    (let ((overlay (make-overlay 2 6)))
      (overlay-put overlay 'invisible t)
      (should (equal (hel-visible-line-bounds 1) '(1 . 7)))
      (should (equal (hel-visible-line-bounds 7) '(7 . 9))))
    (goto-char 7)
    (should (equal (hel-visible-line-bounds) '(7 . 9)))
    (should (= (point) 7))))

(ert-deftest hel-function-jumps ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "class A")
    (skip-unless (hel-treesit-text-objects-p))
    (let (forward backward)
      (dotimes (_ 3)
        (hel-mark-function-forward 1)
        (push (hel-to--first-line) forward))
      (should (equal (nreverse forward)
                     '("def f(self, a, b):" "def h(self):" "def top(x):")))
      (should (= (point) (region-end)))
      (dotimes (_ 2)
        (hel-mark-function-backward 1)
        (push (hel-to--first-line) backward))
      ;; The first [f finishes the selected function, from its end back to
      ;; its start, and the next one selects the function before it.
      (should (equal (nreverse backward)
                     '("def top(x):" "def h(self):")))
      (should (= (point) (region-beginning))))))

(ert-deftest hel-function-jump-count ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "class A")
    (skip-unless (hel-treesit-text-objects-p))
    ;; Three whole functions, with everything between them.
    (hel-mark-function-forward 3)
    (should (equal (hel-to--first-line) "def f(self, a, b):"))
    (should (string-suffix-p "return x\n" (hel-test-selection)))
    ;; Fewer functions exist than the count: the motion stops at the last.
    (hel-mark-function-forward 5)
    (should (equal (hel-to--first-line) "def last():"))
    ;; No function is left: the selection stays.
    (hel-mark-function-forward 1)
    (should (equal (hel-to--first-line) "def last():"))))

(ert-deftest hel-function-jump-decorated ()
  "A motion selects a decorated function with its decorators."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--decorated "# Header")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function-forward 1)
    (should (equal (hel-to--first-line) "@dec"))))

(ert-deftest hel-argument-jumps ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "self, a")
    (skip-unless (hel-treesit-text-objects-p))
    (let (steps)
      (dotimes (_ 3)
        (hel-mark-argument-forward 1)
        (push (hel-test-selection) steps))
      ;; The first ]a finishes `self', and the next ones select whole
      ;; arguments without their commas.
      (should (equal (nreverse steps) '("self" "a" "b"))))))

(ert-deftest hel-backward-jump-to-adjacent-object ()
  "A backward motion reaches an object that ends right before the cursor."
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a,b)\n" "b")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-argument-backward 1)
    (should (equal (hel-test-selection) "a"))))

(ert-deftest hel-object-count ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function 2)
    (should (equal (hel-test-selection)
                   "    def f(self, a, b):\n        return g(a, b)\n\n    def h(self):\n        pass\n"))
    (hel-test-goto "pass")
    (hel-mark-around-function 2)
    (should (equal (hel-to--first-line) "def h(self):"))
    (should (equal (string-trim (hel-test-selection))
                   "def h(self):\n        pass")))
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a, b, c)\n" "a,")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-argument 2)
    (should (equal (hel-test-selection) "a, b"))
    (hel-test-goto "a,")
    (hel-mark-around-argument 2)
    (should (equal (hel-test-selection) "a, b, "))
    (hel-test-goto "b,")
    (hel-mark-argument -2)
    (should (equal (hel-test-selection) "a, b"))))

(ert-deftest hel-object-repeated-key ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (let ((one (hel-test-selection)))
      (hel-mark-around-function 1)
      (let ((two (hel-test-selection)))
        (should (string-prefix-p one two))
        (should (string-match-p "def h(self):\n        pass\n\n\\'" two))
        (hel-mark-around-function 1)
        (should (equal (hel-test-selection) two))
        (hel-shrink-selection 1)
        (should (equal (hel-test-selection) one))))))

(ert-deftest hel-object-shrink-returns ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--objects "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (set-mark (point))
    (forward-char 6)
    (hel-mark-around-function 1)
    (hel-shrink-selection 1)
    (should (equal (hel-test-selection) "return"))))

(ert-deftest hel-object-inherited-queries ()
  "The TypeScript file inherits the function object from `ecma'."
  (hel-test-with #'typescript-ts-mode (hel-test-mark "function f(a: number) {\n  return a;\n}\n" "return")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-to--first-line) "function f(a: number) {"))))

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
      (hel-test-with (car spec) (hel-test-mark (nth 1 spec) "a")
        (skip-unless (hel-treesit-text-objects-p))
        (goto-char (point-max))
        (search-backward "a")
        (hel-mark-around-function 1)
        (should (string-prefix-p (nth 2 spec)
                                 (string-trim-left (hel-test-selection))))))))

(ert-deftest hel-treesit-argument-is-built-as-argument ()
  "A tree-sitter argument is an instance of the thing `argument'."
  (hel-test-with #'python-ts-mode (hel-test-mark "f(a, b)\n" "a,")
    (skip-unless (hel-treesit-text-objects-p))
    (let (things)
      (cl-letf* ((expand #'hel-thing-maybe-expand-bounds-to-full-lines)
                 (original (symbol-function expand))
                 ((symbol-function expand)
                  (lambda (thing range)
                    (push thing things)
                    (funcall original thing range))))
        (hel-mark-argument 1))
      (should (equal (hel-test-selection) "a"))
      (should things)
      (should (equal (-uniq things) '(argument))))))

(ert-deftest hel-object-language-at-cursor ()
  "In HTML, the cursors in a script and in the markup use their own query."
  (skip-unless (and (fboundp 'mhtml-ts-mode)
                    (treesit-language-available-p 'html)
                    (treesit-language-available-p 'javascript)))
  (hel-test-with #'mhtml-ts-mode (hel-test-mark "<p>text</p>\n<!-- note -->\n<script>\nfunction f(a) {\n  return a;\n}\n</script>\n" "return")
    (treesit-update-ranges)
    (skip-unless (hel-treesit-text-objects-p))
    (should (eq 'javascript (treesit-language-at (point))))
    (hel-mark-function 1)
    (should (equal (hel-to--first-line) "function f(a) {"))
    (hel-test-goto "text")
    (should (eq 'html (treesit-language-at (point))))
    ;; The HTML query has no function object, so the markup keeps the
    ;; selection, and its comment object works.
    (hel-mark-function 1)
    (should (eq (hel-test-selection) nil))
    (hel-test-goto "note")
    (should (eq 'html (treesit-language-at (point))))
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) "<!-- note -->\n"))))

;;; Text objects without a parser

(defconst hel-to--elisp
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
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "1)")
    (hel-mark-function 1)
    (should (equal (hel-test-selection) "(defun a ()\n  1)\n"))
    (hel-test-goto "1)")
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   ";; Header of a.\n;; Second line.\n(defun a ()\n  1)\n\n"))
    (hel-test-goto "3)")
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "\n(defun c ()\n  3)\n"))))

(ert-deftest hel-elisp-function-count ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "2)")
    (hel-mark-function 2)
    (should (equal (hel-test-selection)
                   "(defun b ()\n  2)\n\n(defun c ()\n  3)\n"))
    (hel-test-goto "1)")
    (hel-mark-around-function 2)
    (should (equal (hel-test-selection)
                   ";; Header of a.\n;; Second line.\n(defun a ()\n  1)\n\n;; Header of b.\n(defun b ()\n  2)\n\n"))))

(ert-deftest hel-elisp-function-jumps ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "Header of a")
    (hel-mark-function-forward 1)
    (should (equal (hel-to--first-line) "(defun a ()"))
    (hel-mark-function-forward 1)
    (should (equal (hel-to--first-line) "(defun b ()"))
    ;; The first [f finishes b, from its end back to its start.
    (hel-mark-function-backward 1)
    (should (equal (hel-to--first-line) "(defun b ()"))
    (hel-mark-function-backward 1)
    (should (equal (hel-to--first-line) "(defun a ()"))))

(ert-deftest hel-elisp-comment-object ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "Second")
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection)
                   ";; Header of a.\n;; Second line.\n"))
    (hel-test-goto "Second")
    (hel-mark-comment 1)
    (should (equal (hel-test-selection)
                   ";; Header of a.\n;; Second line.\n"))
    (hel-test-goto "1)")
    (hel-mark-comment-forward 1)
    (should (equal (hel-test-selection) ";; Header of b.\n"))))

(ert-deftest hel-treesit-comment-jumps-select-runs ()
  "A comment after code and the comment lines below it are separate
comments. A jump selects comments of whole lines."
  (hel-test-with #'python-ts-mode (hel-test-mark "x = 1  # a\n# b\n# c\n\ny = 2\n# d\n" "x =")
    (skip-unless (hel-treesit-text-objects-p))
    (should (equal (cl-loop repeat 3
                            do (hel-mark-comment-forward 1)
                            collect (hel-test-selection))
                   '("# a" "# b\n# c\n" "# d\n")))
    (hel-test-goto "y =")
    (should (equal (cl-loop repeat 2
                            do (hel-mark-comment-backward 1)
                            collect (hel-test-selection))
                   '("# b\n# c\n" "# a")))))

;;; Arguments without a parser

(defconst hel-to--call "f(a, g(b, c), \"x, y\", [1, 2], d)\n")

(ert-deftest hel-syntax-argument-object ()
  "Without a parser, a comma at the level of the parentheses separates
the arguments, and a comma in nested brackets or in a string does not."
  (hel-test-with #'python-mode (hel-test-mark hel-to--call "a,")
    (should (hel-syntax-argument-producer-p (hel-thing-producer 'argument)))
    (dolist (case '(("a," "a")
                    ("g(" "g(b, c)")
                    ("(b" "g(b, c)")
                    ("b," "b")
                    ("c)" "c")
                    ("), \"" "c")
                    ("y\"" "\"x, y\"")
                    ("2]" "[1, 2]")
                    ("d)" "d")))
      (hel-test-goto (car case))
      (hel-mark-argument 1)
      (should (equal (list (car case) (hel-test-selection)) case)))))

(ert-deftest hel-syntax-argument-around ()
  "The gap of an argument is the whitespace and the comma after it, or
before it on the last argument."
  (hel-test-with #'python-mode (hel-test-mark hel-to--call "a,")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) "a, "))
    (hel-test-goto "c)")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) ", c"))
    (hel-test-goto "d)")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) ", d"))))

(ert-deftest hel-syntax-argument-count ()
  "A count selects the next arguments with the same parentheses."
  (hel-test-with #'python-mode (hel-test-mark hel-to--call "a,")
    (hel-mark-argument 2)
    (should (equal (hel-test-selection) "a, g(b, c)"))
    (hel-test-goto "c)")
    (hel-mark-argument 2)
    (should (equal (hel-test-selection) "c"))
    (hel-test-goto "d)")
    (hel-mark-argument -2)
    (should (equal (hel-test-selection) "[1, 2], d"))))

(ert-deftest hel-syntax-argument-jumps ()
  (hel-test-with #'python-mode (hel-test-mark "def f(self, a, b):\n    pass\n" "self")
    (should (equal (cl-loop repeat 3
                            do (hel-mark-argument-forward 1)
                            collect (hel-test-selection))
                   '("self" "a" "b"))))
  (hel-test-with #'python-mode (hel-test-mark "f(a,b)\n" "b")
    (hel-mark-argument-backward 1)
    (should (equal (hel-test-selection) "a"))))

(ert-deftest hel-syntax-argument-comments ()
  "A comma in a comment separates nothing, the comments at the edges of
an argument stay out of it, and a comment on the line above is its
header comment."
  (hel-test-with #'c-mode (hel-test-mark "f(a /* x, y */, b);\n" "b)")
    (hel-mark-argument -1)
    (should (equal (hel-test-selection) "b"))
    (hel-test-goto "a ")
    (hel-mark-argument 1)
    (should (equal (hel-test-selection) "a")))
  (hel-test-with #'c-mode (hel-test-mark "f(\n  // first\n  a,\n  b);\n" "a,")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) "// first\n  a,\n  "))))

(ert-deftest hel-syntax-argument-empty ()
  "Empty parentheses and a trailing comma give no argument."
  (hel-test-with #'python-mode (hel-test-mark "f(a, b,)\ng()\n" "b,")
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) ", b"))
    (hel-test-goto ")\ng(")
    (should-not (hel-thing-at 'argument (hel-thing-producer 'argument)
                              (point)))
    (let ((inhibit-message t))
      (hel-test-goto "()")
      (forward-char)
      (hel-mark-argument 1)
      (should (eq (hel-test-selection) nil)))))

(ert-deftest hel-syntax-argument-search-in-growing-ranges ()
  "A search in small ranges finds the same arguments as one search over
the whole buffer, from every position and in both directions."
  (hel-test-with #'python-mode (hel-test-mark hel-to--search-text "def first")
    (let* ((producer (hel-thing-producer 'argument))
           (results (lambda ()
                      (cl-loop for pos from (point-min) to (point-max)
                               append (cl-loop for dir in '(1 -1)
                                               collect (-some->
                                                           (hel-thing-next 'argument producer pos dir)
                                                         (hel-thing-instance-bounds))))))
           (whole (let ((hel--search-range-size (buffer-size)))
                    (funcall results)))
           (small (let ((hel--search-range-size 16))
                    (funcall results))))
      (should (hel-syntax-argument-producer-p producer))
      (should (cl-some #'identity whole))
      (should (equal small whole)))))

(ert-deftest hel-syntax-argument-agrees-with-query ()
  "In calls and definitions, the syntax table and the query find the same
arguments, with the same around ranges."
  (hel-test-with #'python-ts-mode
      (hel-test-mark "def f(self, a, b=[1, 2],\n      c=None):\n    return g(a, h(b, c), \"x, y\")\n" "def")
    (skip-unless (hel-treesit-text-objects-p))
    (let ((query (hel-treesit-query-producer--create))
          (syntax (hel-syntax-argument-producer--create)))
      (cl-flet ((ranges (producer)
                  (cl-loop for pos from (point-min) below (point-max)
                           for instance = (hel-thing-owning 'argument producer pos)
                           collect (and instance
                                        (list (hel-thing-instance-bounds instance)
                                              (hel-thing-around 'argument producer
                                                                instance instance))))))
        (let ((expected (ranges query)))
          (should (cl-some #'identity expected))
          (should (equal (ranges syntax) expected)))))))

(ert-deftest hel-no-parser-class-and-argument ()
  "In Lisp without a query, the class and argument objects keep the
selection."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "defun a")
    (let ((inhibit-message t))
      (hel-mark-argument 1)
      (hel-mark-around-class 1))
    (should (eq (hel-test-selection) nil))))

;;; Prose objects

(defconst hel-to--prose-paragraphs
  "Foo bar.  Baz qux.\nMore here.\n\n  Second para.\nline two.\n\n\nThird.\n")

(ert-deftest hel-word-object ()
  "The gap of a word holds only spaces and tabs."
  (hel-test-with #'text-mode (hel-test-mark "foo bar baz\n  qux" "bar")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) "bar "))
    (hel-test-goto "baz")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) " baz"))
    (hel-test-goto "bar")
    (hel-mark-word 2)
    (should (equal (hel-test-selection) "bar baz"))))

(ert-deftest hel-word-object-before-punctuation ()
  "A word right before punctuation takes the gap before it. So does
a WORD, because the punctuation at its edges is not part of it."
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "bar")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) " bar"))
    (hel-test-goto "bar")
    (hel-mark-around-WORD 1)
    (should (equal (hel-test-selection) " bar"))))

(ert-deftest hel-WORD-object-trims-punctuation ()
  (hel-test-with #'text-mode (hel-test-mark "used only for ~hel-thing-owning~.\n" "thing")
    (hel-mark-WORD 1)
    (should (equal (hel-test-selection) "~hel-thing-owning~"))
    (hel-test-goto "thing")
    (hel-mark-around-WORD 1)
    (should (equal (hel-test-selection) " ~hel-thing-owning~")))
  (hel-test-with #'text-mode (hel-test-mark "review it against /generic-programming-design.\n" "programming")
    (hel-mark-WORD 1)
    (should (equal (hel-test-selection) "/generic-programming-design"))
    (hel-test-goto "programming")
    (hel-mark-around-WORD 1)
    (should (equal (hel-test-selection) " /generic-programming-design"))))

(ert-deftest hel-trim-punctuation ()
  "A separator goes from its own side of the WORD. Brackets and quotes
stay when their pair is inside the WORD, a pair that wraps the whole WORD
goes, and an apostrophe without a pair stays. A WORD of only punctuation
stays."
  (dolist (case '(("foo." "foo")
                  ("(foo" "foo")
                  ("foo)," "foo")
                  ("(foo)" "foo")
                  ("foo(bar)" "foo(bar)")
                  ("foo(bar)," "foo(bar)")
                  ("(foo(bar))" "foo(bar)")
                  ("(a)(b)" "(a)(b)")
                  ("\"quoted\"" "quoted")
                  ("dogs'" "dogs'")
                  ("'tis" "'tis")
                  ("'foo" "'foo")
                  ("'quoted'," "quoted")
                  (":begin" ":begin")
                  ("#'car," "#'car")
                  ("!enabled" "!enabled")
                  ("?a" "?a")
                  ("¿Dónde" "Dónde")
                  ("estás?" "estás")
                  ("«слово»," "слово")
                  ("«слово" "слово")
                  ("„Wort“" "Wort")
                  ("“word”." "word")
                  ("‘word’" "word")
                  ("`foo'." "foo")
                  ("``foo''" "foo")
                  ("`code`" "code")
                  ("don't" "don't")
                  ("std::vector," "std::vector")
                  ("hel-text-objects.el." "hel-text-objects.el")
                  ("..." "...")
                  ("()" "()")))
    (with-temp-buffer
      (insert (car case))
      (should (equal (-let [(begin . end)
                            (hel-trim-punctuation-around-word
                             (cons (point-min) (point-max)))]
                       (buffer-substring begin end))
                     (cadr case))))))

(ert-deftest hel-sentence-object ()
  "The gap of a sentence stays on its line."
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "Baz")
    (hel-mark-sentence 1)
    (should (equal (hel-test-selection) "Baz qux."))
    (hel-test-goto "Baz")
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) "  Baz qux."))
    (hel-test-goto "More")
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) "More here."))))

(ert-deftest hel-sentence-count-stays-in-paragraph ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "Baz")
    (hel-mark-sentence 5)
    (should (equal (hel-test-selection) "Baz qux.\nMore here."))))

(ert-deftest hel-sentence-motions ()
  "]s and [s select from point to the end or the start of the sentence."
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "qux")
    (hel-mark-sentence-forward 1)
    (should (equal (hel-test-selection) "qux."))
    (hel-test-goto "here")
    (hel-mark-sentence-backward 1)
    (should (equal (hel-test-selection) "More "))))

(ert-deftest hel-sentence-stays-in-string ()
  "A sentence in a docstring leaves out the quotes and the code around them."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  \"Return |foo.  Next one.\"\n  nil)\n"
    (hel-mark-sentence 1)
    (should (equal (hel-test-selection) "Return foo."))
    (hel-test-goto "one")
    (hel-mark-sentence 1)
    (should (equal (hel-test-selection) "Next one."))))

(ert-deftest hel-sentence-stays-in-comment ()
  "A sentence in a comment leaves out the comment starter and the code."
  (hel-test-with #'emacs-lisp-mode "(foo) ;; Bar |baz.  Qux.\n"
    (hel-mark-sentence 1)
    (should (equal (hel-test-selection) "Bar baz."))
    (hel-test-goto "Qux")
    (hel-mark-sentence 1)
    (should (equal (hel-test-selection) "Qux."))))

(ert-deftest hel-paragraph-object ()
  "A paragraph covers whole lines."
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "two")
    (hel-mark-paragraph 1)
    (should (equal (hel-test-selection)
                   "  Second para.\nline two.\n"))
    (hel-test-goto "two")
    (hel-mark-around-paragraph 1)
    (should (equal (hel-test-selection)
                   "  Second para.\nline two.\n\n\n"))
    (hel-test-goto "Third")
    (hel-mark-around-paragraph 1)
    (should (equal (hel-test-selection) "\n\nThird.\n"))))

(ert-deftest hel-paragraph-object-from-indentation ()
  "From the indentation of the first line, `mip' selects that paragraph."
  (hel-test-with #'text-mode (hel-test-mark hel-to--prose-paragraphs "Second")
    (dolist (pos (list (line-beginning-position) (1+ (line-beginning-position))))
      (hel-test-goto "Second")
      (goto-char pos)
      (hel-mark-paragraph 1)
      (should (equal (hel-test-selection)
                     "  Second para.\nline two.\n")))))

(ert-deftest hel-word-object-no-header-comment ()
  "A comment line above a word stays out of the selection."
  (hel-test-with #'emacs-lisp-mode (hel-test-mark ";; note\nfoo bar\n" "foo")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) "foo "))))

;;; Search in growing ranges

(defconst hel-to--search-text
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

(defun hel-to--search-results ()
  "Return the answers of the text object searches at every position.
The list holds the next function, the next argument and the next comment
from each position in both directions, and the sibling of each function
and argument in both directions."
  (let* ((function-producer (hel-treesit-query-producer--create))
         (argument-producer (hel-treesit-query-producer--create))
         (known (->> `((function ,function-producer)
                       (argument ,argument-producer))
                     (-mapcat
                      (-lambda ((thing producer))
                        (-map (lambda (instance) (list thing producer instance))
                              (hel-treesit--capture-things
                               thing (point-min) (point-max))))))))
    (append
     (cl-loop for pos from (point-min) to (point-max)
              append (cl-loop for dir in '(1 -1)
                              collect (list pos dir
                                            (hel-thing-next 'function function-producer pos dir)
                                            (hel-thing-next 'argument argument-producer pos dir)
                                            (-some-> (hel--comment-piece-next
                                                      (hel-treesit-comment-producer--create)
                                                      pos dir)
                                              (car)))))
     (cl-loop for (thing producer instance) in known
              append (cl-loop for dir in '(1 -1)
                              collect (-some-> (hel-thing-sibling thing producer instance dir)
                                        (hel-thing-instance-begin)))))))

(ert-deftest hel-treesit-search-in-growing-ranges ()
  "A search in small ranges finds the same objects as one whole-buffer
scan, from every position and in both directions."
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--search-text "def first")
    (skip-unless (hel-treesit-text-objects-p))
    (let ((whole (let ((hel--search-range-size (buffer-size)))
                   (hel-to--search-results)))
          (small (let ((hel--search-range-size 16))
                   (hel-to--search-results))))
      ;; The text must give answers, or the comparison proves nothing.
      (should (cl-some #'caddr whole))
      (should (cl-some (lambda (result) (and (integerp result) result)) whole))
      (should (equal small whole)))))

;;; Query files

(ert-deftest hel-treesit-query-patterns ()
  (should (equal (hel-treesit--query-patterns
                  "; comment\n(a) @x\n\n((b)\n (#match? @y \"(\"))\n(c)+\n @z\n")
                 '("(a) @x" "((b)\n (#match? @y \"(\"))" "(c)+\n @z"))))

(defmacro hel-to--with-query-file (text &rest body)
  "Run BODY with a queries directory whose Python file holds TEXT."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'python))
     (let* ((dir (make-temp-file "hel-queries" t))
            (hel-test-queries-directory (file-name-as-directory dir))
            (hel-test--query-cache (make-hash-table :test #'eq))
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
  (hel-to--with-query-file
      "(no_such_node) @function\n(function_definition) @function\n"
    (hel-test-with #'python-ts-mode (hel-test-mark "def f():\n    return 1\n" "return")
      (skip-unless (hel-treesit-text-objects-p))
      (hel-mark-around-function 1)
      (should (equal (hel-to--first-line) "def f():")))))

;;;; The things of a match

(defun hel-to--capture-types (thing)
  "Return the node types of each THING in the buffer, one list per thing."
  (->> (hel-treesit--capture-things thing (point-min) (point-max))
       (-map (lambda (instance)
               (-map #'treesit-node-type (hel-thing-instance-data instance))))))

(ert-deftest hel-treesit-node-hash-test ()
  "Two lookups of the same node find the same key of a `hel-treesit-node'
table, and another node does not."
  (hel-test-with #'python-ts-mode "|def f(a, b):\n    return a\n"
    (let ((table (make-hash-table :test 'hel-treesit-node)))
      (puthash (treesit-node-at 7) 'a table)
      (should (eq 'a (gethash (treesit-node-at 7) table)))
      (should (null (gethash (treesit-node-at 10) table))))))

(ert-deftest hel-treesit-common-ancestor-laws ()
  "The common ancestor is idempotent, commutative and associative, and nil
stands above the root."
  (hel-test-with #'python-ts-mode "|def f(a, b):\n    return a\n"
    (let* ((a (treesit-node-at 7))
           (b (treesit-node-at 10))
           (return (treesit-node-parent (treesit-node-at 25)))
           (root (treesit-buffer-root-node)))
      (cl-flet ((ancestor (x y) (hel-treesit--common-ancestor x y)))
        (should (treesit-node-eq (ancestor a a) a))
        (should (treesit-node-eq (ancestor a b) (ancestor b a)))
        (should (equal (treesit-node-type (ancestor a b)) "parameters"))
        (should (treesit-node-eq (ancestor (ancestor a b) return)
                                 (ancestor a (ancestor b return))))
        (should (equal (treesit-node-type (ancestor a return))
                       "function_definition"))
        (should (treesit-node-eq (ancestor a root) root))
        (should (null (ancestor a nil)))
        (should (null (ancestor nil a)))))))

(ert-deftest hel-treesit-shadowed-node-not-largest ()
  "A match is shadowed through any of its nodes, not only its largest one."
  (hel-to--with-query-file
      (concat "((comment) @function . (function_definition) @function)\n"
              "(module (function_definition) @_shadowed) @function\n")
    (hel-test-with #'python-ts-mode
        "|# A comment that is longer than the function after it.\ndef f(): pass\n"
      (should (equal (hel-to--capture-types 'function) '(("module")))))))

(ert-deftest hel-treesit-shadowed-nodes-all-count ()
  "A pattern that marks two nodes as shadowed drops the matches of both."
  (hel-to--with-query-file
      (concat "(function_definition) @function\n"
              "(module (function_definition) @_shadowed\n"
              "        .\n"
              "        (function_definition) @_shadowed) @function\n")
    (hel-test-with #'python-ts-mode "|def f(): pass\ndef g(): pass\n"
      (should (equal (hel-to--capture-types 'function) '(("module")))))))

(ert-deftest hel-treesit-parent-of-several-nodes ()
  "The parent of a thing is the nearest node above all of its nodes: the
parent of siblings, and the parent of the outer node when the nodes nest."
  (hel-to--with-query-file
      (concat "((comment) @function . (function_definition) @function)\n"
              "(class_definition\n"
              "  body: (block (function_definition name: (identifier) @function)\n"
              "               @function))\n")
    (hel-test-with #'python-ts-mode
        "|# f\ndef f(): pass\nclass C:\n    def m(self): pass\n"
      (let ((producer (hel-treesit-query-producer--create)))
        (should (equal (->> (hel-treesit--capture-things 'function
                                                         (point-min) (point-max))
                            (-map (lambda (instance)
                                    (treesit-node-type
                                     (hel-thing-parent 'function producer
                                                       instance)))))
                       '("module" "block")))))))

(ert-deftest hel-treesit-unbalanced-query-file ()
  "A query file that cannot be split gives no query and no error, and the
function object comes from `hel-function'."
  (hel-to--with-query-file
      "(function_definition) @function\n(class_definition\n"
    (hel-test-with #'python-ts-mode (hel-test-mark "def f():\n    return 1\n" "return")
      (should-not (hel-treesit-text-objects-p))
      (hel-mark-function 1)
      (should (equal (hel-test-selection)
                     "def f():\n    return 1\n")))))

;;; The patterns of each language

(defun hel-to--object-text (language text needle object)
  "Return the text of the innermost OBJECT at NEEDLE in TEXT, or nil.
TEXT is parsed as LANGUAGE, with the queries of the repository."
  (hel-test-with #'fundamental-mode (hel-test-mark text needle)
    (treesit-parser-create language)
    (let ((pos (point)))
      (-some->> (hel-treesit--capture-things (intern object)
                                             (point-min) (point-max))
        (-filter (lambda (o)
                   (<= (hel-thing-instance-begin o) pos (hel-thing-instance-end o))))
        (-min-by (-on #'> (-compose #'hel-range-length
                                    #'hel-thing-instance-bounds)))
        (funcall (lambda (o)
                   (string-trim
                    (buffer-substring-no-properties (hel-thing-instance-begin o)
                                                    (hel-thing-instance-end o)))))))))

(defmacro hel-to--objects-should (language &rest cases)
  "Check CASES of (TEXT NEEDLE OBJECT EXPECTED) in LANGUAGE.
Skip the test when LANGUAGE has no grammar or Emacs has no GROUPED
captures."
  (declare (indent 1))
  `(progn
     (skip-unless (and (<= 31 emacs-major-version)
                       (treesit-language-available-p ',language)))
     ,@(mapcar (pcase-lambda (`(,text ,needle ,object ,expected))
                 `(should (equal (hel-to--object-text
                                  ',language ,text ,needle ,object)
                                 ,expected)))
               cases)))

(ert-deftest hel-language-patterns-elixir ()
  "The Elixir patterns, whose regexes use alternation, find objects."
  (hel-to--objects-should elixir
    ("defmodule M do\n  def f() do\n    1\n  end\nend\n" "f()" "function"
     "def f() do\n    1\n  end")
    ("defmodule M do\n  def f() do\n    1\n  end\nend\n" "M " "class"
     "defmodule M do\n  def f() do\n    1\n  end\nend")))

(ert-deftest hel-language-patterns-javascript ()
  (hel-to--objects-should javascript
    ("export function f() {}\n" "f(" "function" "export function f() {}")
    ("export default class { m() {} }\n" "m(" "class"
     "export default class { m() {} }")
    ("const h = () => 1;\n" "=>" "function" "const h = () => 1;")
    ("export const e = () => 1;\n" "=>" "function" "export const e = () => 1;")
    ("foo(() => 1);\n" "=>" "function" "() => 1")))

(ert-deftest hel-language-patterns-typescript ()
  (hel-to--objects-should typescript
    ("export abstract class A { }\n" "A " "class" "export abstract class A { }")
    ("export interface I { x: number }\n" "I " "class"
     "export interface I { x: number }")
    ("declare function d(): void;\n" "d(" "function"
     "declare function d(): void;")))

(ert-deftest hel-language-patterns-rust ()
  (hel-to--objects-should rust
    ("#[derive(Debug)]\npub struct S { x: i32 }\n" "S " "class"
     "#[derive(Debug)]\npub struct S { x: i32 }")
    ("struct P { x: i32 }\n" "P " "class" "struct P { x: i32 }")))

(ert-deftest hel-language-patterns-c ()
  (hel-to--objects-should c
    ("struct S { int x; };\n" "S " "class" "struct S { int x; };")
    ("typedef struct S { int x; } S;\n" "S {" "class"
     "typedef struct S { int x; } S;")
    ("struct F;\n" "F;" "class" "struct F;")
    ("void f() { struct stat b; }\n" "stat" "class" nil)))

(ert-deftest hel-language-patterns-cpp ()
  "C++ reads Hel's C patterns through its inheritance, and its own."
  (hel-to--objects-should cpp
    ("struct P { int y; };\n" "P " "class" "struct P { int y; };")
    ("class C { int x; };\n" "C " "class" "class C { int x; };")
    ("template <typename T>\nclass D { T x; };\n" "D " "class"
     "template <typename T>\nclass D { T x; };")
    ("class F;\n" "F;" "class" "class F;")
    ("template <typename T>\nclass G;\n" "G;" "class"
     "template <typename T>\nclass G;")
    ("class H {\n  class I;\n};\n" "I;" "class" "class I;")
    ("void f() { class A *p; }\n" "A " "class" nil)
    ("template <typename T>\nT f(T x) { return x; }\n" "f(" "function"
     "template <typename T>\nT f(T x) { return x; }")))

(ert-deftest hel-language-patterns-ruby ()
  (hel-to--objects-should ruby
    ("class K\n  private def m\n    1\n  end\nend\n" "m\n" "function"
     "private def m\n    1\n  end")))

(ert-deftest hel-language-patterns-entries ()
  "An entry is an element, a key with its value, or a field."
  (hel-to--objects-should json
    ("{\"a\": 1, \"b\": [2, 3]}\n" "\"b\"" "entry" "\"b\": [2, 3]")
    ("{\"a\": 1, \"b\": [2, 3]}\n" "3" "entry" "3"))
  (hel-to--objects-should rust
    ("struct S { x: i32, y: i32 }\n" "y:" "entry" "y: i32")
    ("let v = vec![1, 2];\n" "2]" "entry" "2"))
  (hel-to--objects-should javascript
    ("const o = {a: 1, b: 2};\n" "b:" "entry" "b: 2")))

;;; Bindings

(ert-deftest hel-structural-bindings ()
  "Every key is a plain binding to one command."
  (hel-test-with #'fundamental-mode ""
    (hel-local-mode 1)
    (hel-normal-state)
    (hel-update-active-keymaps)
    (should (equal (-map (lambda (key) (key-binding (kbd key)))
                         '("m f" "] f" "m i a" "M-n" "M-o" "M-a"))
                   '(hel-mark-function hel-mark-function-forward
                                             hel-mark-argument hel-select-next-sibling
                                             hel-expand-selection-backward hel-select-all-siblings)))))

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
  "Three comments: two full lines, a comment after code, one line.")

(defun hel-to--goto-blank-line (text)
  "Deactivate the mark and move point to the blank line before TEXT.
TEXT must follow exactly one blank line."
  (hel-test-goto (concat "\n\n" text))
  (forward-char 1))

(defun hel-to--producer (object)
  "Return the producer of OBJECT at point.
It is the result of the first source of OBJECT that applies at point."
  (-some #'funcall (get object 'hel-thing-sources)))

(defun hel-to--key (object)
  "Return the essential parts of OBJECT, which define its equality."
  (list (hel-thing-instance-begin object)
        (hel-thing-instance-end object)))

(defun hel-to--text (object)
  "Return the text of the range of OBJECT."
  (buffer-substring-no-properties (hel-thing-instance-begin object)
                                  (hel-thing-instance-end object)))

(defun hel-to--known-objects (thing producer)
  "Return every THING that PRODUCER reports somewhere in the buffer.
The list holds the results of `hel-thing-next' from every position in
both directions and the results of `hel-thing-candidates' on every character.
Each object appears once."
  (let (objects)
    (dolist (pos (number-sequence (point-min) (point-max)))
      (dolist (dir '(1 -1))
        (when-let* ((object (hel-thing-next thing producer pos dir)))
          (push object objects)))
      (when (< pos (point-max))
        (setq objects (append (hel-thing-candidates thing producer pos)
                              objects))))
    (cl-remove-duplicates objects :key #'hel-to--key :test #'equal)))

(defun hel-to--object-with-text (objects text)
  "Return the object of OBJECTS whose range holds TEXT and whitespace."
  (-first (lambda (object) (equal (string-trim (hel-to--text object)) text))
          objects))

(defun hel-to--nearest (objects pos dir)
  "Return the object of OBJECTS that `hel-thing-next' must give at POS.
Forward, it is the object that starts first after POS. Backward, it is
the object that ends last at or before POS. Of two objects that start, or
end, at the same place, the larger one wins."
  (let ((forward (< 0 dir)))
    (car (sort (-filter (lambda (object)
                          (if forward
                              (< pos (hel-thing-instance-begin object))
                            (<= (hel-thing-instance-end object) pos)))
                        objects)
               (lambda (a b)
                 (let ((edge-a (if forward
                                   (hel-thing-instance-begin a)
                                 (- (hel-thing-instance-end a))))
                       (edge-b (if forward
                                   (hel-thing-instance-begin b)
                                 (- (hel-thing-instance-end b))))
                       (size-a (hel-range-length (hel-thing-instance-bounds a)))
                       (size-b (hel-range-length (hel-thing-instance-bounds b))))
                   (or (< edge-a edge-b)
                       (and (= edge-a edge-b)
                            (> size-a size-b)))))))))

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
      (should (< (hel-thing-instance-begin object) (hel-thing-instance-end object))))
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
                                     (< pos (hel-thing-instance-begin next))
                                   (<= (hel-thing-instance-end next) pos))))))
          (should (equal (list pos dir (-some-> next (hel-to--key)))
                         (list pos dir (-some-> (hel-to--nearest objects pos dir)
                                         hel-to--key)))))))
    ;; `hel-thing-at' returns the innermost object that holds the character
    ;; at POS, and when none holds it, the innermost object that ends at POS.
    ;; Objects of the same size that overlap are equally innermost.
    (dolist (pos positions)
      (cl-flet ((innermost (pred)
                  (when-let* ((matching (-filter pred objects))
                              (size (->> matching
                                         (-map #'hel-thing-instance-bounds)
                                         (-map #'hel-range-length)
                                         (-min))))
                    (->> matching
                         (-filter (lambda (object)
                                    (= size (hel-range-length
                                             (hel-thing-instance-bounds object)))))
                         (-map #'hel-to--key)))))
        (let ((expected
               (or (innermost (lambda (object)
                                (and (<= (hel-thing-instance-begin object) pos)
                                     (< pos (hel-thing-instance-end object)))))
                   (innermost (lambda (object)
                                (= pos (hel-thing-instance-end object))))))
              (at (-some-> (hel-thing-at thing producer pos) hel-to--key)))
          (should (equal (list pos (if expected
                                       (and (member at expected) t)
                                     (null at)))
                         (list pos t))))))
    ;; Siblings are inverse in the two directions.
    (dolist (object objects)
      (when-let* ((sibling (hel-thing-sibling thing producer object 1)))
        (cl-incf siblings)
        (should (equal (list (hel-to--key object)
                             (-some-> (hel-thing-sibling thing producer sibling -1)
                               hel-to--key))
                       (list (hel-to--key object) (hel-to--key object))))))
    ;; A comment has no siblings: a count selects one comment.
    (unless (eq thing 'comment)
      (should (< 0 siblings)))
    ;; `hel-thing-after-gap' moves in DIR, and from its own result the gap
    ;; is empty, so a second call returns nil.
    (dolist (pos positions)
      (dolist (dir '(1 -1))
        (when-let* ((end (hel-thing-after-gap thing producer pos dir)))
          (should (equal (list pos dir t)
                         (list pos dir (if (< 0 dir) (< pos end) (< end pos)))))
          (should (equal (list pos dir nil)
                         (list pos dir (hel-thing-after-gap thing producer end dir)))))))
    objects))

;;; Moving to the edge of an object: functions

;; Section "Moving to the edge of an object": inside an object, `]f'
;; selects from point to the end of the object under the cursor. At that
;; edge it selects the whole next object. The object leaves out the header
;; comment (section "The ranges of an object").
(ert-deftest hel-to-function-forward-from-inside ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection) "return g(a, b)\n"))
    (should (= (point) (region-end)))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection)
                   "    def h(self):\n        pass\n"))
    (should (= (point) (region-end)))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection)
                   "def top(x):\n    return x\n"))
    (should (= (point) (region-end)))))

;; Section "Moving to the edge of an object": `[f' inside a function
;; selects from point to its start, and at that edge it selects the whole
;; previous object. Point goes to the start after `['.
(ert-deftest hel-to-function-backward-from-inside ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function-backward 1)
    (should (equal (hel-test-selection) "    def h(self):\n        "))
    (should (= (point) (region-beginning)))
    (hel-mark-function-backward 1)
    (should (equal (hel-test-selection)
                   "    def f(self, a, b):\n        return g(a, b)\n"))
    (should (= (point) (region-beginning)))))

;; Section "Moving to the edge of an object": "Of nested objects, the
;; innermost one counts."
(ert-deftest hel-to-function-edge-innermost ()
  (hel-test-with #'python-ts-mode (hel-test-mark "def f():\n    def inner():\n        pass\n    return 1\n" "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection) "pass\n"))
    (hel-test-goto "pass")
    (hel-mark-function-backward 1)
    (should (equal (hel-test-selection) "    def inner():\n        "))))

;; Section "Moving to the edge of an object": "3]f inside a function selects
;; to the end of the function and of the two functions after it, with
;; everything between them. When fewer objects exist, the motion stops at
;; the last one." Section "Tests": one more `]f' keeps the selection.
(ert-deftest hel-to-function-forward-count ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (let ((expected (concat "return g(a, b)\n\n"
                            "    def h(self):\n        pass\n\n"
                            "# Header of top.\n"
                            "def top(x):\n    return x\n")))
      (hel-mark-function-forward 3)
      (should (equal (hel-test-selection) expected))
      (should (= (point) (region-end)))
      (hel-mark-function-forward 1)
      (should (equal (hel-test-selection) expected)))))

;; Section "Moving to the edge of an object": "]f selects a decorated
;; function with its decorators", because the motion uses the same objects
;; as `mi'. The header comment line is outside every object, so `]f'
;; selects the whole next object. From the decorator line the cursor is
;; inside the object, and `]f' selects from point to its end: the same text.
(ert-deftest hel-to-function-forward-decorated ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--decorated "# Header")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection)
                   "@dec\ndef f():\n    def inner():\n        pass\n"))
    (hel-test-goto "@dec")
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection)
                   "@dec\ndef f():\n    def inner():\n        pass\n"))))

;; Section "Moving to the edge of an object" and "Tests": "]a from self
;; selects to the end of self, and the next two ]a select a and b without
;; their commas."
(ert-deftest hel-to-argument-forward ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "self")
    (skip-unless (hel-treesit-text-objects-p))
    (should (equal (cl-loop repeat 3
                            do (hel-mark-argument-forward 1)
                            collect (hel-test-selection))
                   '("self" "a" "b")))))

;; Section "Moving to the edge of an object": without a parser, `]f' runs
;; through `hel-function-motion'. Inside a `defun' it selects to its end,
;; and the next `]f' selects the next `defun' without its header comment.
(ert-deftest hel-to-elisp-function-edges ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "1)")
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection) "1)\n"))
    (hel-mark-function-forward 1)
    (should (equal (hel-test-selection) "(defun b ()\n  2)\n"))
    (hel-test-goto "2)")
    (hel-mark-function-backward 1)
    (should (equal (hel-test-selection) "(defun b ()\n  "))
    (should (= (point) (region-beginning)))
    (hel-mark-function-backward 1)
    (should (equal (hel-test-selection) "(defun a ()\n  1)\n"))))

;; Section "Moving to the edge of an object": "]c selects comments of whole
;; lines, so in the lines x = 1  # a, # b and # c, it selects # a first and
;; then the run of # b and # c." Section "Choosing a producer": without a
;; parser, the comment object uses the syntax table.
(ert-deftest hel-to-comment-forward-syntax-table ()
  (hel-test-with #'python-mode (hel-test-mark "x = 1  # a\n# b\n# c\n" "x =")
    (should (equal (cl-loop repeat 2
                            do (hel-mark-comment-forward 1)
                            collect (hel-test-selection))
                   '("# a" "# b\n# c\n")))))

;;; Moving to the edge of an object: prose

;; Section "Moving to the edge of an object": at the edge of a sentence,
;; `]s' selects the whole next sentence. "The motion follows the order of
;; the buffer and ignores parents", so it passes the end of the paragraph.
(ert-deftest hel-to-sentence-edges ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
    (setq-local sentence-end-double-space nil)
    (should (equal (cl-loop repeat 3
                            do (hel-mark-sentence-forward 1)
                            collect (hel-test-selection))
                   '("Three four." "Five six." "Seven eight.")))
    (hel-test-goto "four")
    (should (equal (cl-loop repeat 2
                            do (hel-mark-sentence-backward 1)
                            collect (hel-test-selection))
                   '("Three " "One two.")))
    (should (= (point) (region-beginning)))))

;; Section "Moving to the edge of an object": "]s with a count passes the
;; end of a paragraph, while mis stops there." "When fewer objects exist,
;; the motion stops at the last one."
(ert-deftest hel-to-sentence-forward-count ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
    (setq-local sentence-end-double-space nil)
    (hel-mark-sentence-forward 3)
    (should (equal (hel-test-selection)
                   "Three four.\nFive six.\n\nSeven eight."))
    (hel-test-goto "Three")
    (hel-mark-sentence-forward 10)
    (should (equal (hel-test-selection)
                   "Three four.\nFive six.\n\nSeven eight. Nine ten."))))

;; Section "Moving to the edge of an object": outside every object, `]s'
;; selects the whole next object and `[s' the whole previous one. A blank
;; line is outside every sentence, because an object has no whitespace at
;; its edges (section "The object").
(ert-deftest hel-to-sentence-edges-outside-objects ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "One")
    (setq-local sentence-end-double-space nil)
    (hel-to--goto-blank-line "Seven")
    (hel-mark-sentence-forward 1)
    (should (equal (hel-test-selection) "Seven eight."))
    (hel-to--goto-blank-line "Seven")
    (hel-mark-sentence-backward 1)
    (should (equal (hel-test-selection) "Five six."))))

;; Section "Moving to the edge of an object": `}' selects from point to the
;; start of the next paragraph. A second `}' from the start of a paragraph
;; goes to the start of the paragraph after it, and a count adds
;; paragraphs.
(ert-deftest hel-to-paragraph-forward-start ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--paragraphs "bar")
    (hel-forward-paragraph 1)
    (should (equal (hel-test-selection) "bar.\nMore here.\n\n"))
    (should (= (point) (region-end)))
    (hel-forward-paragraph 1)
    (should (equal (hel-test-selection) "Second para.\nline two.\n\n"))
    (hel-test-goto "bar")
    (hel-forward-paragraph 2)
    (should (equal (hel-test-selection)
                   "bar.\nMore here.\n\nSecond para.\nline two.\n\n"))))

;; Section "Moving to the edge of an object": `{' selects from point to the
;; start of the paragraph under the cursor. When point is already there,
;; it selects to the start of the previous paragraph.
(ert-deftest hel-to-paragraph-backward-start ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--paragraphs "two")
    (hel-backward-paragraph 1)
    (should (equal (hel-test-selection) "Second para.\nline "))
    (should (= (point) (region-beginning)))
    ;; From the start of the paragraph: to the start of the one before.
    (hel-backward-paragraph 1)
    (should (equal (hel-test-selection)
                   "Foo bar.\nMore here.\n\n"))
    (hel-test-goto "two")
    (hel-backward-paragraph 2)
    (should (equal (hel-test-selection)
                   "Foo bar.\nMore here.\n\nSecond para.\nline "))))

;;; Prose objects

;; Section "The ranges of an object": "A word's gap stays on its line."
;; Section "The around range": "When the object has neither gap, the around
;; range has no gap." A tab is a gap of a word, as a space is.
(ert-deftest hel-to-word-gap-stays-on-line ()
  (hel-test-with #'text-mode (hel-test-mark "foo\nbar baz\n" "foo")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) "foo")))
  (hel-test-with #'text-mode (hel-test-mark "one\ttwo\nthree\n" "two")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) "\ttwo"))))

;; Section "The ranges of an object": a word has no header comment, so
;; `maw' in an Emacs Lisp buffer leaves out a comment line above the word.
;; The word starts its line and ends it, so it has no gap either.
(ert-deftest hel-to-word-ignores-comment-above ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark ";; note\nfoo\n" "foo")
    (hel-mark-around-word 1)
    (should (equal (hel-test-selection) "foo"))))

;; Section "The ranges of an object": "A sentence's gap stays on its line."
;; `mas' takes the blanks after a sentence, and the blanks before the last
;; sentence of a line.
(ert-deftest hel-to-sentence-around ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "One")
    (setq-local sentence-end-double-space nil)
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) "One two. "))
    (should (= (point) (region-end)))
    (hel-test-goto "Three")
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) " Three four."))
    (hel-test-goto "Five")
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) "Five six."))
    (hel-test-goto "Nine")
    (hel-mark-around-sentence 1)
    (should (equal (hel-test-selection) " Nine ten."))))

;; Section "The ranges of an object": "mas and d never join two lines."
(ert-deftest hel-to-sentence-around-delete ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
    (setq-local sentence-end-double-space nil)
    (hel-mark-around-sentence 1)
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string)
                   "One two.\nFive six.\n\nSeven eight. Nine ten.\n"))))

;; Section "The around range": a gap counts unless it is empty or ends at
;; a border, the edge of the buffer or a bracket.
(ert-deftest hel-to-around-gap-and-border ()
  ;; The blank line after a function counts before code of another kind.
  (hel-test-with #'python-ts-mode (hel-test-mark "def f():\n    pass\n\nx = 1\n" "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection) "def f():\n    pass\n\n")))
  ;; The blank line before a closing bracket does not.
  (when (treesit-language-available-p 'javascript)
    (hel-test-with #'js-ts-mode (hel-test-mark "class A {\n  f() {}\n\n  g() {}\n\n}\n" "g()")
      (skip-unless (hel-treesit-text-objects-p))
      (hel-mark-around-function 1)
      (should (equal (hel-test-selection) "\n  g() {}\n"))))
  ;; The blanks before a comment after code count even after a bracket.
  (hel-test-with #'c-mode (hel-test-mark "int f() {  // note\n}\n" "note")
    (hel-mark-around-comment 1)
    (should (equal (hel-test-selection) "  // note"))))

;; Section "Selecting an object": "The parent of a sentence is its
;; paragraph, so 3 mis on the last sentence of a paragraph selects only that
;; sentence." "When fewer objects exist, the driver selects the ones that
;; exist."
(ert-deftest hel-to-sentence-count-stops-at-paragraph ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Five")
    (setq-local sentence-end-double-space nil)
    (hel-mark-sentence 3)
    (should (equal (hel-test-selection) "Five six."))
    (hel-test-goto "Seven")
    (hel-mark-sentence 3)
    (should (equal (hel-test-selection) "Seven eight. Nine ten."))))

;; Section "The ranges of an object": "A paragraph covers whole lines ...
;; So mip and d leave no empty line behind." `map' adds the blank lines
;; after the paragraph.
(ert-deftest hel-to-paragraph-delete ()
  (hel-test-with #'text-mode (hel-test-mark "A.\n\nB b.\nb2.\n\nC.\n" "b2")
    (hel-mark-paragraph 1)
    (should (equal (hel-test-selection) "B b.\nb2.\n"))
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string) "A.\n\n\nC.\n")))
  (hel-test-with #'text-mode (hel-test-mark "A.\n\nB b.\nb2.\n\nC.\n" "b2")
    (hel-mark-around-paragraph 1)
    (should (equal (hel-test-selection) "B b.\nb2.\n\n"))
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string) "A.\n\nC.\n"))))

;; Section "Selecting an object": "A cursor in a gap finds the object that
;; the gap belongs to", and "when two around ranges contain the cursor, the
;; object before the cursor wins". Section "Several objects": N objects run
;; from the start of the first to the end of the last, with everything
;; between them.
(ert-deftest hel-to-paragraph-in-gap-and-count ()
  (hel-test-with #'text-mode (hel-test-mark "A.\n\nB.\n" "A.")
    (hel-to--goto-blank-line "B.")
    (hel-mark-paragraph 1)
    (should (equal (hel-test-selection) "A.\n"))
    (hel-test-goto "A.")
    (hel-mark-paragraph 2)
    (should (equal (hel-test-selection) "A.\n\nB.\n"))))

;;; Sources and the definition of an object

;; Section "Text objects and motions" and "Choosing a producer": the
;; function object lists its query first and the motion second. Without a
;; parser, the query source returns nil and the motion source applies.
(ert-deftest hel-to-sources-without-parser ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "1)")
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
    ;; The class object has only a query, and the syntax source of the
    ;; argument object does not apply in Lisp.
    (should (= 1 (length (get 'class 'hel-thing-sources))))
    (dolist (object '(class argument))
      (should-not (-some #'funcall (get object 'hel-thing-sources))))))

;; Section "Choosing a producer": the source of a Helix query applies when
;; `hel-treesit-text-objects-p' returns non-nil at the cursor.
(ert-deftest hel-to-sources-with-parser ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (dolist (object '(function class argument comment))
      (should (funcall (car (get object 'hel-thing-sources)))))))

;; Section "Text objects and motions": word, WORD, sentence and paragraph
;; each have one motion source, and it applies in every buffer.
(ert-deftest hel-to-sources-of-prose ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
    (setq-local sentence-end-double-space nil)
    (dolist (object '(word WORD sentence paragraph))
      (should (hel-thing-producer object)))))

;; Section "Sources and the definition of an object": "A user adds a source
;; with (push SOURCE (get 'function 'hel-thing-sources))." "The first source
;; that returns a producer wins" (section "Choosing a producer").
(ert-deftest hel-to-pushed-source-wins ()
  (let ((sources (get 'function 'hel-thing-sources)))
    (unwind-protect
        (progn
          (push (hel-motion-source #'hel-sentence-motion)
                (get 'function 'hel-thing-sources))
          (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
            (setq-local sentence-end-double-space nil)
            (hel-mark-function 1)
            (should (equal (hel-test-selection) "Three four."))))
      (put 'function 'hel-thing-sources sources))))

;; Section "Choosing a producer": a source returns nil when it does not
;; apply, and the next source is tried.
(ert-deftest hel-to-nil-source-is-skipped ()
  (let ((sources (get 'word 'hel-thing-sources)))
    (unwind-protect
        (progn
          (push (lambda () nil) (get 'word 'hel-thing-sources))
          (hel-test-with #'text-mode (hel-test-mark "foo bar baz\n" "bar")
            (hel-mark-around-word 1)
            (should (equal (hel-test-selection) "bar "))))
      (put 'word 'hel-thing-sources sources))))

;;; The laws of the protocol

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; sentence motion. The parent of a sentence is its paragraph (section
;; "The models"), so the last sentence of a paragraph has no forward
;; sibling.
(ert-deftest hel-to-laws-sentence-motion ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "One")
    (setq-local sentence-end-double-space nil)
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

;; Section "The ranges of an object": the gap of a sentence is the blanks
;; on its line, and a line end is a border. Section "The finding operations
;; have two levels": `hel-thing-after-gap' returns the position after the
;; gap from POS in direction DIR, or nil when no gap separates sentences.
(ert-deftest hel-to-after-gap-sentence ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "One")
    (setq-local sentence-end-double-space nil)
    (let ((thing 'sentence)
          (producer (hel-to--producer 'sentence))
          (end-of-one (progn (search-forward "two.") (point)))
          (three (progn (hel-test-goto "Three") (point)))
          (end-of-three (progn (search-forward "four.") (point))))
      (should (equal (hel-thing-after-gap thing producer end-of-one 1) three))
      (should (equal (hel-thing-after-gap thing producer three -1) end-of-one))
      (should-not (hel-thing-after-gap thing producer end-of-three 1)))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; Python query, for functions.
(ert-deftest hel-to-laws-treesit-function ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python-laws "return g")
    (skip-unless (hel-treesit-text-objects-p))
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
      (should (equal (-some-> (hel-thing-sibling thing producer top 1) hel-to--text string-trim)
                     "@dec\ndef last(y, z):\n    def inner():\n        pass")))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; Python query, for arguments.
(ert-deftest hel-to-laws-treesit-argument ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python-laws "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (hel-to--check-laws 'argument)))

;; Section "The finding operations have two levels": `hel-thing-next' from the end of a top-level
;; function finds the method of the class after it, while the sibling of
;; the function is the second top-level function.
(ert-deftest hel-to-treesit-sibling-skips-nested ()
  (hel-test-with #'python-ts-mode (hel-test-mark "def a():\n    pass\n\nclass K:\n    def m(self):\n        pass\n\ndef b():\n    pass\n" "pass")
    (skip-unless (hel-treesit-text-objects-p))
    (let* ((thing 'function)
           (producer (hel-to--producer thing))
           (objects (hel-to--known-objects thing producer))
           (a (hel-to--object-with-text objects "def a():\n    pass")))
      (should a)
      (should (equal (-some-> (hel-thing-next thing producer (hel-thing-instance-end a) 1)
                       hel-to--text string-trim)
                     "def m(self):\n        pass"))
      (should (equal (-some-> (hel-thing-sibling thing producer a 1) hel-to--text string-trim)
                     "def b():\n    pass")))))

;; Section "The finding operations have two levels" and "Tests": the laws of the protocol on the
;; comments of the syntax table. A comment of whole lines covers them, and
;; a comment after code covers only itself.
(cl-defstruct (hel-test-list-comment-producer
               (:include hel-comment-producer)
               (:constructor hel-test-list-comment-producer-create)
               (:copier nil))
  "A comment producer that reads the comment pieces from a list of ranges."
  ranges)

(cl-defmethod hel--comment-piece-under ((producer hel-test-list-comment-producer)
                                        pos)
  (-when-let ((begin . end)
              (-first (-lambda ((begin . end)) (and (<= begin pos) (< pos end)))
                      (hel-test-list-comment-producer-ranges producer)))
    (cons begin end)))

(cl-defmethod hel--comment-piece-next ((producer hel-test-list-comment-producer)
                                       pos dir)
  (let* ((ranges (hel-test-list-comment-producer-ranges producer))
         (found (if (< 0 dir)
                    (-first (lambda (range) (<= pos (car range))) ranges)
                  (-last (lambda (range) (<= (cdr range) pos)) ranges))))
    (when found
      (hel--comment-piece-under producer (car found)))))

(ert-deftest hel-comments-need-only-the-two-piece-operations ()
  "A third model of the comment producer gets the comments for nothing."
  (hel-test-with #'fundamental-mode "# a\n# b\n\n# c\n|"
    (let ((producer (hel-test-list-comment-producer-create
                     :ranges '((1 . 4) (5 . 8) (10 . 13)))))
      (should (equal (hel--comment-run-at producer 2) '(1 . 8)))
      (should (equal (hel-thing-instance-bounds
                      (hel-thing-next 'comment producer 1 1))
                     '(10 . 14)))
      (should (equal (hel-thing-instance-bounds
                      (hel-thing-next 'comment producer 12 -1))
                     '(1 . 9))))))


(ert-deftest hel-comment-piece-under-at-start-of-next-piece ()
  "The piece under the start of a piece that `hel--comment-piece-next'
returns is that same piece, on all three models. `hel-thing-next' builds
the comment from that start."
  (cl-flet ((check (producer)
              (cl-loop for pos from (point-min) to (point-max)
                       do (dolist (dir '(1 -1))
                            (when-let* ((piece (hel--comment-piece-next
                                                producer pos dir)))
                              (should (equal (hel--comment-piece-under
                                              producer (car piece))
                                             piece)))))))
    (let ((text "int a; // one\n// two\n/* three\n   four */\nint b; /* five */ // six\n|"))
      (hel-test-with #'c-mode text
        (check (hel-syntax-comment-source)))
      (when (treesit-language-available-p 'c)
        (hel-test-with #'c-ts-mode text
          (when (hel-treesit-text-objects-p)
            (check (hel-treesit-comment-producer--create))))))
    (hel-test-with #'fundamental-mode "# a\n# b\n\n# c\n|"
      (check (hel-test-list-comment-producer-create
              :ranges '((1 . 4) (5 . 8) (10 . 13)))))))
(ert-deftest hel-to-laws-syntax-comments ()
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp-comments "(x)")
    (let* ((thing 'comment)
           (producer (hel-to--producer thing))
           (objects (hel-to--check-laws thing)))
      (should (equal (-map #'hel-to--text
                           (-sort (lambda (a b)
                                    (< (hel-thing-instance-begin a)
                                       (hel-thing-instance-begin b)))
                                  objects))
                     '(";; a\n;; b\n" "; c" ";; d\n")))
      ;; Away from the start of a line, the gap stays on its line, so it
      ;; never joins a line of code to the next one.
      (dolist (pos (number-sequence (point-min) (point-max)))
        (unless (save-excursion (goto-char pos) (bolp))
          (dolist (dir '(1 -1))
            (let ((gap-end (or (hel-thing-after-gap thing producer pos dir)
                               pos)))
              (should-not (string-search
                           "\n" (buffer-substring-no-properties
                                 (min pos gap-end) (max pos gap-end)))))))))))

;; Section "The ranges of an object", the table of objects: word, WORD,
;; sentence, paragraph and comment take no header comment. The function
;; motion takes one (section "Things and sources").
(ert-deftest hel-to-header-p-without-parser ()
  (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
    (setq-local sentence-end-double-space nil)
    (dolist (object '(word WORD sentence paragraph))
      (should-not (hel-thing-owns-header-comment-p object))))
  (hel-test-with #'emacs-lisp-mode (hel-test-mark hel-to--elisp "1)")
    (should (hel-thing-owns-header-comment-p 'function))
    (should-not (hel-thing-owns-header-comment-p 'comment))))

;; Section "The ranges of an object", the table of objects: function, class
;; and argument have a header comment, and comment has none.
(ert-deftest hel-to-header-p-treesit ()
  (hel-test-with #'python-ts-mode (hel-test-mark hel-to--python "return g")
    (skip-unless (hel-treesit-text-objects-p))
    (dolist (object '(function class argument))
      (should (hel-thing-owns-header-comment-p object)))
    (should-not (hel-thing-owns-header-comment-p 'comment))))

;; Section "Five ways to extend": a new thing needs only its sources, and
;; its commands call the algorithms.
(ert-deftest hel-to-new-thing ()
  (unwind-protect
      (progn
        (put 'hel-to-probe 'hel-thing-sources
             (list (hel-motion-source #'hel-sentence-motion)))
        (hel-test-with #'text-mode (hel-test-mark hel-to--sentences "Three")
          (setq-local sentence-end-double-space nil)
          (hel-mark-text-object 'hel-to-probe :inner 1)
          (should (equal (hel-test-selection) "Three four."))
          (hel-test-goto "four")
          (hel-mark-to-thing-edge 'hel-to-probe 1 :end t)
          (should (equal (hel-test-selection) "four."))))
    (put 'hel-to-probe 'hel-thing-sources nil)))

;;; Things and sources

(ert-deftest hel-thing-sources ()
  "Section Things and sources: function and comment have more than one
source.  Section Choosing a producer: class has only a query, and argument
has a query and the syntax table.  An extension may add a source of WORD,
as hel-paredit does, so its count is not checked."
  (should (= 2 (length (get 'function 'hel-thing-sources))))
  (should (= 2 (length (get 'comment 'hel-thing-sources))))
  (should (= 2 (length (get 'argument 'hel-thing-sources))))
  (should (= 2 (length (default-value 'hel-node-source-functions))))
  (dolist (thing '(word sentence paragraph class))
    (should (= 1 (length (get thing 'hel-thing-sources))))))

(ert-deftest hel-class-has-no-source-in-text-mode ()
  "Section Choosing a producer: when no source applies, the class command
leaves the selection as it is."
  (hel-test-with #'text-mode "foo |bar\n"
    (should-not (hel-to--producer 'class))
    (hel-test-run #'hel-mark-class)
    (should (equal (hel-test-render) "foo |bar\n"))
    (should (null mark-ring))))

(ert-deftest hel-treesit-text-objects-p ()
  "Section Choosing a producer: `hel-treesit-text-objects-p' is non-nil
in a buffer with a parser and a query file on Emacs 31, and nil in
text-mode."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "x = |1\n"
    (should (hel-treesit-text-objects-p)))
  (hel-test-with #'text-mode "x = |1\n"
    (should-not (hel-treesit-text-objects-p))))

(ert-deftest hel-thing-rules ()
  "Section Two questions, two arguments: owning the header comment and
covering whole lines are rules of the thing (table of section The ranges
of an object)."
  (should (hel-thing-owns-header-comment-p 'function))
  (should (hel-thing-owns-header-comment-p 'class))
  (should (hel-thing-owns-header-comment-p 'argument))
  (should-not (hel-thing-owns-header-comment-p 'word))
  (should-not (hel-thing-owns-header-comment-p 'sentence))
  (should-not (hel-thing-owns-header-comment-p 'comment))
  (with-temp-buffer
    (insert "foo\n  bar  \nbaz")
    ;; `bar' is the only text on its line.
    (should (equal '(5 . 13)
                   (hel-thing-maybe-expand-bounds-to-full-lines
                    'paragraph '(7 . 10))))
    (should (equal '(7 . 10)
                   (hel-thing-maybe-expand-bounds-to-full-lines
                    'word '(7 . 10)))))
  (should (= -1 (hel-thing-selection-direction 'function)))
  (should (= 1 (hel-thing-selection-direction 'word))))

;;; Laws of the protocol

(defun hel-to--after-gap-idempotent (thing pos)
  "Check that a gap of THING lies next to POS, and that none follows its end."
  (let ((producer (hel-to--producer thing))
        (gaps 0))
    (should producer)
    (dolist (dir '(1 -1))
      (when-let* ((once (hel-thing-after-gap thing producer pos dir)))
        (cl-incf gaps)
        (should-not (hel-thing-after-gap thing producer once dir))))
    (should (< 0 gaps))))

(ert-deftest hel-law-after-gap-sentence ()
  "Tests of the protocol: a second after-gap from its own result returns
that result (the sentence motion)."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode "One two.   Three four.\n"
      (hel-to--after-gap-idempotent 'sentence 9))))

(ert-deftest hel-law-after-gap-python-query ()
  "Tests of the protocol: after-gap idempotence on the Python query."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a,   b)\n"
    (goto-char 4)
    (hel-to--after-gap-idempotent 'argument 4)))

(ert-deftest hel-law-after-gap-comments ()
  "Tests of the protocol: after-gap idempotence on the comments of the
syntax table."
  (hel-test-with #'emacs-lisp-mode ";; a\n;; b\n\n\n;; c\n(foo)\n"
    (hel-to--after-gap-idempotent 'comment 12)))

(ert-deftest hel-law-comment-gap-on-line ()
  "Tests of the protocol: away from the start of a line, the gap of a
comment holds no line end."
  (hel-test-with #'emacs-lisp-mode "(foo)  ; note\n(bar)\n"
    (let* ((producer (hel-to--producer 'comment))
           (end 14)                     ; after "; note"
           (start 8))                   ; before "; note"
      (should producer)
      (let ((after (or (hel-thing-after-gap 'comment producer end 1) end))
            (before (or (hel-thing-after-gap 'comment producer start -1) start)))
        (should-not (string-search "\n" (buffer-substring end after)))
        (should-not (string-search "\n" (buffer-substring before start)))))))

;;; Python text objects

(defconst hel-to--class
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

(defun hel-to--class-at (needle marker &optional needle2 marker2)
  "Return `hel-to--class' with MARKER before NEEDLE.
With NEEDLE2, also put MARKER2 before it.  NEEDLE2 must come after NEEDLE."
  (let* ((s hel-to--class)
         (i (string-search needle s))
         (j (and needle2 (string-search needle2 s))))
    (if j
        (concat (substring s 0 i) marker (substring s i j) marker2
                (substring s j))
      (concat (substring s 0 i) marker (substring s i)))))

(defun hel-to--class-pos (needle)
  "Return the buffer position of NEEDLE in `hel-to--class'."
  (1+ (string-search needle hel-to--class)))

;;; Whole lines, buffer edges, indentation and negative counts

(ert-deftest hel-mif-d-leaves-no-line ()
  "`mif' and `d' on an indented method leave no indentation and no line."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode
      "class A:\n    def f(self):\n        |pass\n\n    def g(self):\n        pass\n"
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-selection) "    def f(self):\n        pass\n"))))

(ert-deftest hel-mit-whole-lines ()
  "`mit' selects the whole lines of a class."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "class A:\n    |x = 1\n\ny = 2\n"
    (hel-test-run #'hel-mark-class)
    (should (equal (hel-test-selection) "class A:\n    x = 1\n"))))

(ert-deftest hel-mif-shares-line ()
  "A function that shares its line with other code covers only itself."
  (hel-test-need-queries 'c)
  (hel-test-with #'c-ts-mode
      "int a(void) { |return 1; } int b(void) { return 2; }\n"
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-selection) "int a(void) { return 1; }"))))

(ert-deftest hel-brace-last-paragraph ()
  "`}' with no next paragraph selects to the end of the buffer."
  (pcase-dolist (`(,input ,expected)
                 '(("aa\n\n|bb\n" "aa\n\n^bb\n|")
                   ("aa\n\nb|b" "aa\n\nb^b|")
                   ("|aa\n" "^aa\n|")
                   ("aa\n\nb|b\n\n" "aa\n\nb^b\n\n|")))
    (hel-test-with #'text-mode input
      (hel-test-run #'hel-forward-paragraph)
      (should (equal (hel-test-render) expected))))
  (hel-test-with #'text-mode "|aa\n\ncc\n"
    (hel-test-run #'hel-forward-paragraph 3)
    (should (equal (hel-test-render) "^aa\n\ncc\n|"))))

(ert-deftest hel-brace-first-paragraph ()
  "`{' with no previous paragraph selects to the start of the buffer."
  (hel-test-with #'text-mode "\n\n|aa\nbb\n"
    (hel-test-run #'hel-backward-paragraph)
    (should (equal (hel-test-render) "|\n\n^aa\nbb\n"))))

(ert-deftest hel-brace-buffer-edge-message ()
  "`}' and `{' at the buffer edge keep the selection and show an unlogged message."
  (pcase-dolist (`(,command ,input ,text)
                 `((hel-forward-paragraph "aa\n|" "End of buffer")
                   (hel-backward-paragraph "|aa\n" "Beginning of buffer")))
    (hel-test-with #'text-mode input
      (let (shown)
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (cons (apply #'format format-string args) message-log-max)
                           shown))))
          (hel-test-run command))
        (should (equal (assoc text shown) (cons text nil)))
        (should (equal (hel-test-render) input))))))

(ert-deftest hel-miw-on-indentation ()
  "On the indentation of a line, `miw' selects nothing."
  (dolist (input '("foo\n|  bar baz\n" "foo\n |  bar baz\n" "|  bar baz\n"))
    (hel-test-with #'text-mode input
      (hel-test-run #'hel-mark-word)
      (should (equal (hel-test-render) input)))))

(ert-deftest hel-miw-on-blanks-in-line ()
  "On the blanks between two words, `miw' selects the word before them."
  (hel-test-with #'text-mode "foo |  bar"
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "^foo|   bar"))))

(ert-deftest hel-negative-count-point-at-start ()
  "A negative count puts point at the start for every text object."
  (hel-test-with #'text-mode "foo b|ar baz qux"
    (hel-test-run #'hel-mark-word -2)
    (should (equal (hel-test-render) "|foo bar^ baz qux")))
  (hel-test-with #'text-mode "aa\n\nb|b\n\ncc\n"
    (hel-test-run #'hel-mark-paragraph -1)
    (should (equal (hel-test-render) "aa\n\n|bb\n^\ncc\n"))))

(ert-deftest hel-side-keys-are-unbound ()
  "`H' and `L' have no binding in Normal state."
  (dolist (key '("H" "L"))
    (should-not (keymap-lookup hel-normal-state-map key))))

(ert-deftest hel-mif-method ()
  "Mif selects the whole method with its def line, point at the
start (sections One rule for every object, rule 7)."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "|" "\n    def m2" "^")))))

(ert-deftest hel-maf-method ()
  "Maf selects the method and the blank lines after it, with point
at the start.  The gap runs up to the next object."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "|" "    def m2" "^")))))

(ert-deftest hel-maf-gap-ends-at-section-comment ()
  "A section comment before the next function ends the gap after, so
maf takes the blank line after the function and not the one before it."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ())\n\n(defun b ()\n  |1)\n\n;;; Head\n\n(defun c ())\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "(defun a ())\n\n|(defun b ()\n  1)\n\n^;;; Head\n\n(defun c ())\n"))))

(ert-deftest hel-maf-between-two-section-comments ()
  "A function with section comments on both sides takes the gap after."
  (hel-test-with #'emacs-lisp-mode
      ";;; One\n\n(defun b ()\n  |1)\n\n;;; Two\n\n(defun c ())\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   ";;; One\n\n|(defun b ()\n  1)\n\n^;;; Two\n\n(defun c ())\n"))))

(ert-deftest hel-maf-leaves-indentation-of-section-comment ()
  "The gap after stops at the line of an indented comment, before its
indentation."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode
      "class A:\n    def m(self):\n        |pass\n\n    # Section\n\n    def n(self):\n        pass\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "class A:\n|    def m(self):\n        pass\n\n^    # Section\n\n    def n(self):\n        pass\n"))))

(ert-deftest hel-maf-comment-on-next-line-is-empty-gap ()
  "A comment on the line after the function leaves an empty gap, so
maf takes the gap before the function."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ())\n\n(defun b ()\n  |1)\n;;; Head\n\n(defun c ())\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "(defun a ())\n|\n(defun b ()\n  1)\n^;;; Head\n\n(defun c ())\n"))))

(ert-deftest hel-mic-from-indentation ()
  "Mic on the indentation of a comment line selects the comment."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  ;; before\n  (g)\n    ;; one\n  |  ;; two\n    ;; three\n    a)\n"
    (hel-test-run #'hel-mark-comment)
    (should (equal (hel-test-render)
                   "(defun f ()\n  ;; before\n  (g)\n|    ;; one\n    ;; two\n    ;; three\n^    a)\n"))))

(ert-deftest hel-backward-comment-from-indentation ()
  "[c on the indentation stops at the start of the first line of the comment."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  ;; before\n  (g)\n    ;; one\n  |  ;; two\n    ;; three\n    a)\n"
    (hel-test-run #'hel-mark-comment-backward)
    (should (equal (hel-test-render)
                   "(defun f ()\n  ;; before\n  (g)\n|    ;; one\n  ^  ;; two\n    ;; three\n    a)\n"))))

(ert-deftest hel-backward-comment-from-first-delimiter-of-a-line ()
  "[c on the delimiter of a later line stays in the comment."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  ;; before\n  (g)\n    ;; one\n    |;; two\n    ;; three\n    a)\n"
    (hel-test-run #'hel-mark-comment-backward)
    (should (equal (hel-test-render)
                   "(defun f ()\n  ;; before\n  (g)\n|    ;; one\n    ^;; two\n    ;; three\n    a)\n"))))

(ert-deftest hel-backward-comment-from-first-delimiter ()
  "[c on the first delimiter of a comment stops at the start of its line."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  ;; before\n  (g)\n    |;; one\n    ;; two\n    a)\n"
    (hel-test-run #'hel-mark-comment-backward)
    (should (equal (hel-test-render)
                   "(defun f ()\n  ;; before\n  (g)\n|    ^;; one\n    ;; two\n    a)\n"))))

(ert-deftest hel-backward-comment-at-start-goes-to-previous ()
  "[c at the start of a comment selects the previous comment."
  (hel-test-with #'emacs-lisp-mode
      "(defun f ()\n  ;; before\n  (g)\n|    ;; one\n    ;; two\n    a)\n"
    (hel-test-run #'hel-mark-comment-backward)
    (should (equal (hel-test-render)
                   "(defun f ()\n|  ;; before\n^  (g)\n    ;; one\n    ;; two\n    a)\n"))))

(ert-deftest hel-mif-def-line ()
  "Mif on the def line selects that function."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "f m1" "|")
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "|" "\n    def m2" "^")))))

(ert-deftest hel-2mif-two-methods ()
  "`2 mif' on the first method selects both methods."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-function 2)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "^" "\n\ndef top" "|")))))

(ert-deftest hel-2maf-last-method ()
  "`2 maf' on the last method selects only that method: the
objects stay inside the parent of the first one."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "2\n" "|")
    (hel-test-run #'hel-mark-around-function 2)
    (should (use-region-p))
    (should (string-search "def m2(self):\n        return 2" (hel-test-selection)))
    (should (<= (hel-to--class-pos "\n\n    def m2") (region-beginning)))
    (should (<= (region-end) (hel-to--class-pos "def top")))))

(ert-deftest hel-maf-repeated ()
  "A second maf on the first method selects both methods, and a
third one adds nothing, because the class holds no more methods."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-around-function)
    (hel-test-run #'hel-mark-around-function)
    (should (= (region-beginning) (hel-to--class-pos "    def m1")))
    (should (string-search "return 2" (hel-test-selection)))
    (should (<= (region-end) (hel-to--class-pos "def top")))
    (let ((second (hel-test-render)))
      (hel-test-run #'hel-mark-around-function)
      (should (equal (hel-test-render) second)))))

(ert-deftest hel-maf-repeated-then-shrink ()
  "M-i after the second maf returns to the first method."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-around-function)
    (let ((first (hel-test-render)))
      (hel-test-run #'hel-mark-around-function)
      (hel-test-run #'hel-shrink-selection)
      (should (equal (hel-test-render) first)))))

(ert-deftest hel-maf-then-shrink ()
  "M-i after maf returns to the selection before it (rule 3)."
  (hel-test-need 'python)
  (let ((text (hel-to--class-at "a\n\n" "|")))
    (hel-test-with #'python-ts-mode text
      (hel-test-run #'hel-mark-around-function)
      (hel-test-run #'hel-shrink-selection)
      (should-not (use-region-p))
      (should (equal (hel-test-render) text)))))

(ert-deftest hel-maf-pushes-point ()
  "Section Selecting an object: the driver pushes the old point to the
mark ring."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "a\n\n" "|")
    (hel-test-run #'hel-mark-around-function)
    (should (= 1 (length mark-ring)))))

(defconst hel-to--headers
  (concat "x = 0\n\n\n"
          "# Header of f.\n"
          "def f():\n"
          "    return 1\n"
          "\n\n"
          "# Header of g.\n"
          "def g():\n"
          "    return 2\n")
  "Two Python functions with header comments.")

(defun hel-to--headers-at (&rest pairs)
  "Return `hel-to--headers' with markers inserted.
PAIRS is a list of (NEEDLE MARKER OFFSET) in buffer order.  OFFSET is
added to the index of NEEDLE."
  (let ((s hel-to--headers)
        (shift 0))
    (pcase-dolist (`(,needle ,marker ,offset) pairs)
      (let ((i (+ (string-search needle hel-to--headers) (or offset 0) shift)))
        (setq s (concat (substring s 0 i) marker (substring s i)))
        (cl-incf shift (length marker))))
    s))

(ert-deftest hel-maf-header-comment ()
  "Maf on a function with a comment directly above it takes that
header comment, and leaves out the header comment of the next function."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--headers-at '("1\n" "|" 0))
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   (hel-to--headers-at '("# Header of f" "|" 0)
                                       '("# Header of g" "^" 0))))))

(ert-deftest hel-maf-header-comment-indented ()
  "The header comment of an indented method starts at its line start, so
deleting the method leaves the next method with its own indentation. The
header comment of an argument starts at its text."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "class A:\n    def e(self):\n        pass\n\n    # Header of f.\n    def f(self):\n        pa|ss\n\n    def h(self):\n        pass\n"
    (hel-mark-around-function 1)
    (should (equal (hel-test-selection)
                   "    # Header of f.\n    def f(self):\n        pass\n\n"))
    (delete-region (region-beginning) (region-end))
    (should (equal (buffer-string)
                   "class A:\n    def e(self):\n        pass\n\n    def h(self):\n        pass\n")))
  (hel-test-with #'python-ts-mode "f(\n    # note\n    |a,\n    b)\n"
    (hel-mark-around-argument 1)
    (should (equal (hel-test-selection) "# note\n    a,\n    "))))

(ert-deftest hel-maf-header-comment-embedded ()
  "Maf takes the header comment that the parser of an embedded
language finds, although the syntax table of the buffer does not see it."
  (hel-test-need-queries 'html 'javascript)
  (hel-test-with #'mhtml-ts-mode
      (concat "<script>\n// Header of f.\nfunction f() {\n  return |1;\n}\n\n"
              "// Header of g.\nfunction g() {\n  return 2;\n}\n</script>\n")
    (treesit-update-ranges)
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   (concat "<script>\n|// Header of f.\nfunction f() {\n  return 1;\n}\n\n"
                           "^// Header of g.\nfunction g() {\n  return 2;\n}\n</script>\n")))))

(ert-deftest hel-mif-on-header-comment-embedded ()
  "Mif on a header comment of an embedded language selects the
function under it, although the syntax table of the buffer does not see the
comment."
  (hel-test-need-queries 'html 'javascript)
  (hel-test-with #'mhtml-ts-mode
      (concat "<script>\n// Header |of g.\nfunction g() {\n  return 2;\n}\n"
              "</script>\n")
    (treesit-update-ranges)
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (concat "<script>\n// Header of g.\n|function g() {\n"
                           "  return 2;\n}\n^</script>\n")))))

(ert-deftest hel-mif-on-header-comment ()
  "Mif on a header comment selects the function under it."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--headers-at '("Header of f" "|" 3))
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (hel-to--headers-at '("def f" "|" 0)
                                       '("1\n" "^" 2))))))

(ert-deftest hel-mic-on-header-comment ()
  "Mic on the line of a header comment selects the comment line
with its line end."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--headers-at '("Header of f" "|" 3))
    (hel-test-run #'hel-mark-comment)
    (should (equal (hel-test-render)
                   (hel-to--headers-at '("# Header of f" "|" 0)
                                       '("def f" "^" 0))))))

(ert-deftest hel-mif-on-blank-line ()
  "Mif on a blank line between two functions selects the function
above it."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--headers-at '("1\n\n" "|" 3))
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (hel-to--headers-at '("def f" "|" 0)
                                       '("1\n" "^" 2))))))

(ert-deftest hel-maf-decorated-function ()
  "Maf on a decorated function selects its header comment, its
decorators and the function (section The object; Shadowing patterns)."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode
      "# Header of f.\n@deco\ndef f():\n    pa|ss\n\n\ndef g():\n    pass\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "|# Header of f.\n@deco\ndef f():\n    pass\n\n\n^def g():\n    pass\n"))))

(ert-deftest hel-mat-decorated-class ()
  "Mat on a decorated class selects its header comment, its
decorators and the class."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode
      "# Header of K.\n@deco\nclass K:\n    pa|ss\n\n\nclass L:\n    pass\n"
    (hel-test-run #'hel-mark-around-class)
    (should (equal (hel-test-render)
                   "|# Header of K.\n@deco\nclass K:\n    pass\n\n\n^class L:\n    pass\n"))))

(ert-deftest hel-nested-function ()
  "A nested function that ends where its outer function ends stays
a separate object."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode
      "def outer():\n    def inner():\n        return |1\n"
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   "def outer():\n|    def inner():\n        return 1\n^"))))

(ert-deftest hel-nested-function-decorated-outer ()
  "The nested function stays separate also when the outer function
is decorated (the replacement compares nodes, not positions)."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode
      "@deco\ndef outer():\n    def inner():\n        return |1\n"
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   "@deco\ndef outer():\n|    def inner():\n        return 1\n^"))))

;;; Arguments

(ert-deftest hel-mia-on-comma ()
  "On the comma in f(a, b), mia selects a (a cursor in a gap finds
the object that the gap belongs to)."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a|, b)\n"
    (hel-test-run #'hel-mark-argument)
    (should (equal (hel-test-render) "f(^a|, b)\n"))))

(ert-deftest hel-mia-on-comma-after-b ()
  "On the comma after b in f(a, b, c), mia selects b (when two
around ranges contain the cursor, the object before it wins)."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a, b|, c)\n"
    (hel-test-run #'hel-mark-argument)
    (should (equal (hel-test-render) "f(a, ^b|, c)\n"))))

(ert-deftest hel-mia-nothing-here ()
  "Mia with no argument around the cursor leaves the selection and
the mark ring as they are."
  (hel-test-need-queries 'python)
  (should (hel-test-unchanged-p #'hel-mark-argument "|x = 1\n"
                                #'python-ts-mode)))

(ert-deftest hel-maa-on-self-pushes-once ()
  "Maa on self pushes point once."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "def m(se|lf, a):\n    pass\n"
    (hel-test-run #'hel-mark-around-argument)
    (should (equal (hel-test-render) "def m(^self, |a):\n    pass\n"))
    (should (= 1 (length mark-ring)))))

(ert-deftest hel-maa-first ()
  "Tests and section The around range: maa on a selects a and the gap
`, ' after it."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(|a, b, c)\n"
    (hel-test-run #'hel-mark-around-argument)
    (should (equal (hel-test-render) "f(^a, |b, c)\n"))))

(ert-deftest hel-maa-last ()
  "Maa on the last argument selects the gap before it and the
argument."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a, b, |c)\n"
    (hel-test-run #'hel-mark-around-argument)
    (should (equal (hel-test-render) "f(a, b^, c|)\n"))))

(ert-deftest hel-mia-each-argument ()
  "Mia on any argument selects the argument alone."
  (hel-test-need-queries 'python)
  (dolist (case '(("f(|a, b, c)\n" . "f(^a|, b, c)\n")
                  ("f(a, |b, c)\n" . "f(a, ^b|, c)\n")
                  ("f(a, b, |c)\n" . "f(a, b, ^c|)\n")))
    (hel-test-with #'python-ts-mode (car case)
      (hel-test-run #'hel-mark-argument)
      (should (equal (hel-test-render) (cdr case))))))

(ert-deftest hel-maa-delete-first-and-last ()
  "Maa and d on the first and on the last argument leave a list
without a stray comma."
  (hel-test-need-queries 'python)
  (dolist (case '(("f(|a, b, c)\n" . "f(b, c)\n")
                  ("f(a, b, |c)\n" . "f(a, b)\n")))
    (hel-test-with #'python-ts-mode (car case)
      (hel-test-run #'hel-mark-around-argument)
      (delete-region (region-beginning) (region-end))
      (should (equal (buffer-string) (cdr case))))))

(ert-deftest hel-2mia ()
  "On a in f(a, b, c), `2 mia' selects `a, b'."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(|a, b, c)\n"
    (hel-test-run #'hel-mark-argument 2)
    (should (equal (hel-test-render) "f(^a, b|, c)\n"))))

(ert-deftest hel-2maa ()
  "On a in f(a, b, c), `2 maa' selects `a, b' and the gap after b."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(|a, b, c)\n"
    (hel-test-run #'hel-mark-around-argument 2)
    (should (equal (hel-test-render) "f(^a, b, |c)\n"))))

(ert-deftest hel-negative-2mia ()
  "On b in f(a, b, c), `-2 mia' selects `a, b' with point at the start."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a, |b, c)\n"
    (hel-test-run #'hel-mark-argument -2)
    (should (equal (hel-test-render) "f(|a, b^, c)\n"))))

(ert-deftest hel-mie-each-entry ()
  "Mie selects one element of a list, a key with its value in
a dictionary, and the innermost entry in nested structures."
  (hel-test-need-queries 'python)
  (dolist (case '(("x = [1, |2, 3]\n" . "x = [1, ^2|, 3]\n")
                  ("d = {\"a\": 1, \"b\": |2}\n" . "d = {\"a\": 1, ^\"b\": 2|}\n")
                  ("d = {\"a\": [1, |2]}\n" . "d = {\"a\": [1, ^2|]}\n")))
    (hel-test-with #'python-ts-mode (car case)
      (hel-test-run #'hel-mark-entry)
      (should (equal (hel-test-render) (cdr case))))))

(ert-deftest hel-mae-first-and-last ()
  "Mae takes the separator after an entry, and before the last one."
  (hel-test-need-queries 'python)
  (dolist (case '(("x = [|1, 2, 3]\n" . "x = [^1, |2, 3]\n")
                  ("x = [1, 2, |3]\n" . "x = [1, 2^, 3|]\n")))
    (hel-test-with #'python-ts-mode (car case)
      (hel-test-run #'hel-mark-around-entry)
      (should (equal (hel-test-render) (cdr case))))))

(ert-deftest hel-mae-delete-first-and-last ()
  "Mae and d on the first and on the last entry leave a list without
a stray comma."
  (hel-test-need-queries 'python)
  (dolist (case '(("x = [|1, 2, 3]\n" . "x = [2, 3]\n")
                  ("x = [1, 2, |3]\n" . "x = [1, 2]\n")))
    (hel-test-with #'python-ts-mode (car case)
      (hel-test-run #'hel-mark-around-entry)
      (delete-region (region-beginning) (region-end))
      (should (equal (buffer-string) (cdr case))))))

(ert-deftest hel-2mie ()
  "On 1 in [1, 2, 3], `2 mie' selects `1, 2'."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "x = [|1, 2, 3]\n"
    (hel-test-run #'hel-mark-entry 2)
    (should (equal (hel-test-render) "x = [^1, 2|, 3]\n"))))

(ert-deftest hel-entry-jumps ()
  "]e selects to the end of the entry, then the whole next entry; [e
does the same backward."
  (hel-test-need-queries 'python)
  (dolist (case '((hel-mark-entry-forward "x = [|1, 2]\n" "x = [^1|, 2]\n")
                  (hel-mark-entry-forward "x = [1|, 2]\n" "x = [1, ^2|]\n")
                  (hel-mark-entry-backward "x = [1, 2|]\n" "x = [1, |2^]\n")
                  (hel-mark-entry-backward "x = [1, |2]\n" "x = [|1^, 2]\n")))
    (pcase-let ((`(,command ,input ,expected) case))
      (hel-test-with #'python-ts-mode input
        (hel-test-run command)
        (should (equal (hel-test-render) expected))))))

(ert-deftest hel-mie-nothing ()
  "Mie with no entry around the cursor, or without a parser, leaves the
selection as it is."
  (hel-test-need-queries 'python)
  (should (hel-test-unchanged-p #'hel-mark-entry "|x = 1\n"
                                #'python-ts-mode))
  (should (hel-test-unchanged-p #'hel-mark-entry "(list 1 |2)\n"
                                #'emacs-lisp-mode)))

(ert-deftest hel-count-more-than-exist ()
  "Section Selecting an object: when fewer objects exist, the driver
selects the ones that exist."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "f(a, |b, c)\n"
    (hel-test-run #'hel-mark-argument 5)
    (should (equal (hel-test-render) "f(a, ^b, c|)\n"))))

;;; Comments

(ert-deftest hel-mic-run ()
  "Mic selects both comment lines with their # markers and the
line end."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = 1\n\n\n# one\n# t|wo\n\n# three\n"
    (hel-test-run #'hel-mark-comment)
    (should (equal (hel-test-render) "x = 1\n\n\n|# one\n# two\n^\n# three\n"))))

(ert-deftest hel-mac-run ()
  "Mac adds the blank lines up to the next comment."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = 1\n\n\n# one\n# t|wo\n\n# three\n"
    (hel-test-run #'hel-mark-around-comment)
    (should (equal (hel-test-render) "x = 1\n\n\n|# one\n# two\n\n^# three\n"))))

(ert-deftest hel-mic-after-code ()
  "Section The comment object: a comment after code covers only the
comment."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = 1  # no|te\ny = 2\n"
    (hel-test-run #'hel-mark-comment)
    (should (equal (hel-test-render) "x = 1  |# note^\ny = 2\n"))))

(ert-deftest hel-mac-after-code ()
  "Mac on a comment after code selects only the comment, and its
gap stays on the line: here the spaces before it."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "x = 1  # no|te\ny = 2\n"
    (hel-test-run #'hel-mark-around-comment)
    (should (equal (hel-test-render) "x = 1|  # note^\ny = 2\n"))))

;;; Other languages and embedded languages

(defun hel-to--maf-check (mode text expected)
  "Check that maf in MODE turns TEXT into EXPECTED."
  (hel-test-with mode text
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render) expected))))

(ert-deftest hel-maf-typescript ()
  "Maf works in TypeScript through the inherited ecma file."
  (hel-test-need-queries 'typescript)
  (hel-to--maf-check
   #'typescript-ts-mode
   "function a() {\n  return |1;\n}\n\nfunction b() {\n  return 2;\n}\n"
   "|function a() {\n  return 1;\n}\n\n^function b() {\n  return 2;\n}\n"))

(ert-deftest hel-maf-javascript ()
  "Maf works in JavaScript."
  (hel-test-need-queries 'javascript)
  (hel-to--maf-check
   #'js-ts-mode
   "function a() {\n  return |1;\n}\n\nfunction b() {\n  return 2;\n}\n"
   "|function a() {\n  return 1;\n}\n\n^function b() {\n  return 2;\n}\n"))

(ert-deftest hel-maf-rust ()
  "Maf works in Rust."
  (hel-test-need-queries 'rust)
  (hel-to--maf-check
   #'rust-ts-mode
   "fn a() {\n    |1\n}\n\nfn b() {\n    2\n}\n"
   "|fn a() {\n    1\n}\n\n^fn b() {\n    2\n}\n"))

(ert-deftest hel-maf-c ()
  "Maf works in C."
  (hel-test-need-queries 'c)
  (hel-to--maf-check
   #'c-ts-mode
   "int a(void) {\n  return |1;\n}\n\nint b(void) {\n  return 2;\n}\n"
   "|int a(void) {\n  return 1;\n}\n\n^int b(void) {\n  return 2;\n}\n"))

(ert-deftest hel-maf-go ()
  "Maf works in Go."
  (hel-test-need-queries 'go)
  (hel-to--maf-check
   #'go-ts-mode
   "package main\n\nfunc a() int {\n\treturn |1\n}\n\nfunc b() int {\n\treturn 2\n}\n"
   "package main\n\n|func a() int {\n\treturn 1\n}\n\n^func b() int {\n\treturn 2\n}\n"))

(ert-deftest hel-html-script ()
  "In an HTML buffer, a cursor in a <script> block uses the
JavaScript query, and a cursor in the markup uses the HTML query (rule 5,
section Choosing a producer)."
  (hel-test-need-queries 'html 'javascript 'css)
  (skip-unless (fboundp 'mhtml-ts-mode))
  (let ((text (concat "<!-- note -->\n<p>text</p>\n<script>\n"
                      "function foo() { return 1; }\n</script>\n")))
    (hel-test-with #'mhtml-ts-mode
        (replace-regexp-in-string "return" "re|turn" text)
      (treesit-update-ranges)
      (hel-test-run #'hel-mark-function)
      (should (equal (hel-test-selection) "function foo() { return 1; }\n")))
    (hel-test-with #'mhtml-ts-mode
        (replace-regexp-in-string "note" "no|te" text)
      (treesit-update-ranges)
      (hel-test-run #'hel-mark-comment)
      (should (string-prefix-p "<!-- note -->" (hel-test-selection))))))

;;; The ] and [ motions

(ert-deftest hel-forward-function-chain ()
  "]f inside a method selects from point to the end of the method,
the next ]f selects the second method whole, and the next one the
top-level function (section Moving to the edge of an object)."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "urn a" "|")
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   (hel-to--class-at "urn a" "^" "\n    def m2" "|")))
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m2" "^" "\n\ndef top" "|")))
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   (concat "class A:\n"
                           "    def m1(self, a, b):\n"
                           "        return a\n\n"
                           "    def m2(self):\n"
                           "        return 2\n\n\n"
                           "^def top():\n"
                           "    pass\n|")))))

(ert-deftest hel-backward-function-chain ()
  "[f inside a method selects from point to its start, and the next
[f selects the method before it whole."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at " 2\n" "|")
    (hel-test-run #'hel-mark-function-backward)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m2" "|" " 2\n" "^")))
    (hel-test-run #'hel-mark-function-backward)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "|" "\n    def m2" "^")))))

(ert-deftest hel-forward-function-on-class-line ()
  "]f on the class line selects the first method."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode (concat "cl|" (substring hel-to--class 2))
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   (hel-to--class-at "    def m1" "^" "\n    def m2" "|")))))

(ert-deftest hel-forward-function-count ()
  "`3]f' inside the first method reaches the end of the top-level
function, and one more ]f keeps the selection."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode (hel-to--class-at "urn a" "|")
    (hel-test-run #'hel-mark-function-forward 3)
    (let ((expected (concat "class A:\n"
                            "    def m1(self, a, b):\n"
                            "        ret^urn a\n\n"
                            "    def m2(self):\n"
                            "        return 2\n\n\n"
                            "def top():\n"
                            "    pass\n|")))
      (should (equal (hel-test-render) expected))
      (hel-test-run #'hel-mark-function-forward)
      (should (equal (hel-test-render) expected)))))

(ert-deftest hel-forward-function-decorated ()
  "]f at a decorated function selects it with its decorators."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "x = 1|\n\n\n@deco\ndef f():\n    pass\n"
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   "x = 1\n\n\n^@deco\ndef f():\n    pass\n|"))))

(ert-deftest hel-forward-argument-chain ()
  "]a from self selects to the end of self, and the next two ]a
select a and b without their commas."
  (hel-test-need-queries 'python)
  (hel-test-with #'python-ts-mode "def m(|self, a, b):\n    pass\n"
    (hel-test-run #'hel-mark-argument-forward)
    (should (equal (hel-test-render) "def m(^self|, a, b):\n    pass\n"))
    (hel-test-run #'hel-mark-argument-forward)
    (should (equal (hel-test-render) "def m(self, ^a|, b):\n    pass\n"))
    (hel-test-run #'hel-mark-argument-forward)
    (should (equal (hel-test-render) "def m(self, a, ^b|):\n    pass\n"))))

(ert-deftest hel-forward-comment-chain ()
  "In `x = 1  # a' followed by `# b' and `# c', ]c from x selects
`# a', and the next ]c selects the run of `# b' and `# c'."
  (hel-test-need 'python)
  (hel-test-with #'python-ts-mode "|x = 1  # a\n# b\n# c\n"
    (hel-test-run #'hel-mark-comment-forward)
    (should (equal (hel-test-render) "x = 1  ^# a|\n# b\n# c\n"))
    (hel-test-run #'hel-mark-comment-forward)
    (should (equal (hel-test-render) "x = 1  # a\n^# b\n# c\n|"))))

(ert-deftest hel-forward-sentence ()
  "]s inside a sentence selects to its end."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode
        "One two. Th|ree four. Five six.\n\nSeven eight. Nine.\n"
      (hel-test-run #'hel-mark-sentence-forward)
      (should (equal (hel-test-render)
                     "One two. Th^ree four.| Five six.\n\nSeven eight. Nine.\n")))))

(ert-deftest hel-forward-sentence-count-passes-paragraph ()
  "]s with a count passes the end of the paragraph (the motion
ignores parents)."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode
        "One two. Th|ree four. Five six.\n\nSeven eight. Nine.\n"
      (hel-test-run #'hel-mark-sentence-forward 3)
      (should (equal (hel-test-render)
                     "One two. Th^ree four. Five six.\n\nSeven eight.| Nine.\n")))))

(ert-deftest hel-forward-paragraph-brace ()
  "} selects from point to the start of the next paragraph, with
the blank lines before it."
  (hel-test-with #'text-mode "a|aa\nbbb\n\nccc\n"
    (hel-test-run #'hel-forward-paragraph)
    (should (equal (hel-test-render) "a^aa\nbbb\n\n|ccc\n"))))

(ert-deftest hel-backward-paragraph-brace ()
  "Section Moving to the edge of an object: { selects from point to the
start of the paragraph under the cursor, and when point is already there,
to the start of the previous paragraph."
  (hel-test-with #'text-mode "aaa\n\nccc\nd|dd\n"
    (hel-test-run #'hel-backward-paragraph)
    (should (equal (hel-test-render) "aaa\n\n|ccc\nd^dd\n"))
    (hel-test-run #'hel-backward-paragraph)
    (should (equal (hel-test-render) "|aaa\n\n^ccc\nddd\n"))))

;;; Prose objects

(ert-deftest hel-maw-spaces-after ()
  "Maw selects a word with the spaces after it."
  (hel-test-with #'text-mode "foo |bar  baz\n"
    (hel-test-run #'hel-mark-around-word)
    (should (equal (hel-test-render) "foo ^bar  |baz\n"))))

(ert-deftest hel-maw-last-word-of-line ()
  "Maw on the last word of a line takes the spaces before it, not
the line end."
  (hel-test-with #'text-mode "foo  b|ar\nbaz\n"
    (hel-test-run #'hel-mark-around-word)
    (should (equal (hel-test-render) "foo^  bar|\nbaz\n"))))

(ert-deftest hel-maw-before-period ()
  "Maw on a word before a period takes the spaces before it."
  (hel-test-with #'text-mode "foo b|ar.\n"
    (hel-test-run #'hel-mark-around-word)
    (should (equal (hel-test-render) "foo^ bar|.\n"))))

(ert-deftest hel-miw-in-gap ()
  "Section Selecting an object: on the space between two words, miw
selects the word before it."
  (hel-test-with #'text-mode "foo | bar\n"
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "^foo|  bar\n"))))

(ert-deftest hel-miw-bare-point-touches-two ()
  "Section Selecting an object: when a bare point touches two objects, the
object after point wins.  In `foo|.' miw selects the period."
  (hel-test-with #'text-mode "foo|.\n"
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "foo^.|\n"))))

(ert-deftest hel-miw-bare-point-before-space ()
  "Section Selecting an object: in `foo| bar' miw selects foo, because
nothing else touches point."
  (hel-test-with #'text-mode "foo| bar\n"
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "^foo| bar\n"))))

(ert-deftest hel-miw-repeated ()
  "Section Selecting an object: a repeated key adds the next objects."
  (hel-test-with #'text-mode "|foo bar baz\n"
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "^foo| bar baz\n"))
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-render) "^foo bar| baz\n"))))

(ert-deftest hel-2miw ()
  "Rule 8: in mi, a count is the number of objects."
  (hel-test-with #'text-mode "|foo bar baz\n"
    (hel-test-run #'hel-mark-word 2)
    (should (equal (hel-test-render) "^foo bar| baz\n"))))

(ert-deftest hel-mas-whitespace-after ()
  "Mas selects a sentence with the whitespace after it."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode "One. Tw|o three. Four.\n"
      (hel-test-run #'hel-mark-around-sentence)
      (should (equal (hel-test-render) "One. ^Two three. |Four.\n")))))

(ert-deftest hel-mas-at-line-end ()
  "The gap of a sentence stops at a line end, so `mas' on the last sentence
of a line takes the blanks before it."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode "One. Tw|o three.\nFour five.\n"
      (hel-test-run #'hel-mark-around-sentence)
      (should (equal (hel-test-render) "One.^ Two three.|\nFour five.\n")))))

(ert-deftest hel-mas-last-sentence ()
  "Mas on the last sentence of a paragraph takes the whitespace
before it, so it never joins two paragraphs."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode "One. Tw|o.\n\nThree.\n"
      (hel-test-run #'hel-mark-around-sentence)
      (should (equal (hel-test-render) "One.^ Two.|\n\nThree.\n")))))

(ert-deftest hel-3mis-last-sentence ()
  "`3 mis' on the last sentence of a paragraph selects only that
sentence (the paragraph is the parent of its sentences)."
  (let ((sentence-end-double-space nil))
    (hel-test-with #'text-mode "One. Tw|o.\n\nThree. Four.\n"
      (hel-test-run #'hel-mark-sentence 3)
      (should (equal (hel-test-render) "One. ^Two.|\n\nThree. Four.\n")))))

(ert-deftest hel-mip ()
  "Mip selects whole lines with the indentation of the first line
and the last line end."
  (hel-test-with #'text-mode "x\n\n  first line\nsec|ond\n\nnext\n"
    (hel-test-run #'hel-mark-paragraph)
    (should (equal (hel-test-render) "x\n\n|  first line\nsecond\n^\nnext\n"))))

(ert-deftest hel-map ()
  "Map adds the blank lines after the paragraph."
  (hel-test-with #'text-mode "x\n\n  first line\nsec|ond\n\nnext\n"
    (hel-test-run #'hel-mark-around-paragraph)
    (should (equal (hel-test-render) "x\n\n|  first line\nsecond\n\n^next\n"))))

(ert-deftest hel-map-last-paragraph ()
  "Map on the last paragraph adds the blank lines before it."
  (hel-test-with #'text-mode "aaa\n\n\nbb|b\n"
    (hel-test-run #'hel-mark-around-paragraph)
    (should (equal (hel-test-render) "aaa\n|\n\nbbb\n^"))))

(ert-deftest hel-maw-elisp-comment-above ()
  "Maw in an Emacs Lisp buffer leaves out a comment line above the
word."
  (hel-test-with #'emacs-lisp-mode ";; comment\n|foo\n"
    (hel-test-run #'hel-mark-around-word)
    (should (equal (hel-test-render) ";; comment\n^foo|\n"))))

;;; Emacs Lisp: functions and comments without a parser

(defconst hel-to--elisp-headers
  (concat ";; Header of a.\n(defun a ()\n  1)\n\n"
          ";; Header of b.\n(defun b ()\n  2)\n")
  "Two Emacs Lisp functions with header comments.")

(ert-deftest hel-elisp-mif ()
  "In an Emacs Lisp buffer, mif selects the defun without its
header comment."
  (hel-test-with #'emacs-lisp-mode
      (replace-regexp-in-string "  1)" "  |1)" hel-to--elisp-headers)
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   (concat ";; Header of a.\n|(defun a ()\n  1)\n^\n"
                           ";; Header of b.\n(defun b ()\n  2)\n")))))

(ert-deftest hel-elisp-maf ()
  "Maf selects the header comment, the defun and the blank lines
after it, and leaves out the header comment of the next defun."
  (hel-test-with #'emacs-lisp-mode
      (replace-regexp-in-string "  1)" "  |1)" hel-to--elisp-headers)
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   (concat "|;; Header of a.\n(defun a ()\n  1)\n\n"
                           "^;; Header of b.\n(defun b ()\n  2)\n")))))

(ert-deftest hel-elisp-maf-last ()
  "Maf on the last defun of the buffer selects the blank lines
before it, its header comment and the defun."
  (hel-test-with #'emacs-lisp-mode
      (replace-regexp-in-string "  2)" "  |2)" hel-to--elisp-headers)
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   (concat ";; Header of a.\n(defun a ()\n  1)\n|\n"
                           ";; Header of b.\n(defun b ()\n  2)\n^")))))

(ert-deftest hel-elisp-2mf ()
  "In an Emacs Lisp buffer, `2 mf' selects two neighbouring defun
forms and the blank line between them."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
    (hel-test-run #'hel-mark-function 2)
    (should (equal (hel-test-render)
                   "^(defun a ()\n  1)\n\n(defun b ()\n  2)\n|"))))

(ert-deftest hel-elisp-2maf ()
  "`2 maf' in an Emacs Lisp buffer selects two defun forms and the
gap after the second."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n\n(defun c ()\n  3)\n"
    (hel-test-run #'hel-mark-around-function 2)
    (should (equal (hel-test-render)
                   "^(defun a ()\n  1)\n\n(defun b ()\n  2)\n\n|(defun c ()\n  3)\n"))))

(ert-deftest hel-elisp-maf-backward-repeat ()
  "Section Selecting an object: each `-1 maf' adds the function before
the selection."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ()\n  1)\n\n(defun b ()\n  |2)\n\n(defun c ()\n  3)\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "(defun a ()\n  1)\n\n|(defun b ()\n  2)\n\n^(defun c ()\n  3)\n"))
    (hel-test-run #'hel-mark-around-function -1)
    (should (equal (hel-test-render)
                   "|(defun a ()\n  1)\n\n(defun b ()\n  2)\n\n^(defun c ()\n  3)\n"))))

(ert-deftest hel-elisp-maf-forward-repeat ()
  "`maf' puts point at the start of the function, and a second
`maf' adds the next function with point at the end, where it grew."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n\n(defun c ()\n  3)\n"
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "|(defun a ()\n  1)\n\n^(defun b ()\n  2)\n\n(defun c ()\n  3)\n"))
    (hel-test-run #'hel-mark-around-function)
    (should (equal (hel-test-render)
                   "^(defun a ()\n  1)\n\n(defun b ()\n  2)\n\n|(defun c ()\n  3)\n"))))

(ert-deftest hel-elisp-forward-function ()
  "In a buffer without a parser, ]f selects functions through
`hel-function-motion'."
  (hel-test-with #'emacs-lisp-mode
      "(defun a ()\n  |1)\n\n(defun b ()\n  2)\n"
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   "(defun a ()\n  ^1)\n|\n(defun b ()\n  2)\n"))
    (hel-test-run #'hel-mark-function-forward)
    (should (equal (hel-test-render)
                   "(defun a ()\n  1)\n\n^(defun b ()\n  2)\n|"))))

(ert-deftest hel-elisp-mic-run ()
  "In a buffer without a parser, mic selects the `;;' lines with no
blank line between them as one comment."
  (hel-test-with #'emacs-lisp-mode ";; one\n;; t|wo\n(foo)\n"
    (hel-test-run #'hel-mark-comment)
    (should (equal (hel-test-render) "|;; one\n;; two\n^(foo)\n"))))

(ert-deftest hel-elisp-mia-nothing ()
  "In a buffer without a parser, mia leaves the selection as it is."
  (should (hel-test-unchanged-p #'hel-mark-argument
                                "(foo |a b)\n" #'emacs-lisp-mode)))

(ert-deftest hel-c-mode-mif ()
  "In a buffer without a parser, mif selects a function through
`hel-function-motion'."
  (hel-test-with #'c-mode
      "int a(void)\n{\n  return |1;\n}\n\nint b(void)\n{\n  return 2;\n}\n"
    (hel-test-run #'hel-mark-function)
    (should (equal (hel-test-render)
                   "|int a(void)\n{\n  return 1;\n}\n^\nint b(void)\n{\n  return 2;\n}\n"))))

;;; Query files

(ert-deftest hel-query-broken-pattern ()
  "A query file with one broken pattern still yields the objects of
its other pattern (section Query files and grammar versions)."
  (hel-test-need-queries 'python)
  (hel-to--with-query-file
      (concat "(no_such_node_type) @argument\n\n"
              "(argument_list\n"
              "  (_) @argument)\n")
    (hel-test-with #'python-ts-mode "f(|a, b)\n"
      (hel-test-run #'hel-mark-argument)
      (should (equal (hel-test-render) "f(^a|, b)\n")))))

(ert-deftest hel-query-unbalanced ()
  "A query file with unbalanced brackets gives no query and signals
no error, and mif then selects the function through
`hel-function-motion', and mia selects the argument through the syntax
table."
  (hel-test-need-queries 'python)
  (hel-to--with-query-file "(function_definition\n  body: (block\n"
    (hel-test-with #'python-ts-mode "def f(a, b):\n    return |1\n"
      (hel-test-run #'hel-mark-function)
      (should (equal (hel-test-render) "|def f(a, b):\n    return 1\n^")))
    (hel-test-with #'python-ts-mode "def f(|a, b):\n    return 1\n"
      (hel-test-run #'hel-mark-argument)
      (should (equal (hel-test-selection) "a")))))

;;; `hel-thing-motion-range'

(ert-deftest hel-thing-motion-range-of-words ()
  "A backward edge gives a range with its begin before its end."
  (hel-test-with #'fundamental-mode "one two three|"
    (let ((word (hel-thing-producer 'word)))
      (should (equal (hel-thing-motion-range 'word word 9 1 :begin nil)
                     (cons 9 14)))
      (should (equal (hel-thing-motion-range 'word word 9 -1 :begin nil)
                     (cons 5 9)))
      (should (equal (hel-thing-motion-range 'word word 9 -1 :end nil)
                     (cons 8 9))))))

(ert-deftest hel-thing-motion-range-default-method ()
  (hel-test-with #'fundamental-mode "One. Two. Three.|"
    (let ((sentence-end-double-space nil)
          (sentence (hel-thing-producer 'sentence)))
      (should (equal (hel-thing-motion-range 'sentence sentence 7 1 :end nil)
                     (cons 7 10)))
      (should (equal (hel-thing-motion-range 'sentence sentence 7 -1 :begin nil)
                     (cons 6 7))))))

;;; hel-text-object-tests.el ends here
