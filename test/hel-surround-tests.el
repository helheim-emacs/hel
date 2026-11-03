;;; hel-surround-tests.el --- Tests for surround -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The texts of the tests use the notation of `hel-test-helpers.el'.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hel)
(require 'hel-test-helpers
         (expand-file-name "hel-test-helpers"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defun hel-surround-tests--check (cases)
  "Check CASES, a list of (MODE TEXT COMMAND EXPECTED).
EXPECTED is the text that COMMAND selects, or nil for no selection."
  (pcase-dolist (`(,mode ,text ,command ,expected) cases)
    (ert-info ((format "%S in %S" command text))
      (hel-test-with mode text
        (funcall command 1)
        (should (equal (when (use-region-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end)))
                       expected))))))

(ert-deftest hel-surround-quoted ()
  "`mi\"' and `ma\"' in code, in a string and in a comment."
  (hel-surround-tests--check
   '((emacs-lisp-mode "(foo \"a|b\" c)" hel-mark-inner-double-quoted "ab")
     (emacs-lisp-mode "(foo \"a|b\" c)" hel-mark-a-double-quoted "\"ab\"")
     (emacs-lisp-mode ";; say \"o|ne\" end\n" hel-mark-inner-double-quoted "one")
     (emacs-lisp-mode ";; say \"o|ne\" end\n" hel-mark-a-double-quoted "\"one\"")
     (emacs-lisp-mode "(foo) a|b\n" hel-mark-inner-double-quoted nil))))

(ert-deftest hel-surround-brackets ()
  "`mi(' and `ma(' in code, in a string and in a comment."
  (hel-surround-tests--check
   '((emacs-lisp-mode "(a (b|) c)" hel-mark-inner-paren "b")
     (emacs-lisp-mode "(a (b|) c)" hel-mark-a-paren "(b)")
     (emacs-lisp-mode "(foo \"a|b\" c)" hel-mark-inner-paren "foo \"ab\" c")
     (emacs-lisp-mode "(foo \"a|b\" c)" hel-mark-a-paren "(foo \"ab\" c)")
     (emacs-lisp-mode ";; (a| b) end\n" hel-mark-inner-paren "a b")
     (emacs-lisp-mode ";; (a| b) end\n" hel-mark-a-paren "(a b)")
     ;; The comment holds no pair, so the search goes on around the comment.
     (emacs-lisp-mode "(x ;; b|\n y)" hel-mark-a-paren "(x ;; b\n y)"))))

(ert-deftest hel-surround-comment-limits ()
  "A comment of several lines is the limit of the search inside it."
  (hel-surround-tests--check
   '(;; The pair spans two lines of a run.
     (emacs-lisp-mode ";; say \"o|ne\n;; two\" end\n"
                      hel-mark-inner-double-quoted "one\n;; two")
     (emacs-lisp-mode ";; say \"o|ne\n;; two\" end\n"
                      hel-mark-a-double-quoted "\"one\n;; two\"")
     (emacs-lisp-mode ";; (a|\n;; b) end\n"
                      hel-mark-a-paren "(a\n;; b)")
     ;; The line end belongs to the run.
     (emacs-lisp-mode ";; (a b)|\n;; c\n" hel-mark-a-paren "(a b)")
     ;; A blank line ends a run.
     (emacs-lisp-mode ";; \"o|ne\n\n;; two\" end\n"
                      hel-mark-inner-double-quoted nil))))

(ert-deftest hel-surround-comment-needs-point-inside ()
  "A run limits the search only when the run itself holds point."
  (hel-surround-tests--check
   '(;; Point on the code right after a run, at the end edge of the run.
     (emacs-lisp-mode ";; (a)\n|(b)\n" hel-mark-a-paren "(b)")
     ;; Point on a blank line in the gap after a run.
     (emacs-lisp-mode ";; (a)\n\n|\n;; (c)\n" hel-mark-a-paren nil))))

(ert-deftest hel-surround-touching-pairs ()
  "When point touches two pairs, the pair after point wins. Point right
after a closing bracket, with no pair after it, belongs to that bracket."
  (hel-surround-tests--check
   '((emacs-lisp-mode "(progn (foo)|(bar))" hel-mark-a-paren "(bar)")
     (emacs-lisp-mode "(progn (foo)| (bar))" hel-mark-a-paren "(foo)")
     (emacs-lisp-mode "(progn (foo)|)" hel-mark-a-paren "(foo)"))))

(ert-deftest hel-surround-unmatched-bracket-in-string-or-comment ()
  "A bracket without its pair inside a string or a comment does not pair
up with a bracket outside: the search finds the pair around the string
or the comment."
  (hel-surround-tests--check
   '((emacs-lisp-mode "(foo \"(|\" 1)" hel-mark-a-paren "(foo \"(\" 1)")
     (emacs-lisp-mode "(foo \"(\"| 1)" hel-mark-a-paren "(foo \"(\" 1)")
     (emacs-lisp-mode "(foo \")|\" 1)" hel-mark-a-paren "(foo \")\" 1)")
     (emacs-lisp-mode "(foo ;; (|\n 1)" hel-mark-a-paren "(foo ;; (\n 1)")
     (c-mode "f(a, \"[|\", b);" hel-mark-inner-paren "a, \"[\", b"))))

;;; The laws of the pair producers

(defun hel-surround-tests--law-failures (producer &optional same-delimiters?)
  "Return the positions where PRODUCER breaks a law of the pair protocol.
The laws: the pair at a position holds the position; the pair at the
start of a pair is that pair; the parent of a pair holds it, is larger,
and is the pair at its own start. With SAME-DELIMITERS? non-nil, the
delimiters are the same text, and the second law does not apply."
  (let (failures found)
    (cl-loop
     for pos from (point-min) to (point-max)
     for pair = (hel-pair-at producer pos)
     when pair
     do (push pair found)
        (unless (<= (hel-pair-open-begin pair) pos (hel-pair-close-end pair))
          (push (list 'holds pos) failures))
        (unless (or same-delimiters?
                    (equal pair (hel-pair-at producer (hel-pair-open-begin pair))))
          (push (list 'start pos) failures))
        (when-let* ((parent (hel-pair-parent producer pair)))
          (unless (hel--range-strictly-inside-p (hel-pair-outer-bounds pair)
                                                (hel-pair-outer-bounds parent))
            (push (list 'parent-holds pos) failures))
          (unless (equal parent (hel-pair-at producer (hel-pair-open-begin parent)))
            (push (list 'parent-start pos) failures))))
    ;; A text with no pairs would prove nothing.
    (unless found
      (push 'no-pairs failures))
    (nreverse failures)))

(ert-deftest hel-pair-producer-laws ()
  "Every producer keeps the laws of `hel-pair-at' and `hel-pair-parent'."
  (pcase-dolist (`(,mode ,text ,producer ,same-delimiters?)
                 `((emacs-lisp-mode "(a (b \"(x)\" [c]) ;; (d)\n (e))"
                                    ,(hel-syntax-pair-producer-create ?\( ?\)))
                   (emacs-lisp-mode "(a (b \"(x)\" [c]) ;; (d)\n (e))"
                                    ,(hel-syntax-pair-producer-create ?\[ ?\]))
                   (c-mode "f(a, g(b), \"(\"); { x[1]; }"
                           ,(hel-syntax-pair-producer-create ?\( ?\)))
                   (text-mode "x <a <b> c> y"
                              ,(hel-syntax-pair-producer-create ?< ?>))
                   (emacs-lisp-mode "(foo \"a\" \"b c\")\n"
                                    ,(hel-string-pair-producer-create ?\"))
                   ;; Quotes in a comment or in plain text pair up by
                   ;; nearness.
                   (emacs-lisp-mode ";; \"d\" and \"e\"\n"
                                    ,(hel-string-pair-producer-create ?\") t)
                   (text-mode "say \"one\" and \"two\""
                              ,(hel-string-pair-producer-create ?\") t)
                   (text-mode "x << a << b >> c >> y"
                              ,(hel-delimiter-pair-producer-create
                                :left "<<" :right ">>" :balanced t))
                   (text-mode "x *a* b *c* y"
                              ,(hel-delimiter-pair-producer-create
                                :left "*" :right "*")
                              t)
                   (text-mode "<a><b>x</b> <c/>y</a>"
                              ,(hel-tag-pair-producer-create ""))
                   (emacs-lisp-mode "(a [b {c} d] (e))"
                                    ,(hel-any-pair-producer-create
                                      (list (hel-syntax-pair-producer-create ?\( ?\))
                                            (hel-syntax-pair-producer-create ?\[ ?\])
                                            (hel-syntax-pair-producer-create ?{ ?}))))))
    (ert-info ((format "%S in %S" producer text))
      (hel-test-with mode (concat "|" text)
        (should (equal (hel-surround-tests--law-failures producer same-delimiters?)
                       nil))))))

(ert-deftest hel-pair-parent-walks-outward ()
  "The parents of a pair are the pairs around it, the innermost first."
  (hel-test-with #'emacs-lisp-mode "(a (b (c|) d) e)"
    (let ((producer (hel-syntax-pair-producer-create ?\( ?\))))
      (should (equal (cl-loop for pair = (hel-pair-at producer (point))
                              then (hel-pair-parent producer pair)
                              while pair
                              collect (hel-pair-outer-bounds pair))
                     '((7 . 10) (4 . 13) (1 . 16))))))
  (hel-test-with #'text-mode "<a><b>x|</b></a>"
    (let* ((producer (hel-tag-pair-producer-create ""))
           (pair (hel-pair-at producer (point))))
      (should (equal (hel-pair-outer-bounds pair) '(4 . 12)))
      (should (equal (hel-pair-outer-bounds (hel-pair-parent producer pair))
                     '(1 . 16))))))

(ert-deftest hel-pair-function-producer ()
  "A function that returns four positions gives a pair without a parent."
  (hel-test-with #'text-mode "a [[b|]] c"
    (let ((producer (hel-function-pair-producer-create
                     (lambda () (list 3 5 6 8)))))
      (should (equal (hel-pair-at producer (point))
                     (hel-pair-create 3 5 6 8)))
      (should-not (hel-pair-parent producer (hel-pair-at producer (point)))))))

;;; Strings of the syntax tree

(ert-deftest hel-surround-treesit-strings ()
  "With a parser, a string is one pair, with its whole delimiters: the
three quotes of a Python docstring, and the prefix of an f-string. Where
the query finds no string of the mark, as in a comment, the syntax table
finds the pair."
  (skip-unless (and (treesit-language-available-p 'python)
                    (<= 31 emacs-major-version)))
  (hel-test-with #'python-ts-mode "a = \"\"\"bar\"\"\"\n"
    (skip-unless (hel-treesit-text-objects-p)))
  (hel-surround-tests--check
   '((python-ts-mode "a = |\"\"\"bar\"\"\"\n" hel-mark-inner-double-quoted "bar")
     (python-ts-mode "a = \"\"\"bar\"\"\"|\n" hel-mark-inner-double-quoted "bar")
     (python-ts-mode "a = \"\"\"b|ar\"\"\"\n" hel-mark-a-double-quoted
                     "\"\"\"bar\"\"\"")
     (python-ts-mode "a = f\"b|ar\"\n" hel-mark-a-double-quoted "f\"bar\"")
     (python-ts-mode "a = \"\"|\n" hel-mark-a-double-quoted "\"\"")
     ;; The string of another mark, and a comment.
     (python-ts-mode "a = 'b|ar'\n" hel-mark-inner-double-quoted nil)
     (python-ts-mode "a = \"it|'s\"\n" hel-mark-inner-single-quoted nil)
     (python-ts-mode "# say \"o|ne\"\n" hel-mark-inner-double-quoted "one"))))

;;; The obsolete `hel-surround-alist'

(ert-deftest hel-surround-alist-fallback ()
  "An entry of the obsolete `hel-surround-alist' wins over the new tables:
strings to insert, strings to remove, and a function that finds a pair."
  (hel-test-with #'text-mode "x <<fo|o>> y"
    (with-suppressed-warnings ((obsolete hel-surround-alist))
      (let ((hel--surround-alist-warned t))
        (setq-local hel-surround-alist
                    `((?x :insert ("<<" . ">>") :remove ("<<" . ">>"))
                      (?y :remove ,(lambda () (list 3 5 8 10)))
                      (?\( :insert ("(<" . ">)"))
                      (?z :insert ("<<\n" . ">>\n"))))
        (should (equal (hel-surround--delimiters ?x) '(("<<" . ">>"))))
        (should (equal (hel-surround--delimiters ?\() '(("(<" . ">)"))))
        ;; A newline in the old strings stands for the padding linewise.
        (should (equal (hel-surround--delimiters ?z) '(("<<" . ">>") . linewise)))
        (let ((producer (hel-pair-producer ?x)))
          (should (hel-delimiter-pair-producer-p producer))
          (should (equal (hel-pair-at producer (point))
                         (hel-pair-create 3 5 8 10))))
        (should (hel-function-pair-producer-p (hel-pair-producer ?y)))
        (should (equal (hel-pair-at (hel-pair-producer ?y) (point))
                       (hel-pair-create 3 5 8 10)))
        ;; A character without an entry uses the new tables.
        (should (hel-syntax-pair-producer-p (hel-pair-producer ?\[)))))))

;;; hel-surround-tests.el ends here
