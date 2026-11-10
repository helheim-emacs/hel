;;; hel-surround-tests.el --- Tests for surround -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; In the texts of these tests, the character `|' marks point.
;;
;;; Code:

(require 'ert)
(require 'hel)

(defmacro hel-surround-tests--with (mode text &rest body)
  "Run BODY in a MODE buffer with TEXT, point at the `|' in TEXT."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'hel-reveal-point-when-on-top) #'ignore)
             ((symbol-function 'hel-push-point) #'ignore))
     (with-temp-buffer
       (let ((transient-mark-mode t))
         (funcall ,mode)
         (insert ,text)
         (goto-char (point-min))
         (search-forward "|")
         (delete-char -1)
         ,@body))))

(defun hel-surround-tests--check (cases)
  "Check CASES, a list of (MODE TEXT COMMAND EXPECTED).
EXPECTED is the text that COMMAND selects, or nil for no selection."
  (pcase-dolist (`(,mode ,text ,command ,expected) cases)
    (ert-info ((format "%S in %S" command text))
      (hel-surround-tests--with mode text
        (funcall command)
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

;;; hel-surround-tests.el ends here
