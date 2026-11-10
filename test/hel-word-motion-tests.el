;;; hel-word-motion-tests.el --- Tests of w, e, b and their WORD forms -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The word motions select from point to the next edge of a word or
;; a WORD, and with `v' they extend the selection.
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

;;; Word motions: helpers

(defun hel-word-motion-tests--run (text start keys &optional extend)
  "Run KEYS from START in TEXT and return the marked text after each key.
START is the text at whose first match the cursor stands. A key is a
command, called with the count 1, or a list of a command and a count.
With EXTEND non-nil, the keys extend a selection that starts at the
cursor, as after `v'."
  (hel-test-with #'fundamental-mode (hel-test-mark text start)
    (setq hel-extending-selection extend)
    (when extend
      (set-mark (point))
      (activate-mark))
    (mapcar (lambda (key)
              (if (consp key)
                  (funcall (car key) (cadr key))
                (funcall key 1))
              (hel-test-render))
            keys)))

;;; w and W

(ert-deftest hel-word-motion-w ()
  "`w' selects from point to the start of the next word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-start hel-forward-word-start
                                           hel-forward-word-start))
                 '("^foo |bar  baz" "foo ^bar  |baz" "foo bar  ^baz|")))
  ;; From the middle of a word, and from the space between two words.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "oo " '(hel-forward-word-start))
                 '("f^oo |bar  baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" " bar" '(hel-forward-word-start))
                 '("foo^ |bar  baz"))))

(ert-deftest hel-word-motion-w-end-of-buffer ()
  "`w' at the last word of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-start hel-forward-word-start))
                 '("foo bar  ^baz|" "foo bar  baz|"))))

(ert-deftest hel-word-motion-w-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '((hel-forward-word-start 2)))
                 '("^foo bar  |baz"))))

(ert-deftest hel-word-motion-w-punctuation ()
  "A run of punctuation is a word of its own."
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-word-start hel-forward-word-start
                                           hel-forward-word-start hel-forward-word-start
                                           hel-forward-word-start))
                 '("^foo|.bar(baz) qux" "foo^.|bar(baz) qux"
                   "foo.^bar|(baz) qux" "foo.bar^(|baz) qux"
                   "foo.bar(^baz|) qux"))))

(ert-deftest hel-word-motion-w-line-ends ()
  "`w' stops at the end of a line, and takes the indentation as a step.
It skips an empty line."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "bar"
                  '(hel-forward-word-start hel-forward-word-start
                                           hel-forward-word-start hel-forward-word-start
                                           hel-forward-word-start))
                 '("foo ^bar|\n  baz qux\n\nend"
                   "foo bar\n^  |baz qux\n\nend"
                   "foo bar\n  ^baz |qux\n\nend"
                   "foo bar\n  baz ^qux|\n\nend"
                   "foo bar\n  baz qux\n\n^end|"))))

(ert-deftest hel-word-motion-W ()
  "`W' treats every run of characters other than whitespace as one WORD."
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-WORD-start hel-forward-WORD-start))
                 '("^foo.bar(baz) |qux" "foo.bar(baz) ^qux|"))))

;;; e and E

(ert-deftest hel-word-motion-e ()
  "`e' selects from point to the end of the word, then of the next word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-end hel-forward-word-end
                                         hel-forward-word-end))
                 '("^foo| bar  baz" "foo^ bar|  baz" "foo bar^  baz|"))))

(ert-deftest hel-word-motion-e-end-of-buffer ()
  "`e' at the end of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-end hel-forward-word-end))
                 '("foo bar  ^baz|" "foo bar  baz|"))))

(ert-deftest hel-word-motion-e-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '((hel-forward-word-end 2)))
                 '("^foo bar|  baz"))))

(ert-deftest hel-word-motion-e-punctuation ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-word-end hel-forward-word-end
                                         hel-forward-word-end))
                 '("^foo|.bar(baz) qux" "foo^.|bar(baz) qux"
                   "foo.^bar|(baz) qux"))))

(ert-deftest hel-word-motion-e-line-ends ()
  "`e' stops at the end of a line, and skips the line end and an empty line."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "bar"
                  '(hel-forward-word-end hel-forward-word-end
                                         hel-forward-word-end hel-forward-word-end))
                 '("foo ^bar|\n  baz qux\n\nend"
                   "foo bar\n^  baz| qux\n\nend"
                   "foo bar\n  baz^ qux|\n\nend"
                   "foo bar\n  baz qux\n\n^end|"))))

(ert-deftest hel-word-motion-E ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-WORD-end hel-forward-WORD-end))
                 '("^foo.bar(baz)| qux" "foo.bar(baz)^ qux|"))))

;;; b and B

(ert-deftest hel-word-motion-b ()
  "`b' selects from point back to the start of the previous word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-backward-word-start hel-backward-word-start))
                 '("foo |bar  ^baz" "|foo ^bar  baz")))
  ;; From the middle of a word: back to its start, without the character
  ;; under the cursor.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "az" '(hel-backward-word-start))
                 '("foo bar  |b^az"))))

(ert-deftest hel-word-motion-b-beginning-of-buffer ()
  "`b' at the start of the buffer leaves no selection."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '(hel-backward-word-start))
                 '("|foo bar  baz"))))

(ert-deftest hel-word-motion-b-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz" '((hel-backward-word-start 2)))
                 '("|foo bar  ^baz"))))

(ert-deftest hel-word-motion-b-punctuation ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "qux"
                  '(hel-backward-word-start hel-backward-word-start
                                            hel-backward-word-start))
                 '("foo.bar(baz|) ^qux" "foo.bar(|baz^) qux"
                   "foo.bar|(^baz) qux"))))

(ert-deftest hel-word-motion-b-line-ends ()
  "`b' skips the line end and an empty line before it starts.
It takes the indentation as a step."
  (should (equal (hel-word-motion-tests--run
                  "foo bar\n  baz qux\n\nend" "end"
                  '(hel-backward-word-start hel-backward-word-start
                                            hel-backward-word-start hel-backward-word-start))
                 '("foo bar\n  baz |qux^\n\nend"
                   "foo bar\n  |baz ^qux\n\nend"
                   "foo bar\n|  ^baz qux\n\nend"
                   "foo |bar^\n  baz qux\n\nend"))))

(ert-deftest hel-word-motion-B ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "qux"
                  '(hel-backward-WORD-start hel-backward-WORD-start))
                 '("|foo.bar(baz) ^qux" "|foo.bar(baz) qux"))))

;;; Extending the selection

(ert-deftest hel-word-motion-extend ()
  "With the selection extended, the mark stays and point moves."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-start hel-forward-word-start) t)
                 '("^foo |bar  baz" "^foo bar  |baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-backward-word-start hel-backward-word-start) t)
                 '("foo |bar  ^baz" "|foo bar  ^baz")))
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo"
                  '(hel-forward-word-end hel-forward-word-end) t)
                 '("^foo| bar  baz" "^foo bar|  baz"))))

;;; Word motions

(ert-deftest hel-word-w ()
  "Section Selection rules, rule 10: w selects a word with the space
after it."
  (hel-test-with #'text-mode "|foo bar\n"
    (hel-test-run #'hel-forward-word-start)
    (should (equal (hel-test-render) "^foo |bar\n"))))

(ert-deftest hel-word-w-line-end ()
  "Section The protocol: a word motion stops at a line end."
  (hel-test-with #'text-mode "|foo\nbar\n"
    (hel-test-run #'hel-forward-word-start)
    (should (equal (hel-test-render) "^foo|\nbar\n"))))

(ert-deftest hel-word-e ()
  "Word motions: e selects to the end of the word."
  (hel-test-with #'text-mode "|foo bar\n"
    (hel-test-run #'hel-forward-word-end)
    (should (equal (hel-test-render) "^foo| bar\n"))))

(ert-deftest hel-word-b ()
  "Word motions: b selects back to the start of the word."
  (hel-test-with #'text-mode "foo bar|\n"
    (hel-test-run #'hel-backward-word-start)
    (should (equal (hel-test-render) "foo |bar^\n"))))

(ert-deftest hel-WORD-W ()
  "Word motions: W selects a WORD with the space after it."
  (hel-test-with #'text-mode "|foo-bar baz\n"
    (hel-test-run #'hel-forward-WORD-start)
    (should (equal (hel-test-render) "^foo-bar |baz\n"))))

(ert-deftest hel-WORD-E ()
  "Word motions: E selects to the end of the WORD."
  (hel-test-with #'text-mode "|foo-bar baz\n"
    (hel-test-run #'hel-forward-WORD-end)
    (should (equal (hel-test-render) "^foo-bar| baz\n"))))

(ert-deftest hel-WORD-B ()
  "Word motions: B selects back to the start of the WORD."
  (hel-test-with #'text-mode "foo-bar baz|\n"
    (hel-test-run #'hel-backward-WORD-start)
    (should (equal (hel-test-render) "foo-bar |baz^\n"))))

(ert-deftest hel-ge ()
  "Section Commands are thin: ge (`hel-backward-word-end') selects from
point back to the end of the previous word."
  (hel-test-with #'text-mode "foo bar b|az\n"
    (hel-test-run #'hel-backward-word-end)
    (should (equal (hel-test-render) "foo bar| b^az\n"))))

(ert-deftest hel-ge-count ()
  "Section Commands are thin: `2 ge' goes back to the end of the second
previous word."
  (hel-test-with #'text-mode "foo bar b|az\n"
    (hel-test-run #'hel-backward-word-end 2)
    (should (equal (hel-test-render) "foo| bar b^az\n"))))

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
    (hel-test-with #'text-mode (concat "foo" (string blank) "bar|")
      (should (equal (hel-blank-tests--stops #'hel-WORD-motion) '(4 8))))))

(ert-deftest hel-word-motions-cross-a-page-break ()
  (dolist (motion '(hel-word-motion hel-WORD-motion))
    (hel-test-with #'emacs-lisp-mode "foo\n\f\nbar|"
      (should (= (-last-item (hel-blank-tests--stops motion)) (point-max))))))

(ert-deftest hel-word-motion-stays-backward-symmetric ()
  (hel-test-with #'text-mode (concat "foo" (string #x3000) "bar|")
    (hel-WORD-motion -1)
    (should (= (point) 5))
    (hel-WORD-motion -1)
    (should (= (point) 1))))

(ert-deftest hel-trim-takes-every-blank ()
  (hel-test-with #'emacs-lisp-mode (concat "\f\n \tab\n\f" (string #xA0) "|")
    (should (equal (hel-trim-whitespace-from-range (cons 1 (point-max)))
                   (cons 5 7)))))

(ert-deftest hel-blank-gap-stays-on-its-line ()
  "In `text-mode' a line end has whitespace syntax, and a gap still stops there."
  (hel-test-with #'text-mode "a \f\nb|"
    (should (= (hel-after-blanks 2 1) 4))
    (should (= (hel-after-blanks 4 -1) 2))))

(ert-deftest hel-only-text-on-lines-p-ignores-the-syntax-of-a-line-end ()
  "A line end has comment-end syntax in Lisp and whitespace syntax in text.
Both modes give the same answer."
  (dolist (mode '(emacs-lisp-mode text-mode))
    (hel-test-with mode "a\n  bb  \nc|"
      ;; "bb" and the blanks around it fill the second line.
      (should (hel-only-text-on-lines-p (cons 5 7)))
      ;; Text follows "b" on its line.
      (should-not (hel-only-text-on-lines-p (cons 5 6)))
      ;; Text precedes the line end after "a".
      (should-not (hel-only-text-on-lines-p (cons 2 7))))))

;;; Line edges

(ert-deftest hel-line-edges-leave-point-alone ()
  (hel-test-with #'fundamental-mode "one\ntw|o words\nthree"
    (should (= (hel-line-begin) 5))
    (should (= (hel-line-end) 14))
    (should (= (hel-line-begin 16) 15))
    (should (= (hel-line-end 16) 20))
    (should (= (point) 7))))

(ert-deftest hel-line-begin-skips-invisible-start ()
  (hel-test-with #'fundamental-mode "one\ntwo words|"
    (put-text-property 5 9 'invisible t)
    (should (= (hel-line-begin 12) 9))))

(ert-deftest hel-line-edges-use-visual-lines ()
  (hel-test-with #'fundamental-mode "one tw|o three"
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
  (hel-test-with #'fundamental-mode "one\ntwo words|"
    (put-text-property 5 9 'invisible t)
    (let ((visual-line-mode nil))
      (goto-char 5)
      (should (hel-bolp))
      (goto-char 9)
      (should-not (hel-bolp)))))

(ert-deftest hel-visual-lines-p-checks-both-edges ()
  (hel-test-with #'fundamental-mode "one two three four|"
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

;;; hel-word-motion-tests.el ends here
