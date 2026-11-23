;;; hel-word-motion-tests.el --- Tests of w, e, b and their WORD forms -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The word motions select from the cursor to the next edge of a word or
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
With EXTEND non-nil, the commands extend a selection that starts at
the cursor."
  (hel-test-with #'fundamental-mode (hel-test-mark text start)
    (when extend
      (set-mark (point))
      (activate-mark))
    (mapcar (lambda (key)
              (if (consp key)
                  (funcall (car key) (cadr key) extend)
                (funcall key 1 extend))
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
  ;; From the space before a word: the space is a step of its own, so the
  ;; motion goes on after it, as in Helix.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" " bar" '(hel-forward-word-start))
                 '("foo ^bar  |baz"))))

(ert-deftest hel-word-motion-w-end-of-buffer ()
  "`w' at the end of the buffer keeps the selection, as in Helix."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-start hel-forward-word-start))
                 '("foo bar  ^baz|" "foo bar  ^baz|"))))

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
                 '("^foo|.bar(baz) qux" "foo|.bar(baz) qux"
                   "foo.^bar|(baz) qux" "foo.bar|(baz) qux"
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

(ert-deftest hel-word-motion-w-indentation-before-fence ()
  "`w' in the indentation selects the rest of it, not the word after a fence."
  (dolist (case '(("(a\n|      (not b))" "(a\n^      |(not b))")
                  ("(a\n   |   (not b))" "(a\n   ^   |(not b))")))
    (hel-test-with #'emacs-lisp-mode (car case)
      (hel-test-run #'hel-forward-word-start)
      (should (equal (hel-test-render) (cadr case))))))

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
  "`e' at the end of the buffer keeps the selection, as in Helix."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-forward-word-end hel-forward-word-end))
                 '("foo bar  ^baz|" "foo bar  ^baz|"))))

(ert-deftest hel-word-motion-e-count ()
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "foo" '((hel-forward-word-end 2)))
                 '("^foo bar|  baz"))))

(ert-deftest hel-word-motion-e-punctuation ()
  (should (equal (hel-word-motion-tests--run
                  "foo.bar(baz) qux" "foo"
                  '(hel-forward-word-end hel-forward-word-end
                                         hel-forward-word-end))
                 '("^foo|.bar(baz) qux" "foo|.bar(baz) qux"
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
  "`b' selects from the cursor back to the start of the previous word."
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "baz"
                  '(hel-backward-word-start hel-backward-word-start))
                 '("foo |bar  ^baz" "|foo ^bar  baz")))
  ;; From the middle of a word: back to its start, with the character
  ;; under the cursor.
  (should (equal (hel-word-motion-tests--run
                  "foo bar  baz" "az" '(hel-backward-word-start))
                 '("foo bar  |ba^z"))))

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
                   "foo.bar|(baz) qux"))))

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
                 ;; At the start of the buffer the selection stays.
                 '("|foo.bar(baz) ^qux" "|foo.bar(baz) ^qux"))))

(ert-deftest hel-word-motion-blanks-after-fence ()
  "`e' and `b' take the blanks between a fence and the word, `w' does not."
  (dolist (case '((hel-forward-word-end "-1) Without an" "1"
                                        ("-1)^ Without| an" "-1) Without^ an|"))
                  (hel-forward-WORD-end "-1) Without an" "1"
                                        ("-1)^ Without| an" "-1) Without^ an|"))
                  (hel-backward-word-start "foo (bar" "bar"
                                           ("|foo ^(bar"))
                  (hel-backward-WORD-start "foo (bar" "bar"
                                           ("|foo ^(bar"))
                  (hel-forward-word-start "-1) Without an" "1"
                                          ("-1) ^Without |an"))))
    (-let [(command text start expected) case]
      (hel-test-with #'emacs-lisp-mode (hel-test-mark text start)
        (should (equal (mapcar (lambda (_)
                                 (funcall command 1 nil)
                                 (hel-test-render))
                               expected)
                       expected))))))

(ert-deftest hel-word-motion-blanks-at-buffer-edge ()
  "Blanks at the edge of the buffer are a step of their own, as in Helix."
  (should (equal (hel-word-motion-tests--run
                  "    Jump" "Jump" '(hel-backward-word-start))
                 '("|    ^Jump")))
  (should (equal (hel-word-motion-tests--run
                  "    Jump" "Jump" '(hel-backward-WORD-start))
                 '("|    ^Jump")))
  (should (equal (hel-word-motion-tests--run
                  "foo   " "o " '(hel-forward-word-end))
                 '("foo^   |")))
  ;; A fence between the word and the blanks stays out of the selection.
  (hel-test-with #'emacs-lisp-mode "  (|foo"
    (hel-backward-word-start 1)
    (should (equal (hel-test-render) "|  ^(foo"))))

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
                 '("foo |bar  b^az" "|foo bar  b^az")))
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
the cursor back to the end of the previous word."
  (hel-test-with #'text-mode "foo bar b|az\n"
    (hel-test-run #'hel-backward-word-end)
    (should (equal (hel-test-render) "foo bar| ba^z\n"))))

(ert-deftest hel-ge-count ()
  "Section Commands are thin: `2 ge' goes back to the end of the second
previous word."
  (hel-test-with #'text-mode "foo bar b|az\n"
    (hel-test-run #'hel-backward-word-end 2)
    (should (equal (hel-test-render) "foo| bar ba^z\n"))))

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
        (should (hel-visual-line-selection-p))
        (hel-select (cons 2 9))
        (should-not (hel-visual-line-selection-p))
        (hel-select (cons 1 12))
        (should-not (hel-visual-line-selection-p))))))

;;; The cases of Helix

;; The cases of `helix-core/src/movement.rs' in Helix that start from
;; a bare cursor with a count of 1.  Each one is (COMMAND TEXT POS ANCHOR
;; HEAD): the cursor stands on the character at index POS, and the
;; motion selects from index ANCHOR to index HEAD.  Indices count from 0.
;; Four cases of Helix are left out, because `_' ends a word in the
;; syntax of `text-mode'.
(defconst hel-word-motion-tests--helix-cases
  '(
    (hel-forward-word-start "Basic forward motion stops at the first space" 0 0 6)
    (hel-forward-word-start " Starting from a boundary advances the anchor" 0 1 10)
    (hel-forward-word-start "Long       whitespace gap is bridged by the head" 0 0 11)
    (hel-forward-word-start "    Starting from whitespace moves to last space in sequence" 0 0 4)
    (hel-forward-word-start "Starting from mid-word leaves anchor at start position and moves head" 3 3 9)
    (hel-forward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 0 0 12)
    (hel-forward-word-start "...   ... punctuation and spaces behave as expected" 0 0 6)
    (hel-forward-word-start "Newlines\n\nare bridged seamlessly." 0 0 8)
    (hel-forward-word-start "oh oh oh two character words!" 0 0 3)
    (hel-forward-word-start "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 0 1 4)
    (hel-forward-word-start "ヒーリクス multibyte characters behave as normal characters" 0 0 6)
    (hel-forward-WORD-start "Basic forward motion stops at the first space" 0 0 6)
    (hel-forward-WORD-start " Starting from a boundary advances the anchor" 0 1 10)
    (hel-forward-WORD-start "Long       whitespace gap is bridged by the head" 0 0 11)
    (hel-forward-WORD-start "    Starting from whitespace moves to last space in sequence" 0 0 4)
    (hel-forward-WORD-start "Starting from mid-word leaves anchor at start position and moves head" 3 3 9)
    (hel-forward-WORD-start "Identifiers_with_underscores are considered a single word" 0 0 29)
    (hel-forward-WORD-start "alphanumeric.!,and.?=punctuation are not treated any differently than alphanumerics" 0 0 33)
    (hel-forward-WORD-start "...   ... punctuation and spaces behave as expected" 0 0 6)
    (hel-forward-WORD-start ".._.._ punctuation is joined by underscores into a single word, as it behaves like alphanumerics" 0 0 7)
    (hel-forward-WORD-start "Newlines\n\nare bridged seamlessly." 0 0 8)
    (hel-forward-WORD-start "oh oh oh two character words!" 0 0 3)
    (hel-forward-WORD-start "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 0 1 4)
    (hel-forward-WORD-start "ヒー..リクス multibyte characters behave as normal characters, including their interaction with punctuation" 0 0 8)
    (hel-backward-word-start "Basic backward motion from the middle of a word" 3 4 0)
    (hel-backward-word-start "    Jump to start of a word preceded by whitespace" 5 6 4)
    (hel-backward-word-start "    Jump to start of line from start of word preceded by whitespace" 4 4 0)
    (hel-backward-WORD-start "Basic backward motion from the middle of a word" 3 4 0)
    (hel-backward-WORD-start "    Jump to start of a word preceded by whitespace" 5 6 4)
    (hel-forward-word-end "Basic forward motion from the start of a word to the end of it" 0 0 5)
    (hel-forward-word-end "Basic forward motion from the middle of a word to the end of it" 2 2 5)
    (hel-forward-word-end "    Jumping to end of a word preceded by whitespace" 0 0 11)
    (hel-forward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 0 0 12)
    (hel-forward-word-end "...   ... punctuation and spaces behave as expected" 0 0 3)
    (hel-forward-word-end "Newlines\n\nare bridged seamlessly." 0 0 8)
    (hel-forward-word-end "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 0 1 4)
    (hel-forward-word-end "ヒーリクス multibyte characters behave as normal characters" 0 0 5)
    (hel-forward-WORD-end "Basic forward motion from the start of a word to the end of it" 0 0 5)
    (hel-forward-WORD-end "Basic forward motion from the middle of a word to the end of it" 2 2 5)
    (hel-forward-WORD-end "    Jumping to end of a word preceded by whitespace" 0 0 11)
    (hel-forward-WORD-end "Identifiers_with_underscores are considered a single word" 0 0 28)
    (hel-forward-WORD-end "alphanumeric.!,and.?=punctuation are treated the same way" 0 0 32)
    (hel-forward-WORD-end "...   ... punctuation and spaces behave as expected" 0 0 3)
    (hel-forward-WORD-end ".._.._ punctuation is joined by underscores into a single block" 0 0 6)
    (hel-forward-WORD-end "Newlines\n\nare bridged seamlessly." 0 0 8)
    (hel-forward-WORD-end "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 0 1 4)
    (hel-forward-WORD-end "ヒーリ..クス multibyte characters behave as normal characters, including  when they interact with punctuation" 0 0 7)
    (hel-backward-word-end "Basic backward motion from the middle of a word" 9 10 5)
    (hel-backward-word-end "Jump     to end of a word succeeded by whitespace" 11 11 4)
    (hel-backward-word-end "    Jump to start of line from end of word preceded by whitespace" 8 8 0)
    (hel-backward-word-end "Jumping to start of word from the end selects the whole word" 16 16 10)
    (hel-backward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 30 31 21)
    (hel-backward-WORD-end "Basic backward motion from the middle of a word" 3 4 0)
    (hel-backward-WORD-end "Jump    to end of a word succeeded by whitespace" 10 10 4)))

(ert-deftest hel-word-motion-helix-cases ()
  "The word motions agree with the tests of Helix from a bare cursor."
  (pcase-dolist (`(,command ,text ,pos ,anchor ,head)
                 hel-word-motion-tests--helix-cases)
    (hel-test-with #'text-mode
        (concat (substring text 0 pos) "|" (substring text pos))
      (funcall command 1)
      (should (equal (list command text (mark) (point))
                     (list command text (1+ anchor) (1+ head)))))))

;; The cases of the same file that start from a selection, with a count
;; of 1.  Each one is (COMMAND TEXT ANCHOR HEAD NEW-ANCHOR NEW-HEAD): the
;; selection runs from index ANCHOR to index HEAD, and the motion selects
;; from NEW-ANCHOR to NEW-HEAD.  As in Helix, the motion starts at the
;; head character of the selection.  Four cases of Helix are left out,
;; because `_' ends a word in `text-mode'.
;; Hel departs from Helix in one rule. When a word or a gap ends right
;; past the head character in the direction of the motion, the motion
;; leaves the head character out. Helix leaves it out only when the target
;; of the motion lies right past it. The cases where the two differ hold
;; the result of Hel: the backward motions from a blank after a word, and
;; from the first character of a word.
(defconst hel-word-motion-tests--helix-selection-cases
  '(
    (hel-forward-word-start "Previous anchor is irrelevant for forward motions" 12 0 0 9)
    (hel-forward-word-start "Jumping\n    into starting whitespace selects the spaces before 'into'" 0 7 8 12)
    (hel-forward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 0 12 12 15)
    (hel-forward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 12 15 15 18)
    (hel-forward-word-start "...   ... punctuation and spaces behave as expected" 0 6 6 10)
    (hel-forward-word-start "Newlines\n\nare bridged seamlessly." 0 8 10 14)
    (hel-forward-word-start "Jumping\n\n\n\n\n\n   from newlines to whitespace selects whitespace." 0 9 13 16)
    (hel-forward-word-start "oh oh oh two character words!" 0 3 3 6)
    ;; The case of Helix from the selection "oh" to "h " is in the table
    ;; `motion-w' of the behaviour tests: Hel leaves the "h" out.
    (hel-forward-word-start "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 1 4 5 8)
    (hel-forward-WORD-start "Previous anchor is irrelevant for forward motions" 12 0 0 9)
    (hel-forward-WORD-start "Jumping\n    into starting whitespace selects the spaces before 'into'" 0 7 8 12)
    (hel-forward-WORD-start "...   ... punctuation and spaces behave as expected" 0 6 6 10)
    (hel-forward-WORD-start "Newlines\n\nare bridged seamlessly." 0 8 10 14)
    (hel-forward-WORD-start "Jumping\n\n\n\n\n\n   from newlines to whitespace selects whitespace." 0 9 13 16)
    (hel-forward-WORD-start "oh oh oh two character words!" 0 3 3 6)
    (hel-forward-WORD-start "oh oh oh two character words!" 0 1 0 3)
    (hel-forward-WORD-start "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 1 4 5 8)
    (hel-backward-word-start "Previous anchor is irrelevant for backward motions" 12 5 6 0)
    (hel-backward-word-start "    Starting from whitespace moves to first space in sequence" 0 4 4 0)
    (hel-backward-word-start "Jumping\n    \nback through a newline selects whitespace" 0 13 12 8)
    (hel-backward-word-start "Jumping to start of word from the end selects the word" 6 7 7 0)
    (hel-backward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 29 30 30 21)
    (hel-backward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 30 21 21 18)
    (hel-backward-word-start "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 21 18 18 15)
    (hel-backward-word-start "...   ... punctuation and spaces behave as expected" 0 10 9 6)
    (hel-backward-word-start "...   ... punctuation and spaces behave as expected" 10 6 6 0)
    (hel-backward-word-start "Newlines\n\nare bridged seamlessly." 0 10 8 0)
    (hel-backward-word-start "Jumping    \n\n\n\n\nback from within a newline group selects previous block" 0 13 11 0)
    (hel-backward-word-start "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 0 8 7 4)
    (hel-backward-word-start "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 7 4 3 0)
    (hel-backward-word-start "ヒーリクス multibyte characters behave as normal characters" 0 6 5 0)
    (hel-backward-WORD-start "    Jump to start of line from start of word preceded by whitespace" 3 4 4 0)
    (hel-backward-WORD-start "Previous anchor is irrelevant for backward motions" 12 5 6 0)
    (hel-backward-WORD-start "    Starting from whitespace moves to first space in sequence" 0 4 4 0)
    (hel-backward-WORD-start "Identifiers_with_underscores are considered a single word" 0 20 20 0)
    (hel-backward-WORD-start "Jumping\n    \nback through a newline selects whitespace" 0 13 12 8)
    (hel-backward-WORD-start "Jumping to start of word from the end selects the word" 6 7 7 0)
    (hel-backward-WORD-start "alphanumeric.!,and.?=punctuation are treated exactly the same" 29 30 30 0)
    (hel-backward-WORD-start "...   ... punctuation and spaces behave as expected" 0 10 9 6)
    (hel-backward-WORD-start "...   ... punctuation and spaces behave as expected" 10 6 6 0)
    (hel-backward-WORD-start ".._.._ punctuation is joined by underscores into a single block" 0 6 6 0)
    (hel-backward-WORD-start "Newlines\n\nare bridged seamlessly." 0 10 8 0)
    (hel-backward-WORD-start "Jumping    \n\n\n\n\nback from within a newline group selects previous block" 0 13 11 0)
    (hel-backward-WORD-start "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 0 8 7 4)
    (hel-backward-WORD-start "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 7 4 3 0)
    (hel-backward-WORD-start "ヒーリ..クス multibyte characters behave as normal characters, including when interacting with punctuation" 0 8 7 0)
    (hel-forward-word-end "Basic forward motion from the end of a word to the end of the next" 0 5 5 13)
    (hel-forward-word-end "Previous anchor is irrelevant for end of word motion" 12 2 2 8)
    (hel-forward-word-end "Jumping\n    into starting whitespace selects up to the end of next word" 0 7 8 16)
    (hel-forward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 0 12 12 15)
    (hel-forward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 12 15 15 18)
    (hel-forward-word-end "...   ... punctuation and spaces behave as expected" 0 3 3 9)
    (hel-forward-word-end "Newlines\n\nare bridged seamlessly." 0 8 10 13)
    (hel-forward-word-end "Jumping\n\n\n\n\n\n   from newlines to whitespace selects to end of next word." 0 8 13 20)
    (hel-forward-word-end "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 1 4 5 8)
    (hel-backward-word-end "Starting from after boundary retreats the anchor" 0 14 13 8)
    (hel-backward-word-end "Previous anchor is irrelevant for backward motions" 26 12 13 8)
    (hel-backward-word-end "    Starting from whitespace moves to first space in sequence" 0 4 4 0)
    (hel-backward-word-end "Jumping\n    \nback through a newline selects whitespace" 0 13 12 8)
    (hel-backward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 31 21 21 18)
    (hel-backward-word-end "alphanumeric.!,and.?=punctuation are considered 'words' for the purposes of word motion" 21 18 18 15)
    (hel-backward-word-end "...   ... punctuation and spaces behave as expected" 0 10 9 3)
    (hel-backward-word-end "...   ... punctuation and spaces behave as expected" 9 3 3 0)
    (hel-backward-word-end "Newlines\n\nare bridged seamlessly." 0 10 8 0)
    (hel-backward-word-end "Jumping    \n\n\n\n\nback from within a newline group selects previous block" 0 13 11 7)
    (hel-backward-word-end "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 0 8 7 4)
    (hel-backward-word-end "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 7 4 3 0)
    (hel-backward-word-end "Test ヒーリクス multibyte characters behave as normal characters" 0 10 10 4)
    (hel-forward-WORD-end "Basic forward motion from the end of a word to the end of the next" 0 5 5 13)
    (hel-forward-WORD-end "Previous anchor is irrelevant for end of word motion" 12 2 2 8)
    (hel-forward-WORD-end "Jumping\n    into starting whitespace selects up to the end of next word" 0 7 8 16)
    (hel-forward-WORD-end "...   ... punctuation and spaces behave as expected" 0 3 3 9)
    (hel-forward-WORD-end "Newlines\n\nare bridged seamlessly." 0 8 10 13)
    (hel-forward-WORD-end "Jumping\n\n\n\n\n\n   from newlines to whitespace selects to end of next word." 0 9 13 20)
    (hel-forward-WORD-end "\n   \n   \n Jumping through alternated space blocks and newlines selects the space blocks" 1 4 5 8)
    (hel-backward-WORD-end "Starting from after boundary retreats the anchor" 0 9 8 0)
    (hel-backward-WORD-end "    Jump to start of line from end of word preceded by whitespace" 3 4 4 0)
    (hel-backward-WORD-end "Previous anchor is irrelevant for backward motions" 12 5 6 0)
    (hel-backward-WORD-end "    Starting from whitespace moves to first space in sequence" 0 4 4 0)
    (hel-backward-WORD-end "Identifiers_with_underscores are considered a single word" 0 20 20 0)
    (hel-backward-WORD-end "Jumping\n    \nback through a newline selects whitespace" 0 13 12 8)
    (hel-backward-WORD-end "Jumping to start of word from the end selects the word" 6 7 7 0)
    (hel-backward-WORD-end "alphanumeric.!,and.?=punctuation are treated exactly the same" 29 30 30 0)
    (hel-backward-WORD-end "...   ... punctuation and spaces behave as expected" 0 10 9 3)
    (hel-backward-WORD-end "...   ... punctuation and spaces behave as expected" 10 6 6 3)
    (hel-backward-WORD-end ".._.._ punctuation is joined by underscores into a single block" 0 6 6 0)
    (hel-backward-WORD-end "Newlines\n\nare bridged seamlessly." 0 10 8 0)
    (hel-backward-WORD-end "Jumping    \n\n\n\n\nback from within a newline group selects previous block" 0 13 11 7)
    (hel-backward-WORD-end "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 0 8 7 4)
    (hel-backward-WORD-end "   \n   \nJumping back through alternated space blocks and newlines selects the space blocks" 7 4 3 0)
    (hel-backward-WORD-end "ヒーリ..クス multibyte characters behave as normal characters, including when interacting with punctuation" 0 8 7 0)))

(ert-deftest hel-word-motion-helix-selection-cases ()
  "The word motions agree with the tests of Helix from a selection."
  (pcase-dolist (`(,command ,text ,anchor ,head ,new-anchor ,new-head)
                 hel-word-motion-tests--helix-selection-cases)
    (hel-test-with #'text-mode
        (concat (substring text 0 head) "|" (substring text head))
      (set-mark (1+ anchor))
      (activate-mark)
      (funcall command 1)
      (should (equal (list command text (mark) (point))
                     (list command text (1+ new-anchor) (1+ new-head)))))))

;;; hel-word-motion-tests.el ends here
