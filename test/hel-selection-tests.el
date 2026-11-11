;;; hel-selection-tests.el --- Tests of setting the selection -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The functions that set and read the selection and work on ranges:
;; `hel-select', `hel-motion-select', `hel-selection' and their relatives.
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

;;; Setting the selection

(defun hel-selection-tests--state ()
  "Return the mark and point, and whether the region is active."
  (list (mark t) (point) (and (region-active-p) t)))

(defconst hel-selection-tests--text "xxxxxxxxx|xxxxxxxxxxx"
  "Twenty characters with point at 10.")

(ert-deftest hel-select-direction ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-select (cons 3 7))
    (should (equal (hel-selection-tests--state) '(3 7 t)))
    (hel-select (cons 3 7) 1)
    (should (equal (hel-selection-tests--state) '(3 7 t)))
    (hel-select (cons 3 7) -1)
    (should (equal (hel-selection-tests--state) '(7 3 t)))))

(ert-deftest hel-select-ignores-extending ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (set-mark 15)
    (let ((hel-extending-selection t))
      (hel-select (cons 3 7))
      (should (equal (hel-selection-tests--state) '(3 7 t))))))

(ert-deftest hel-motion-select-new-selection ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-motion-select 10 15)
    (should (equal (hel-selection-tests--state) '(10 15 t)))
    (hel-motion-select 15 10)
    (should (equal (hel-selection-tests--state) '(15 10 t)))))

(ert-deftest hel-motion-select-keeps-mark-when-extending ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-select (cons 5 10))
    (let ((hel-extending-selection t))
      (hel-motion-select 10 15)
      (should (equal (hel-selection-tests--state) '(5 15 t)))
      (hel-motion-select 15 2)
      (should (equal (hel-selection-tests--state) '(5 2 t))))))

(ert-deftest hel-selection-shape ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (should-not (hel-selection))
    (hel-select (cons 3 7) -1)
    (should (equal (hel-selection) (list (cons 3 7) -1)))))

(ert-deftest hel-trim-whitespace-takes-a-range ()
  (hel-test-with #'fundamental-mode "  ab \n|"
    (should (equal (hel-trim-whitespace-from-range (cons 1 7)) (cons 3 5)))
    (should-not (hel-trim-whitespace-from-range (cons 1 3)))))

(ert-deftest hel-invert-case-takes-a-range ()
  (hel-test-with #'fundamental-mode "abcd|"
    (hel-invert-case-in-range (cons 2 4))
    (should (equal (buffer-string) "aBCd"))))

(ert-deftest hel-replace-chars-takes-a-range ()
  (hel-test-with #'fundamental-mode "ab\ncd|"
    (hel-replace-chars (cons 2 5) ?x)
    (should (equal (buffer-string) "ax\nxd"))))

(ert-deftest hel-range-any-order ()
  (should (equal (hel-range 3 7) (hel-range 7 3)))
  (should (= 3 (car (hel-range 7 3))))
  (should (= 7 (cdr (hel-range 7 3))))
  (should (= 3 (car (hel-range 3 3))))
  (should (= 3 (cdr (hel-range 3 3)))))

(ert-deftest hel-selection-round-trip ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (dolist (dir '(1 -1))
      (hel-select (cons 3 7) dir)
      (let ((region (hel-selection))
            (state (hel-selection-tests--state)))
        (hel-select (cons 11 13))
        (apply #'hel-select region)
        (should (equal (hel-selection-tests--state) state))))))

(ert-deftest hel-sexp-at-closing-bracket ()
  "From either bracket, the sexp holds its prefix and covers the range."
  (hel-test-with #'emacs-lisp-mode "(f '(a b) y)\n|"
    ;; 4 is the quote, 5 the open bracket, 9 the close bracket.
    (let ((producer (hel-tree-syntax-source)))
      (should (equal (hel-node-on producer (cons 5 6)) (cons 4 10)))
      (should (equal (hel-node-on producer (cons 9 10)) (cons 4 10)))
      (should (equal (hel-node-on producer (cons 9 12)) (cons 4 12))))))

;;; hel-selection-tests.el ends here
