;;; hel-selection-tests.el --- Tests of setting the selection -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; The functions that set and read the selection and work on ranges:
;; `hel-select', `hel-set-cursor', `hel-move-to-pos', `hel-move-onto-char',
;; `hel-selection' and their relatives.
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
    (let ((hel-extend-selection t))
      (hel-select (cons 3 7))
      (should (equal (hel-selection-tests--state) '(3 7 t))))))

(ert-deftest hel-move-to-pos-new-selection ()
  "Without EXTEND, `hel-move-to-pos' selects from FROM to TO, whatever the
flag of extending says."
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (let ((hel-extend-selection t))
      (hel-move-to-pos 10 15 nil)
      (should (equal (hel-selection-tests--state) '(10 15 t)))
      (hel-move-to-pos 15 10 nil)
      (should (equal (hel-selection-tests--state) '(15 10 t))))))

(ert-deftest hel-move-to-pos-keeps-anchor-when-extending ()
  "With EXTEND, `hel-move-to-pos' keeps the anchor and ignores FROM,
whatever the flag of extending says."
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-select (cons 5 10))
    (let ((hel-extend-selection nil))
      (hel-move-to-pos 10 15 t)
      (should (equal (hel-selection-tests--state) '(5 15 t)))
      (hel-move-to-pos nil 2 t)
      (should (equal (hel-selection-tests--state) '(5 2 t))))))

(ert-deftest hel-move-to-pos-without-from ()
  "With FROM nil and no EXTEND, `hel-move-to-pos' leaves a bare cursor on
the character after TO, whatever the flag of extending says."
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-select (cons 5 10))
    (let ((hel-extend-selection t))
      (hel-move-to-pos nil 15 nil)
      (should (equal (point) 15))
      (should-not (region-active-p))
      (should (equal (hel-selection) '((15 . 16) nil))))))

(ert-deftest hel-move-onto-char-across-the-anchor ()
  "With EXTEND, `hel-move-onto-char' keeps the anchor character inside the
selection, when the head crosses it in either direction."
  ;; "abcd": forward "bc", the head goes back twice: "b", then "ab".
  (hel-test-with #'fundamental-mode "abcd|"
    (hel-select (cons 2 4) 1)
    (hel-move-onto-char 2 t)
    (should (equal (hel-selection) '((2 . 3) nil)))
    (hel-move-onto-char 1 t)
    (should (equal (hel-selection) '((1 . 3) -1))))
  ;; Backward "bc", the head goes forward twice: "c", then "cd".
  (hel-test-with #'fundamental-mode "abcd|"
    (hel-select (cons 2 4) -1)
    (hel-move-onto-char 3 t)
    (should (equal (hel-selection) '((3 . 4) nil)))
    (hel-move-onto-char 4 t)
    (should (equal (hel-selection) '((3 . 5) 1)))))

(ert-deftest hel-move-onto-char-puts-point-before-it ()
  "Without EXTEND, `hel-move-onto-char' puts point before the character."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-move-onto-char 2 nil)
    (should (= (point) 2))
    (should (equal (hel-selection) '((2 . 3) nil)))))

(ert-deftest hel-extend-selection-argument ()
  "`hel-extend-selection' reads its argument as a minor mode does."
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (pcase-dolist (`(,before ,arg ,after)
                   '((nil nil t) (t nil t) (nil 1 t) (nil 2 t)
                     (t 0 nil) (t -1 nil) (t -2 nil)
                     (nil toggle t) (t toggle nil)))
      (setq hel-extend-selection before)
      (hel-extend-selection arg)
      (should (equal (list before arg (and hel-extend-selection t))
                     (list before arg after))))))

(ert-deftest hel-command-takes-extend ()
  "A command called from Lisp uses its EXTEND argument, not the flag."
  (hel-test-with #'fundamental-mode "|foo bar baz"
    (hel-mode 1)
    (let ((hel-extend-selection nil))
      (hel-forward-word-start 1 t)
      (hel-forward-word-start 1 t)
      (should (equal (hel-test-selection) "foo bar ")))
    (let ((hel-extend-selection t))
      (hel-forward-word-start 1 nil)
      (should (equal (hel-test-selection) "baz")))))

(ert-deftest hel-selection-shape ()
  (hel-test-with #'fundamental-mode hel-selection-tests--text
    (hel-select (cons 3 7) -1)
    (should (equal (hel-selection) (list (cons 3 7) -1)))
    (should (equal (hel-selection-bounds) (cons 3 7)))
    (should (= (hel-selection-direction) -1))))

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

;;; Reading the cursor character

;; Without an active region, `hel-selection' returns the character after
;; point, whatever the inactive mark says.

(defun hel-selection-tests--read ()
  "Return the selection as (TEXT DIRECTION), and check where point is.
TEXT is the text of the selection.  Point must be at the end of the
selection that DIRECTION names, or at its beginning without a direction."
  (-let [((begin . end) dir) (hel-selection)]
    (should (= (point) (if (and dir (< 0 dir)) end begin)))
    (list (buffer-substring-no-properties begin end) dir)))

(ert-deftest hel-selection-without-mark ()
  "Without a mark, the cursor character is the character after point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (should-not (mark t))
    (should (equal (hel-selection-tests--read) '("c" nil)))))

(ert-deftest hel-selection-ignores-the-inactive-mark ()
  "The inactive mark does not change the cursor character."
  (hel-test-with #'fundamental-mode "ab|cd"
    (dolist (offset '(-2 -1 0 1 2))
      (set-marker (mark-marker) (+ (point) offset))
      (should-not (region-active-p))
      (should (equal (list offset (hel-selection-tests--read))
                     (list offset '("c" nil)))))))

(ert-deftest hel-selection-before-a-newline ()
  "Before a newline, the cursor character is the newline."
  (hel-test-with #'fundamental-mode "abc|\ndef"
    (should (equal (hel-selection-tests--read) '("\n" nil)))))

(ert-deftest hel-selection-at-the-end-of-the-buffer ()
  "At the end of the buffer, the cursor character is empty."
  (hel-test-with #'fundamental-mode "abc|"
    (should (equal (hel-selection) (list (cons 4 4) nil)))))

(ert-deftest hel-selection-active-region ()
  "An active region that is not empty is the selection."
  (hel-test-with #'fundamental-mode "abcdefgh|"
    (hel-select (cons 3 6) 1)
    (should (equal (hel-selection) (list (cons 3 6) 1)))
    (hel-select (cons 3 6) -1)
    (should (equal (hel-selection) (list (cons 3 6) -1)))
    ;; A region of one character has no direction.
    (hel-select (cons 3 4) -1)
    (should (equal (hel-selection) (list (cons 3 4) nil)))
    (hel-select (cons 3 4) 1)
    (should (equal (hel-selection) (list (cons 3 4) nil)))))

(ert-deftest hel-selection-empty-active-region ()
  "An empty active region reads as a bare cursor."
  (hel-test-with #'fundamental-mode "ab|cd"
    (push-mark (point) t t)
    (should (region-active-p))
    (should (equal (hel-selection-tests--read) '("c" nil)))))

;;; The round-trip laws

(ert-deftest hel-selection-law-of-select ()
  "After `hel-select', `hel-selection' returns what `hel-select' took."
  (hel-test-with #'fundamental-mode "ab\n\ncd|"
    (cl-loop for begin from (point-min) below (point-max)
             do (cl-loop for end from (1+ begin) to (point-max)
                         do (dolist (dir (if (< 1 (- end begin))
                                             '(1 -1)
                                           '(1 -1 nil)))
                              (hel-select (cons begin end) dir)
                              ;; A selection of one character has no
                              ;; direction.
                              (should (equal (hel-selection)
                                             (list (cons begin end)
                                                   (and (< 1 (- end begin))
                                                        dir)))))))))

(ert-deftest hel-selection-one-character-has-no-direction ()
  "Every stored form of one character gives the same value, without a
direction.  So does the empty selection at the end of the buffer."
  (dolist (input '("fo|o" "fo^o|" "fo|o^"))
    (hel-test-with #'fundamental-mode input
      (should (equal (hel-selection) '((3 . 4) nil)))
      (should (null (hel-selection-direction)))
      (should (hel-bare-cursor-p))))
  (hel-test-with #'fundamental-mode "foo|"
    (should (equal (hel-selection) '((4 . 4) nil)))
    (should (hel-bare-cursor-p)))
  (hel-test-with #'fundamental-mode "f^oo|"
    (should (equal (hel-selection) '((2 . 4) 1)))
    (should-not (hel-bare-cursor-p)))
  ;; The predicate reads the length, also of a value built by hand.
  (should (hel-bare-cursor-p '((3 . 4) 1)))
  (should-not (hel-bare-cursor-p '((3 . 5) nil))))

(ert-deftest hel-selection-head-without-direction ()
  "The head of a selection without a direction is its first character."
  (should (= (hel-selection-head '((3 . 4) nil)) 3))
  (should (= (hel-selection-head '((4 . 4) nil)) 4))
  (should (= (hel-selection-head '((1 . 4) 1)) 3))
  (should (= (hel-selection-head '((1 . 4) -1)) 1)))

(ert-deftest hel-selection-select-needs-a-direction ()
  "`hel-select' signals an error for a longer range without a direction."
  (hel-test-with #'fundamental-mode "ab\n\ncd|"
    (should-error (hel-select (cons 1 3) nil))
    (hel-select (cons 1 2) nil)
    (should (equal (hel-selection) '((1 . 2) nil)))))

(ert-deftest hel-selection-law-of-set-cursor ()
  "After `hel-set-cursor', the cursor character is the character after POS."
  (hel-test-with #'fundamental-mode "ab\n\ncd|"
    (hel-select (cons 1 3))
    (cl-loop for pos from (point-min) to (point-max)
             do (hel-set-cursor pos)
             (should-not (region-active-p))
             (should (= (point) pos))
             (should (equal (hel-selection)
                            (list (hel-char-range pos) nil))))))

;;; The selection of each window

(defun hel-selection-tests--window-state ()
  "Return the selection, whether it is active, and whether motions extend."
  (list (hel-selection) (and (region-active-p) t) hel-extend-selection))

(defun hel-selection-tests--split ()
  "Show the current buffer in a second window, and return it.
The first window stays selected and owns the selection of the buffer."
  (hel-test-press! "")
  (split-window-right))

(defun hel-selection-tests--select-and-mark ()
  "Select the other window and select the first word in it, in one command."
  (interactive)
  (select-window (next-window))
  (goto-char (point-min))
  (push-mark (+ (point) 3) t t))

(put 'hel-selection-tests--select-and-mark 'multiple-cursors nil)

(ert-deftest hel-window-selection-return-after-h ()
  (hel-test-with #'fundamental-mode "one two thr|ee four five six"
    (hel-selection-tests--split)
    (hel-test-press! "h h")
    (let ((before (hel-selection-tests--window-state)))
      (should (equal (car before) (list (cons (point) (1+ (point))) nil)))
      (hel-test-press! "C-x o l l l")
      (hel-test-press! "C-x o")
      (should (equal (hel-selection-tests--window-state) before)))))

(ert-deftest hel-window-selection-return-after-region ()
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (hel-selection-tests--split)
    (hel-test-press! "w")
    (let ((before (hel-selection-tests--window-state)))
      (should (equal (hel-test-selection) "two "))
      (hel-test-press! "C-x o")
      ;; The new window does not see the region of the first one.
      (should-not (region-active-p))
      (hel-test-press! "w w")
      (should (equal (hel-test-selection) "three "))
      (hel-test-press! "C-x o")
      (should (equal (hel-selection-tests--window-state) before))
      (hel-test-press! "C-x o")
      (should (equal (hel-test-selection) "three ")))))

(ert-deftest hel-window-selection-return-after-v ()
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (hel-selection-tests--split)
    (hel-test-press! "v l")
    (let ((before (hel-selection-tests--window-state)))
      (should (nth 2 before))
      (hel-test-press! "C-x o")
      (should-not hel-extend-selection)
      (should-not (region-active-p))
      (hel-test-press! "C-x o")
      (should (equal (hel-selection-tests--window-state) before)))))

(ert-deftest hel-window-selection-point-moved ()
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (let ((first (selected-window))
          (mark (progn (hel-selection-tests--split)
                       (hel-test-press! "v w")
                       (mark t))))
      (hel-test-press! "C-x o")
      (set-window-point first 15)
      (hel-test-press! "C-x o")
      (should (eq (selected-window) first))
      (should (= (point) 15))
      ;; A jump: no region and no extending, and the mark stays.
      (should-not (region-active-p))
      (should-not hel-extend-selection)
      (should (= (mark t) mark)))))

(ert-deftest hel-window-selection-command-sets-region ()
  (hel-test-with #'fundamental-mode "one two |three four five six"
    (let ((first (selected-window)))
      (hel-selection-tests--split)
      (hel-test-press! "w")
      (let ((before (hel-selection-tests--window-state)))
        (global-set-key [f34] #'hel-selection-tests--select-and-mark)
        (unwind-protect (hel-test-press! "<f34>")
          (global-unset-key [f34]))
        (should-not (eq (selected-window) first))
        (should (equal (hel-test-selection) "one"))
        (hel-test-press! "C-x o")
        (should (equal (hel-selection-tests--window-state) before))
        (hel-test-press! "C-x o")
        (should (equal (hel-test-selection) "one"))))))

(ert-deftest hel-window-selection-owner-deleted ()
  "When the owner of the selection is deleted, the window that the user
lands in gets back its own selection, not the mark of the deleted owner."
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (hel-selection-tests--split)
    (hel-test-press! "C-x o w w")
    (let ((before (hel-selection-tests--window-state)))
      (should (equal (hel-test-selection) "three "))
      (hel-test-press! "C-x o w")
      (should (equal (hel-test-selection) "two "))
      (hel-test-press! "C-x 0")
      (should (equal (hel-selection-tests--window-state) before)))))

(ert-deftest hel-window-selection-owner-shows-another-buffer ()
  "When the owner of the selection shows another buffer, the window that
the user lands in gets back its own selection."
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (let ((buffer (current-buffer))
          (other (get-buffer-create "*hel-test-other*")))
      (unwind-protect
          (progn
            (hel-selection-tests--split)
            (hel-test-press! "C-x o w w")
            (let ((before (hel-selection-tests--window-state)))
              (hel-test-press! "C-x o w")
              (switch-to-buffer other)
              (hel-test-press! "C-x o")
              (should (eq (current-buffer) buffer))
              (should (equal (hel-selection-tests--window-state) before))))
        (kill-buffer other)))))

(ert-deftest hel-window-selection-first-switch-after-split ()
  (hel-test-with #'fundamental-mode "one two thr|ee four five six"
    (hel-test-press! "h")
    (let ((before (hel-selection)))
      (should (equal before (list (cons (point) (1+ (point))) nil)))
      (hel-test-press! "C-x 3 C-x o")
      (should-not (eq (selected-window) (frame-first-window)))
      (should (equal (hel-selection) before)))))

(ert-deftest hel-window-selection-through-another-buffer ()
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (let ((other (get-buffer-create "*hel-test-other*")))
      (unwind-protect
          (progn
            (hel-selection-tests--split)
            (set-window-buffer (split-window-below) other)
            (hel-test-press! "w")
            (let ((before (hel-selection-tests--window-state)))
              ;; Through the window on the other buffer to the second window
              ;; on this one, and around again.
              (hel-test-press! "C-x o C-x o")
              (should (eq (window-buffer) (window-buffer (frame-first-window))))
              (should-not (region-active-p))
              (hel-test-press! "C-x o")
              (should (eq (selected-window) (frame-first-window)))
              (should (equal (hel-selection-tests--window-state) before))))
        (kill-buffer other)))))

(ert-deftest hel-window-selection-edit-before-it ()
  (hel-test-with #'fundamental-mode "one two three four |five six"
    (hel-selection-tests--split)
    (hel-test-press! "w")
    (should (equal (hel-test-selection) "five "))
    (hel-test-press! "C-x o g g i new SPC <escape>")
    (should (string-prefix-p "new one" (buffer-string)))
    (hel-test-press! "C-x o")
    (should (equal (hel-test-selection) "five "))))

;;; Drawing the cursor character

;; Batch Emacs does no redisplay, so `hel-test-redisplay' calls
;; `pre-redisplay-function' there; `make test-display' runs the real one.

(ert-deftest hel-cursor-char-drawn-after-point ()
  "The face covers the cursor character after point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-set-cursor 3)
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 4 hel-normal-state-main-cursor nil)))))

(ert-deftest hel-cursor-char-drawn-on-a-newline ()
  "A cursor character on a newline is a cell after the last character."
  (hel-test-with #'fundamental-mode "ab|\ncd"
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 3 nil hel-normal-state-main-cursor)))
    (should (get-text-property
             0 'cursor
             (overlay-get (alist-get (selected-window) hel--main-cursor-overlays)
                          'after-string)))))

(ert-deftest hel-cursor-char-drawn-at-the-end-of-buffer ()
  "The empty bounds at the end of the buffer are a cell too."
  (hel-test-with #'fundamental-mode "ab|"
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 3 nil hel-normal-state-main-cursor)))))

(ert-deftest hel-cursor-char-drawn-inside-an-active-region ()
  "With an active region the box covers the head character, inside the
region: before point in a forward region, after it in a backward one."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-select (cons 1 3))
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(2 3 hel-normal-state-main-cursor nil)))
    (hel-select (cons 1 3) -1)
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(1 2 hel-normal-state-main-cursor nil)))
    ;; An empty region reads as a bare cursor.
    (push-mark (point) t t)
    (should (region-active-p))
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(1 2 hel-normal-state-main-cursor nil)))))

(ert-deftest hel-cursor-char-only-in-normal-state ()
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-switch-state 'insert)
    (hel-test-redisplay)
    (should-not (hel-test-drawn-cursor-char))
    (hel-switch-state 'emacs)
    (hel-test-redisplay)
    (should-not (hel-test-drawn-cursor-char))
    (hel-switch-state 'normal)
    (hel-test-redisplay)
    (should (hel-test-drawn-cursor-char))))

(ert-deftest hel-cursor-char-in-every-window ()
  "Every window shows the box of its cursor at its own point.
The selected window uses the face of the main cursor, and another window
the face `hel-other-window-cursor'.  Each window has its own overlay."
  (hel-test-with #'fundamental-mode "ab|cd"
    (let ((first (selected-window))
          (second (split-window)))
      (set-window-point second 1)
      (hel-test-redisplay)
      (should (equal (hel-test-drawn-cursor-char first)
                     '(3 4 hel-normal-state-main-cursor nil)))
      (should (equal (hel-test-drawn-cursor-char second)
                     '(1 2 hel-other-window-cursor nil)))
      (should (eq second (overlay-get (alist-get second hel--main-cursor-overlays)
                                      'window)))
      (select-window second)
      (hel-test-redisplay)
      (should (equal (hel-test-drawn-cursor-char first)
                     '(3 4 hel-other-window-cursor nil)))
      (should (equal (hel-test-drawn-cursor-char second)
                     '(1 2 hel-normal-state-main-cursor nil))))))

(ert-deftest hel-cursor-box-dims-without-focus ()
  "When the frame loses focus, the selected window draws its box with the
face `hel-other-window-cursor', as Emacs draws a hollow cursor there.  The
box stays over the head character of the selection."
  (hel-test-with #'fundamental-mode "a^bc|d"
    (cl-letf (((symbol-function 'frame-focus-state) #'ignore))
      (hel-test-redisplay)
      (should (equal (hel-test-drawn-cursor-char)
                     '(3 4 hel-other-window-cursor nil))))
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 4 hel-normal-state-main-cursor nil)))))

(ert-deftest hel-cursor-box-in-another-window-under-highlights ()
  "In another window the box lies under a highlight with a priority, as
the match that Consult previews there with priority 2.  In the selected
window it lies above that highlight."
  (hel-test-with #'fundamental-mode "ab|cd"
    (let* ((first (selected-window))
           (second (split-window))
           (match (make-overlay 1 3)))
      (overlay-put match 'priority 2)
      (cl-flet ((box-priority (window)
                  (overlay-get (alist-get window hel--main-cursor-overlays)
                               'priority)))
        (hel-test-redisplay)
        (should (< (box-priority second) 2 (box-priority first)))
        (select-window second)
        (hel-test-redisplay)
        (should (< (box-priority first) 2 (box-priority second)))))))

(ert-deftest hel-cursor-box-on-trailing-whitespace ()
  "A box over a trailing space or tab is a display string of the
character with the face of the box, because the face of
`show-trailing-whitespace' would hide the face of the box.  A space
inside the line keeps the face alone.  So do the secondary cursors."
  (hel-test-with #'fundamental-mode "a b| \t\nc"
    (setq-local show-trailing-whitespace t)
    (cl-flet ((display ()
                (overlay-get (alist-get (selected-window) hel--main-cursor-overlays)
                             'display)))
      (hel-test-redisplay)
      (should (equal (display) " "))
      (should (get-text-property 0 'face (display)))
      (goto-char 5)
      (hel-test-redisplay)
      (should (equal (display) "\t"))
      (goto-char 2)
      (hel-test-redisplay)
      (should-not (display))
      (goto-char 4)
      (setq-local show-trailing-whitespace nil)
      (hel-test-redisplay)
      (should-not (display))
      (setq-local show-trailing-whitespace t)
      (should (equal (overlay-get (hel-test-add-secondary-cursor 4) 'display)
                     " ")))))

(ert-deftest hel-cursor-box-of-a-deleted-window-goes ()
  "A change of the window configuration deletes the boxes of deleted
windows, and a new window draws a box of its own."
  (hel-test-with #'fundamental-mode "ab|cd"
    (cl-flet ((boxes ()
                (seq-filter (lambda (overlay)
                              (eq (overlay-get overlay 'type) 'main-cursor))
                            (overlays-in (point-min) (point-max)))))
      (dotimes (_ 3)
        (let ((window (split-window)))
          (hel-test-redisplay)
          (should (= 2 (length (boxes))))
          (delete-window window)
          (run-hooks 'window-configuration-change-hook)
          (should (equal (list (selected-window))
                         (-map (lambda (overlay) (overlay-get overlay 'window))
                               (boxes)))))))))

(ert-deftest hel-hybrid-cursor-draws-a-bar-and-a-box-on-a-bare-cursor ()
  "With the `hybrid' cursor type the main cursor is a bar at point on a graphic
display.  A bare cursor also has a box with the face `hel-cursor-highlight',
and any other selection has no box.  The secondary cursors keep their boxes,
and a text terminal keeps its own cursor."
  (hel-test-with #'fundamental-mode "ab|cd"
    (let ((hel-cursor-type 'hybrid))
      (cl-letf (((symbol-function 'display-graphic-p) #'always))
        (should (equal (hel--cursor-look 'main t)
                       '(:cursor-type bar :color hel-normal-state-main-cursor
                         :shape box :face hel-cursor-highlight)))
        (should (equal (hel--cursor-look 'main nil)
                       '(:cursor-type bar :color hel-normal-state-main-cursor
                         :shape nil :face hel-cursor-highlight)))
        (should (equal (hel--cursor-look 'secondary)
                       '(:shape box :face hel-normal-state-secondary-cursor))))
      (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
        (should (equal (plist-get (hel--cursor-look 'main t) :cursor-type)
                       'box)))
      (hel-test-redisplay)
      (should (equal (hel-test-drawn-cursor-char)
                     '(3 4 hel-cursor-highlight nil)))
      (hel-select (cons 1 3))
      (hel-test-redisplay)
      (should-not (hel-test-drawn-cursor-char))
      ;; The line selection of an empty line is not a bare cursor, and the
      ;; paint of the line stays.
      (erase-buffer)
      (insert "a\n\nb")
      (hel-select (cons 3 4) nil t)
      (hel-test-redisplay)
      (should-not (hel-test-drawn-cursor-char))
      (should (overlay-buffer hel--main-line-overlay)))
    (cl-letf (((symbol-function 'display-graphic-p) #'always))
      (should (equal (hel--cursor-look 'main t)
                     '(:cursor-type nil :color hel-normal-state-main-cursor
                       :shape box :face hel-normal-state-main-cursor))))))

(ert-deftest hel-hidden-cursor-hides-every-cursor ()
  "A major mode that hides the cursor hides the main and the secondary
cursors.  The region of a secondary cursor stays."
  (hel-test-with #'fundamental-mode "|abcdef"
    (setq-local cursor-type nil)
    (hel--check-if-cursor-is-hidden)
    (should-not (hel--cursor-look 'main t))
    (should-not (hel--cursor-look 'secondary))
    (hel-test-redisplay)
    (should-not (hel-test-drawn-cursor-char))
    (let ((cursor (hel-create-secondary-selection '(3 . 6) 1)))
      (should (equal (hel-test--drawing cursor) '(6 6 nil nil)))
      (should (overlay-buffer (overlay-get cursor 'secondary-region))))))

(ert-deftest hel-paint-cursor ()
  "The painter draws a box over the head character, a cell with the cursor
of Emacs on it at a newline, a bar at point, or nothing."
  (hel-test-with #'fundamental-mode "|ab\ncd"
    (let ((overlay (make-overlay 1 1))
          (box '(:shape box :face bold)))
      (hel--paint-cursor overlay box (hel-selection))
      (should (equal (hel-test--drawing overlay) '(1 2 bold nil)))
      (hel--paint-cursor overlay box (hel--selection-from 3 3 nil))
      (should (equal (hel-test--drawing overlay) '(3 3 nil bold)))
      (should (get-text-property 0 'cursor (overlay-get overlay 'after-string)))
      (hel--paint-cursor overlay '(:shape bar :face bold)
                               (hel--selection-from 4 4 nil))
      (should (equal (hel-test--drawing overlay) '(4 4 nil nil)))
      (should (overlay-get overlay 'before-string))
      (hel--paint-cursor overlay nil (hel--selection-from 5 5 nil))
      (should (equal (hel-test--drawing overlay) '(5 5 nil nil)))
      (should-not (overlay-get overlay 'before-string))
      (delete-overlay overlay))))

(ert-deftest hel-secondary-cursor-follows-edits ()
  "Point and the mark of a secondary cursor move with edits as markers do.
Text inserted at point goes before it, and text inserted at the mark goes
after it.  A deletion that covers them moves them to its start."
  (hel-test-with #'fundamental-mode "|abcdef"
    (let ((cursor (hel-test-add-secondary-cursor 5 3)))
      (cl-flet ((positions ()
                  (list (hel--secondary-cursor-point cursor)
                        (hel--secondary-cursor-mark cursor))))
        (goto-char 1)
        (insert "xx")                   ; before the cursor
        (should (equal (positions) '(7 5)))
        (goto-char 7)
        (insert "y")                    ; at point
        (should (equal (positions) '(8 5)))
        (goto-char 5)
        (insert "z")                    ; at the mark
        (should (equal (positions) '(9 5)))
        (delete-region 4 10)
        (should (equal (positions) '(4 4)))))))

(ert-deftest hel-secondary-cursor-drawn-on-its-cursor-char ()
  "A secondary cursor covers its own cursor character, as the main cursor does."
  (hel-test-with #'fundamental-mode "|ab\ncd\nef"
    (hel-test-add-secondary-cursor 4)         ; "c"
    (hel-test-add-secondary-cursor 8)         ; "f"
    (hel-test-add-secondary-cursor 3)         ; the newline after "b"
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((3 3 nil hel-normal-state-secondary-cursor)
                     (4 5 hel-normal-state-secondary-cursor nil)
                     (8 9 hel-normal-state-secondary-cursor nil))))))

(ert-deftest hel-secondary-cursor-drawn-at-its-head-with-a-region ()
  "A secondary cursor with an active region covers its head character.
The head character is the character of the selection at point, inside
the selection."
  ;; a=1 b=2 c=4 d=5 e=7 f=8, and the end is 9.
  (hel-test-with #'fundamental-mode "|ab\ncd\nef"
    (hel-create-secondary-selection '(1 . 5) 1)      ; "ab\nc" forward: "c"
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((4 5 hel-normal-state-secondary-cursor nil))))
    (hel-disable-multiple-cursors-mode)
    (hel-create-secondary-selection '(7 . 9) -1)     ; "ef" backward: "e"
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((7 8 hel-normal-state-secondary-cursor nil))))
    (hel-disable-multiple-cursors-mode)
    (hel-create-secondary-selection '(4 . 7) 1)      ; "cd\n" forward: a cell
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((6 6 nil hel-normal-state-secondary-cursor))))))

(ert-deftest hel-cursor-look-for-each-state ()
  "The shape of a cursor follows from the state, the role and the kind of
display.  The cursor of Emacs belongs to the main cursor only."
  (hel-test-with #'fundamental-mode "|ab"
    (pcase-dolist (`(,state ,graphic ,main-type ,main ,secondary ,other)
                   '((normal t   nil       box box box)
                     (normal nil box       box box box)
                     (insert t   bar       nil bar nil)
                     (insert nil box       nil box nil)
                     (emacs  t   (hbar . 4) nil nil nil)
                     (emacs  nil (hbar . 4) nil nil nil)))
      (funcall (hel-state-property state :function) 1)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&rest _) graphic)))
        (should (equal (list state graphic
                             (plist-get (hel--cursor-look 'main t) :cursor-type)
                             (plist-get (hel--cursor-look 'main t) :shape)
                             (plist-get (hel--cursor-look 'secondary) :shape)
                             (plist-get (hel--cursor-look 'other t) :shape))
                       (list state graphic main-type main secondary other)))
        (should-not (plist-get (hel--cursor-look 'secondary) :cursor-type))
        (should-not (plist-get (hel--cursor-look 'other t) :cursor-type))))))

(ert-deftest hel-cursor-look-faces ()
  "The faces come from the state, and extending gives its own face.
Another window keeps the face `hel-other-window-cursor'."
  (hel-test-with #'fundamental-mode "|ab"
    (cl-flet ((face (role) (plist-get (hel--cursor-look role t) :face)))
      (should (equal (face 'main) 'hel-normal-state-main-cursor))
      (should (equal (face 'secondary) 'hel-normal-state-secondary-cursor))
      (should (equal (face 'other) 'hel-other-window-cursor))
      (hel-extend-selection 1)
      (should (equal (face 'main) 'hel-extend-selection-cursor))
      (should (equal (face 'secondary) 'hel-extend-selection-cursor))
      (should (equal (face 'other) 'hel-other-window-cursor))
      (hel-insert-state 1)
      (should (equal (plist-get (hel--cursor-look 'main) :color)
                     'hel-insert-state-main-cursor))
      (should (equal (face 'secondary) 'hel-insert-state-secondary-cursor)))))

(ert-deftest hel-main-cursor-drawn-by-state ()
  "The cursor of Emacs is hidden in Normal state on a graphic display,
where Hel draws the box.  It is a bar or a box in Insert state, and an
underscore in Emacs state."
  (hel-test-with #'fundamental-mode "|ab"
    (pcase-dolist (`(,state ,graphic ,type)
                   '((normal t nil) (normal nil box)
                     (insert t bar) (insert nil box)
                     (emacs t (hbar . 4))))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&rest _) graphic)))
        (funcall (hel-state-property state :function) 1)
        (hel-set-emacs-cursor)
        (should (equal (list state graphic cursor-type)
                       (list state graphic type)))))))

(ert-deftest hel-secondary-cursor-drawn-in-insert-state ()
  "In Insert state a secondary cursor is a bar at its point on a graphic
display, and a box over the character after its point in a terminal."
  (hel-test-with #'fundamental-mode "|ab\ncd\nef"
    (cl-letf (((symbol-function 'display-graphic-p) #'always))
      (let ((cursor (hel-test-add-secondary-cursor 5 4)))
        (hel-insert-state 1)
        (hel--draw-secondary-cursor cursor)
        (should (equal (hel-test--drawing cursor) '(5 5 nil nil)))
        (should (equal hel-bar-secondary-cursor
                       (substring-no-properties
                        (overlay-get cursor 'before-string))))
        (should (eq 'hel-insert-state-secondary-cursor
                    (car (get-text-property
                          0 'face (overlay-get cursor 'before-string)))))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (let ((cursor (car (hel-all-secondary-cursors))))
        (hel--draw-secondary-cursor cursor)
        (should (equal (hel-test--drawing cursor)
                       '(5 6 hel-insert-state-secondary-cursor nil)))
        (should-not (overlay-get cursor 'before-string))))))

;;; The cursors as values

(ert-deftest hel-merge-cursors-overlapping-ranges ()
  "Overlapping ranges merge into one; ranges that only touch stay apart."
  ;; Nested.
  (should (equal (hel--merge-cursors '((1 (1 . 10) 1) (2 (2 . 3) nil)))
                 '((1 (1 . 10) 1))))
  ;; Chained: the third range overlaps only the second.
  (should (equal (hel--merge-cursors '((1 (1 . 3) 1)
                                       (2 (2 . 5) 1)
                                       (3 (4 . 6) -1)))
                 '((1 (1 . 6) 1))))
  ;; Touching.
  (let ((cursors '((1 (1 . 3) 1) (2 (3 . 5) 1))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; Equal: two cursors on the same cursor character.
  (should (equal (hel--merge-cursors '((1 (2 . 3) nil) (2 (2 . 3) nil)))
                 '((1 (2 . 3) nil))))
  ;; Two cursors with point at 3, and their cursor characters on the two
  ;; sides of it, only touch.
  (let ((cursors '((1 (2 . 3) nil) (2 (3 . 4) nil))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; Equal and empty, at the end of the buffer.
  (should (equal (hel--merge-cursors '((1 (9 . 9) nil) (2 (9 . 9) nil)))
                 '((1 (9 . 9) nil))))
  ;; An empty range at the end of another range only touches it.
  (let ((cursors '((1 (7 . 9) 1) (2 (9 . 9) nil))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; A bare survivor has no direction, so the longer result is forward.
  (should (equal (hel--merge-cursors '((1 (2 . 3) nil) (2 (2 . 5) -1)))
                 '((1 (2 . 5) 1))))
  ;; Groups keep their order.
  (should (equal (hel--merge-cursors '((1 (1 . 3) 1)
                                       (2 (2 . 4) 1)
                                       (3 (5 . 6) nil)
                                       (4 (7 . 9) 1)
                                       (5 (8 . 9) nil)))
                 '((1 (1 . 4) 1) (3 (5 . 6) nil) (4 (7 . 9) 1))))
  (should (equal (hel--merge-cursors nil) nil)))

(ert-deftest hel-merge-cursors-keeps-a-line-selection ()
  "Section Ranges as values: a merge keeps LINE? only on one character.
Two cursors on one empty line merge into its line selection when one of
them is the line selection.  A merged selection longer than one character
has no LINE?."
  (should (equal (hel--merge-cursors '((1 (2 . 3) nil) (2 (2 . 3) nil t)))
                 '((1 (2 . 3) nil t))))
  (should (equal (hel--merge-cursors '((1 (2 . 3) nil t) (2 (2 . 3) nil)))
                 '((1 (2 . 3) nil t))))
  (should (equal (hel--merge-cursors '((1 (2 . 3) nil t) (2 (2 . 5) 1)))
                 '((1 (2 . 5) 1)))))

(ert-deftest hel-merge-cursors-main-survives ()
  "The main cursor survives in its group, with its direction."
  (let ((after '((7 (8 . 9) nil))))
    ;; The main cursor first, inside and last in its group.
    (should (equal (hel--merge-cursors `((0 (1 . 4) -1)
                                         (5 (2 . 6) 1)
                                         (6 (3 . 7) 1)
                                         ,@after))
                   `((0 (1 . 7) -1) ,@after)))
    (should (equal (hel--merge-cursors `((5 (1 . 4) 1)
                                         (0 (2 . 6) -1)
                                         (6 (3 . 7) 1)
                                         ,@after))
                   `((0 (1 . 7) -1) ,@after)))
    (should (equal (hel--merge-cursors `((5 (1 . 4) 1)
                                         (6 (2 . 6) 1)
                                         (0 (3 . 7) -1)
                                         ,@after))
                   `((0 (1 . 7) -1) ,@after)))))

(defun hel-selection-tests--cursors-state ()
  "Return point, the mark and the activity of every cursor, by ID."
  (cons (list 0 (point) (mark t) mark-active)
        (sort (mapcar (lambda (cursor)
                        (list (overlay-get cursor 'id)
                              (hel--secondary-cursor-point cursor)
                              (hel--secondary-cursor-mark cursor)
                              (overlay-get cursor 'mark-active)))
                      (hel-all-secondary-cursors))
              (lambda (a b) (< (car a) (car b))))))

(defun hel-selection-tests--secondary-cursor (point mark active)
  "Create a secondary cursor with POINT, MARK and an ACTIVE region."
  (let ((mark-active active))
    (hel-test-add-secondary-cursor point mark)))

(ert-deftest hel-selections-law-of-reader-and-writer ()
  "Writing the values that the reader returned changes nothing."
  ;; a=1 b=2 c=4 d=5 e=7 f=8 g=10 h=11 i=13 j=14, and the end is 15.
  (hel-test-with #'fundamental-mode "ab\ncd\nef\ngh\nij"
    (hel-select '(1 . 3) -1)
    (hel-selection-tests--secondary-cursor 4 5 nil)   ; "c"
    (hel-selection-tests--secondary-cursor 9 7 t)     ; region "ef"
    (hel-selection-tests--secondary-cursor 10 10 t)   ; empty region: "g"
    (hel-selection-tests--secondary-cursor 13 15 nil) ; far mark: "i"
    (hel-selection-tests--secondary-cursor 14 13 nil) ; "j"
    (hel-selection-tests--secondary-cursor 15 15 nil) ; the end of the buffer
    (let ((values (hel-selections))
          (state (hel-selection-tests--cursors-state)))
      (should (equal (mapcar #'cdr values)
                     '(((1 . 3) -1)
                       ((4 . 5) nil)
                       ((7 . 9) 1)
                       ((10 . 11) nil)
                       ((13 . 14) nil)
                       ((14 . 15) nil)
                       ((15 . 15) nil))))
      (hel-set-selections values)
      (should (equal (hel-selection-tests--cursors-state) state))
      (should (equal (hel-selections) values)))))

(ert-deftest hel-secondary-cursor-law-of-writer-and-reader ()
  "A selection written into a secondary cursor reads back unchanged.
The writer goes straight into the cursor, both when it creates the
cursor and when it moves one."
  ;; a=1 b=2 c=4 d=5 e=7 f=8 g=10 h=11 i=13 j=14, and the end is 15.
  (hel-test-with #'fundamental-mode "|ab\ncd\nef\ngh\nij"
    (let ((selections '(((4 . 5) nil)         ; "c"
                        ((7 . 8) nil)         ; "e"
                        ((7 . 9) 1)           ; region "ef" forward
                        ((10 . 12) -1)        ; region "gh" backward
                        ((15 . 15) nil))))    ; the end of the buffer
      (dolist (selection selections)
        (let ((cursor (apply #'hel-create-secondary-selection selection)))
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         selection))
          (apply #'hel--set-secondary-selection cursor (car selections))
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         (car selections)))
          (apply #'hel--set-secondary-selection cursor selection)
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         selection)))))))

(ert-deftest hel-set-selections-moves-creates-and-deletes ()
  "The writer moves a cursor by its ID, creates and deletes cursors."
  (hel-test-with #'fundamental-mode "|abcdefgh"
    (let* ((kept (hel-selection-tests--secondary-cursor 3 3 nil))
           (deleted (hel-selection-tests--secondary-cursor 5 5 nil))
           (kept-id (overlay-get kept 'id)))
      (overlay-put kept 'kill-ring '("kept"))
      (setq kill-ring '("main"))
      (hel-set-selections `((0 (2 . 3) nil)
                            (,kept-id (4 . 6) -1)
                            (nil (7 . 8) nil)))
      (should (equal (hel-test-render) "a|bc¦de¬f¦gh"))
      (should (equal (hel-test-cursor-chars)
                     '(("b" nil) ("de" -1) ("g" nil))))
      (should (eq kept (gethash kept-id hel--cursors-table)))
      (should (equal (overlay-get kept 'kill-ring) '("kept")))
      (should (equal kill-ring '("main")))
      (should-not (overlay-buffer deleted))
      (should (= 3 (hel-number-of-cursors)))
      ;; A value whose cursor was deleted since gets a cursor with its ID.
      (let ((id (overlay-get deleted 'id)))
        (hel-set-selections `((0 (2 . 3) nil)
                              (,id (5 . 6) nil)))
        (should (gethash id hel--cursors-table))
        (should (equal (hel-test-cursor-chars) '(("b" nil) ("e" nil))))))))

(ert-deftest hel-merge-overlapping-selections-keeps-main-state ()
  "A merge keeps the main cursor with its own state and deletes the others."
  (hel-test-with #'fundamental-mode "|abcdefgh"
    (setq kill-ring '("main"))
    (hel-select '(3 . 6) -1)
    (overlay-put (hel-selection-tests--secondary-cursor 4 1 t)
                 'kill-ring '("secondary"))
    (hel-selection-tests--secondary-cursor 7 8 nil)   ; "g", apart
    (hel-merge-overlapping-selections)
    (should (equal (hel-test-render) "|abcde^f¦gh"))
    (should (equal kill-ring '("main")))
    (should (= 1 (length (hel-all-secondary-cursors))))))

;;; Saving the selection around an edit

(ert-deftest hel-selection-save-follows-replaced-char ()
  "A bare cursor whose character BODY replaces covers the new text."
  (dolist (input '("a|bc" "a^b|c" "a|b^c"))
    (hel-test-with 'text-mode input
      (hel-save-selection
        (delete-region 2 3)
        (goto-char 2)
        (insert "xyz"))
      (should (equal (list input (hel-selection))
                     (list input '((2 . 5) 1)))))))

;;; The line selection of an empty line
;;
;; In "a\n\nb\n" the positions are a=1, its newline 2, the empty line 3,
;; b=4, its newline 5, and the end 6.

(defconst hel-selection-tests--line-face '(:inherit region :extend t)
  "The face spec that draws the line selection of an empty line.")

(ert-deftest hel-selection-line-of-an-empty-line ()
  "Section Ranges as values: an active region over an empty line is its line.
The value then carries LINE? as a third element.  A bare cursor on the
same line has no third element, so the two values differ."
  (dolist (input '("a\n|\n^b\n" "a\n^\n|b\n"))
    (hel-test-with 'text-mode input
      (should (equal (list input (hel-selection))
                     (list input '((3 . 4) nil t))))))
  (hel-test-with 'text-mode "a\n|\nb\n"
    (should (equal (hel-selection) '((3 . 4) nil)))))

(ert-deftest hel-selection-one-character-elsewhere-is-bare ()
  "Section Ranges as values: LINE? belongs only to one empty line.
An active region of one character anywhere else reads as a bare cursor,
also over the newline of a line that is not empty."
  (dolist (input '("|a^\nb\n" "a^\n|b\n" "a|\n^b\n" "a^\n|\nb\n"))
    (hel-test-with 'text-mode input
      (-let [(range) (hel-selection)]
        (should (equal (list input (hel-selection))
                       (list input (list range nil))))))))

(ert-deftest hel-selection-law-of-select-with-line ()
  "Section Ranges as values: `hel-select' writes LINE? back.
With LINE? a range of one empty line reads back as its line selection, and
any other range of one character reads back as a bare cursor.  Without
LINE? every range of one character is a bare cursor."
  (hel-test-with 'text-mode "ab\n\ncd\n|"
    (cl-loop for begin from (point-min) below (point-max)
             for range = (cons begin (1+ begin))
             for empty-line? = (memq begin '(4))
             do (hel-select range nil t)
                (should (equal (list range (hel-selection))
                               (list range (if empty-line?
                                               (list range nil t)
                                             (list range nil)))))
                (hel-select range nil)
                (should (equal (list range (hel-selection))
                               (list range (list range nil))))
                (should (equal (hel-test-render)
                               (hel-selection-tests--bare-render begin))))))

(defun hel-selection-tests--bare-render (pos)
  "Return the marked text of the buffer with a bare cursor at POS."
  (concat (buffer-substring-no-properties (point-min) pos)
          "|"
          (buffer-substring-no-properties pos (point-max))))

(ert-deftest hel-selection-line-is-checked-on-read ()
  "Section Ranges as values: LINE? is a hint that the reader checks.
A stored line selection whose line has gained text reads back as a bare
cursor on the newline."
  (hel-test-with 'text-mode "a\n|\n^b\n"
    (let ((value (hel-selection)))
      (should (equal value '((3 . 4) nil t)))
      (goto-char 3)
      (deactivate-mark)
      (insert "zz")
      ;; The newline of the line "zz" is at 5.
      (apply #'hel-select (cons '(5 . 6) (cdr value)))
      (should (equal (hel-selection) '((5 . 6) nil))))))

(ert-deftest hel-bare-cursor-p-is-not-a-line-selection ()
  "Section Ranges as values: the line selection of an empty line is not bare.
It covers one character, as a bare cursor does, but the user sees
a selected line."
  (should (hel-bare-cursor-p '((3 . 4) nil)))
  (should (hel-bare-cursor-p '((5 . 5) nil)))
  (should-not (hel-bare-cursor-p '((3 . 4) nil t)))
  (should-not (hel-bare-cursor-p '((3 . 5) 1)))
  (hel-test-with 'text-mode "a\n|\n^b\n"
    (should-not (hel-bare-cursor-p)))
  (hel-test-with 'text-mode "a\n|\nb\n"
    (should (hel-bare-cursor-p))))

(ert-deftest hel-line-selection-p-cases ()
  "Section Ranges as values: whole lines, and one empty line only with LINE?."
  ;; a=1 b=2 newline 3, the empty line 4, c=5 d=6 newline 7, e=8 f=9
  ;; newline 10, and the end is 11.
  (hel-test-with 'text-mode "|ab\n\ncd\nef\n"
    (pcase-dolist (`(,selection ,expected)
                   '((((1 . 4) 1) t)          ; "ab\n"
                     (((1 . 8) -1) t)         ; "ab\n\ncd\n"
                     (((4 . 8) 1) t)          ; "\ncd\n"
                     (((4 . 5) nil t) t)      ; the line selection
                     (((4 . 5) nil) nil)      ; a bare cursor there
                     (((2 . 4) 1) nil)        ; "b\n"
                     (((1 . 7) 1) nil)        ; "ab\n\ncd"
                     (((1 . 2) nil) nil)      ; "a"
                     (((3 . 4) nil) nil)      ; the newline of "ab"
                     (((11 . 11) nil) nil)))  ; the end of the buffer
      (should (equal (list selection (and (hel-line-selection-p selection) t))
                     (list selection expected))))
    ;; Without an argument it reads the selection.
    (hel-select '(1 . 4) 1)
    (should (hel-line-selection-p))
    (hel-select '(4 . 5) nil t)
    (should (hel-line-selection-p))
    (hel-select '(4 . 5) nil)
    (should-not (hel-line-selection-p))))

(ert-deftest hel-paste-line-p-cases ()
  "Section Ranges as values: where pasted text goes on a line of its own.
At the end side the selection must end with a line end; at the beginning
side it must start a line and hold the end of that line.  On one
character only LINE? decides."
  ;; a=1 b=2 newline 3, the empty line 4, c=5 d=6 newline 7, and the end 8.
  (hel-test-with 'text-mode "|ab\n\ncd\n"
    (pcase-dolist (`(,selection ,after ,before)
                   '((((2 . 4) 1) t nil)       ; "b\n"
                     (((1 . 3) -1) nil nil)    ; "ab"
                     (((1 . 6) -1) nil t)      ; "ab\n\nc"
                     (((1 . 4) 1) t t)         ; "ab\n"
                     (((4 . 5) nil t) t t)     ; the line selection
                     (((4 . 5) nil) nil nil)   ; a bare cursor there
                     (((3 . 4) nil) nil nil))) ; the newline of "ab"
      (should (equal (list selection
                           (and (hel--paste-line-p selection 1) t)
                           (and (hel--paste-line-p selection -1) t))
                     (list selection after before))))))

(ert-deftest hel-selections-line-selection-round-trip ()
  "Section Cursors: a cursor value is (ID . SELECTION), LINE? included.
Writing the values that the reader returned changes nothing, also for
a line selection of an empty line and for an active region of one
character, which reads as a bare cursor."
  ;; a=1 b=2 newline 3, the empty line 4, c=5 d=6, and the end 7.
  (hel-test-with #'fundamental-mode "|ab\n\ncd"
    (hel-selection-tests--secondary-cursor 4 5 t)     ; the empty line
    (hel-selection-tests--secondary-cursor 6 5 t)     ; region "c"
    (let ((values (hel-selections))
          (state (hel-selection-tests--cursors-state)))
      (should (equal (mapcar #'cdr values)
                     '(((1 . 2) nil)
                       ((4 . 5) nil t)
                       ((5 . 6) nil))))
      (should (eql 0 (car (car values))))
      (hel-set-selections values)
      (should (equal (hel-selection-tests--cursors-state) state))
      (should (equal (hel-selections) values)))))

(ert-deftest hel-set-selections-writes-a-line-selection ()
  "Section Cursors: the writer gives each cursor its LINE?."
  (hel-test-with #'fundamental-mode "|a\n\nb\n\nc"
    ;; The empty lines are at 3 and 6.
    (let ((cursor (hel-create-secondary-selection '(6 . 7) nil)))
      (hel-set-selections `((0 (3 . 4) nil t)
                            (,(overlay-get cursor 'id) (6 . 7) nil t)))
      (should (equal (mapcar #'cdr (hel-selections))
                     '(((3 . 4) nil t) ((6 . 7) nil t))))
      (hel-set-selections `((0 (3 . 4) nil)
                            (,(overlay-get cursor 'id) (6 . 7) nil)))
      (should (equal (mapcar #'cdr (hel-selections))
                     '(((3 . 4) nil) ((6 . 7) nil))))
      (should (equal (hel-test-render) "a\n|\nb\n¦\nc")))))

(ert-deftest hel-secondary-cursor-line-selection-writer-and-reader ()
  "Section Cursors: a secondary cursor keeps the line selection of its line."
  (hel-test-with #'fundamental-mode "|a\n\nb"
    (let ((cursor (hel-create-secondary-selection '(3 . 4) nil t)))
      (should (equal (cdr (hel--secondary-cursor-value cursor))
                     '((3 . 4) nil t)))
      (hel--set-secondary-selection cursor '(3 . 4) nil)
      (should (equal (cdr (hel--secondary-cursor-value cursor))
                     '((3 . 4) nil)))
      (hel--set-secondary-selection cursor '(3 . 4) nil t)
      (should (equal (cdr (hel--secondary-cursor-value cursor))
                     '((3 . 4) nil t))))))

(ert-deftest hel-selection-save-keeps-empty-line-selection ()
  "The line selection of an empty line stays a line selection."
  (hel-test-with 'text-mode "a\n|\nb"
    (hel-test-press! "x")
    (should (hel-line-selection-p))
    (hel-save-selection nil)
    (should (hel-line-selection-p))
    (should (equal (hel-selection) '((3 . 4) nil t)))))

(ert-deftest hel-selection-save-keeps-a-bare-cursor-on-an-empty-line ()
  "A bare cursor on an empty line stays a bare cursor."
  (hel-test-with 'text-mode "a\n|\nb"
    (hel-save-selection nil)
    (should (equal (hel-selection) '((3 . 4) nil)))
    (should (equal (hel-test-render) "a\n|\nb"))))

(defun hel-selection-tests--line-overlays ()
  "Return the overlays that draw the line selection of an empty line."
  (-filter (lambda (overlay)
             (equal (overlay-get overlay 'face) hel-selection-tests--line-face))
           (overlays-in (point-min) (point-max))))

(ert-deftest hel-line-selection-drawn-for-the-main-cursor ()
  "Section Drawing: the line selection of an empty line paints its line.
The face reaches the edge of the window whatever the theme says, only in
the selected window, and it goes when the selection goes."
  (hel-test-with 'text-mode "a\n|\n^b\n"
    (hel-test-redisplay)
    (let ((overlays (hel-selection-tests--line-overlays)))
      (should (equal (mapcar (lambda (overlay)
                               (list (overlay-start overlay)
                                     (overlay-end overlay)))
                             overlays)
                     '((3 4))))
      (should (eq (overlay-get (car overlays) 'window) (selected-window))))
    (hel-set-cursor 3)
    (hel-test-redisplay)
    (should-not (hel-selection-tests--line-overlays))))

(ert-deftest hel-line-selection-not-drawn-for-a-bare-cursor ()
  "Section Drawing: a bare cursor on an empty line stays a box."
  (hel-test-with 'text-mode "a\n|\nb\n"
    (hel-test-redisplay)
    (should-not (hel-selection-tests--line-overlays))))

(ert-deftest hel-line-selection-drawn-for-a-secondary-cursor ()
  "Section Drawing: a secondary cursor paints the line of its line selection."
  (hel-test-with 'text-mode "|a\n\nb\n"
    (let ((cursor (hel-create-secondary-selection '(3 . 4) nil t)))
      (should (equal (mapcar (lambda (overlay)
                               (list (overlay-start overlay)
                                     (overlay-end overlay)))
                             (hel-selection-tests--line-overlays))
                     '((3 4))))
      (hel--set-secondary-selection cursor '(3 . 4) nil)
      (should-not (hel-selection-tests--line-overlays)))))

;;; Emacs commands that read the text before point

(ert-deftest hel-preceding-sexp-ends-at-the-selection ()
  "In Normal state the sexp before point ends where the selection ends."
  (require 'pp)
  (pcase-dolist (`(,input ,expected)
                 '(("(a b|)" (a b))         ; on the closing bracket
                   ("(fo|o bar)" foo)       ; on the last character of foo
                   ("(^foo bar|)" bar)      ; a forward selection
                   ("(|foo^ bar)" foo)))    ; a backward selection
    (hel-test-with #'emacs-lisp-mode input
      (should (equal (list input (elisp--preceding-sexp) (pp-last-sexp))
                     (list input expected expected)))))
  ;; In Insert state point is a gap, and the sexp ends at point.
  (hel-test-with #'emacs-lisp-mode "(a b|)"
    (hel-insert-state 1)
    (should (equal (elisp--preceding-sexp) 'b))))

(ert-deftest hel-show-paren-at-the-cursor ()
  "In Normal state `show-paren-mode' shows the pair of the bracket under
the box, the head character of the selection."
  (require 'paren)
  (pcase-dolist (`(,input ,pair)
                 '(("(a |b)" nil)            ; not on a bracket
                   ("|(a b)" (1 2 5 6))      ; on the opening bracket
                   ("(a b|)" (5 6 1 2))      ; on the closing bracket
                   ("^(a b)|" (5 6 1 2))     ; a forward selection
                   ("|(a b)^" (1 2 5 6))))   ; a backward selection
    (hel-test-with #'emacs-lisp-mode input
      (should (equal (list input (-take 4 (funcall show-paren-data-function)))
                     (list input pair))))))

;;; A command that fails at some cursors

(defun hel-test--always-fail ()
  "Signal a user error at every cursor."
  (interactive)
  (user-error "Nothing here"))

(put 'hel-test--always-fail 'multiple-cursors t)

(defun hel-test--fail-at-main-cursor ()
  "Signal a user error.  The command runs only for the main cursor."
  (interactive)
  (user-error "Nothing here"))

(put 'hel-test--fail-at-main-cursor 'multiple-cursors nil)

(ert-deftest hel-failed-secondary-cursor-deleted ()
  "A secondary cursor where the command fails is deleted, the others go on.
The cursor at the end of the buffer, after a newline, has no word to
select.  `hel-restore-cursors' brings all three cursors back."
  (hel-test-with #'fundamental-mode "|foo bar\n"
    (hel-test-add-secondary-cursor 5)         ; "b"
    (hel-test-add-secondary-cursor 9)         ; the end of the buffer
    (hel-test-run #'hel-mark-word)
    (should (equal (hel-test-selections) '("foo" "bar")))
    (should (equal (hel-test-selection) "foo"))
    (should (= 3 (length hel--selections-history)))))

(ert-deftest hel-failed-main-cursor-replaced ()
  "When the command fails at the main cursor, the nearest cursor becomes main.
The error goes the way of the command loop: `command-error-function'
reports it, and `post-command-hook' then runs the command for the secondary
cursors.  The default `command-error-function' ends a batch Emacs, so the
test calls only the function that Hel adds to it."
  (should (advice-function-member-p #'hel--record-main-cursor-error
                                    command-error-function))
  (hel-test-with #'fundamental-mode "foo bar\n|"
    (hel-test-add-secondary-cursor 1)         ; "f"
    (hel-test-add-secondary-cursor 5)         ; "b"
    (setq this-command #'hel-mark-word)
    (run-hooks 'pre-command-hook)
    (condition-case err
        (call-interactively #'hel-mark-word)
      (error (hel--record-main-cursor-error err "" nil)))
    (run-hooks 'post-command-hook)
    (should (equal (hel-test-selections) '("foo" "bar")))
    (should (equal (hel-test-selection) "bar"))))

(ert-deftest hel-command-failed-at-every-cursor ()
  "When the command fails at every cursor, every cursor stays."
  (hel-test-with #'fundamental-mode "|foo bar\n"
    (hel-test-add-secondary-cursor 5)
    (hel-test-add-secondary-cursor 9)
    (should-error (hel-test-run #'hel-test--always-fail) :type 'user-error)
    (should (equal (hel-test-selections) '("f" "b" "")))
    (should-not hel--selections-history)))

(ert-deftest hel-command-for-main-cursor-failed ()
  "A command that runs only for the main cursor keeps it when it fails.
The secondary cursors did not run the command, so no cursor has a result
that the main cursor would differ from."
  (hel-test-with #'fundamental-mode "|foo bar\n"
    (hel-test-add-secondary-cursor 5)
    (should-error (hel-test-run #'hel-test--fail-at-main-cursor)
                  :type 'user-error)
    (should (equal (hel-test-selections) '("f" "b")))
    (should-not hel--selections-history)))

;;; hel-selection-tests.el ends here
