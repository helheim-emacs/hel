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
      (should (equal (hel-selection) '((15 . 16) -1))))))

(ert-deftest hel-move-onto-char-across-the-anchor ()
  "With EXTEND, `hel-move-onto-char' keeps the anchor character inside the
selection, when the head crosses it in either direction."
  ;; "abcd": forward "bc", the head goes back twice: "b", then "ab".
  (hel-test-with #'fundamental-mode "abcd|"
    (hel-select (cons 2 4) 1)
    (hel-move-onto-char 2 t)
    (should (equal (hel-selection) '((2 . 3) -1)))
    (hel-move-onto-char 1 t)
    (should (equal (hel-selection) '((1 . 3) -1))))
  ;; Backward "bc", the head goes forward twice: "c", then "cd".
  (hel-test-with #'fundamental-mode "abcd|"
    (hel-select (cons 2 4) -1)
    (hel-move-onto-char 3 t)
    (should (equal (hel-selection) '((3 . 4) 1)))
    (hel-move-onto-char 4 t)
    (should (equal (hel-selection) '((3 . 5) 1)))))

(ert-deftest hel-move-onto-char-keeps-point-on-reversal ()
  "Without EXTEND, `hel-move-onto-char' moves the character and keeps point
when the new character touches point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-set-cursor 3 -1)                 ; on "c", point before it
    (hel-move-onto-char 2 nil)
    (should (= (point) 3))
    (should (equal (hel-selection) '((2 . 3) 1)))))

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

;; Without an active region, `hel-selection' returns the cursor character.
;; The motions that will move it are not written yet, so these tests put
;; point and the inactive mark where those motions leave them: `h' puts the
;; mark just before point, `l' just after it, and `j' keeps the side.

(defun hel-selection-tests--read ()
  "Return the selection as (TEXT DIRECTION), and check where point is.
TEXT is the text of the selection.  Point must be at the end of the
selection that DIRECTION names."
  (-let [((begin . end) dir) (hel-selection)]
    (should (= (point) (if (< 0 dir) end begin)))
    (list (buffer-substring-no-properties begin end) dir)))

(defun hel-selection-tests--read-with-mark (offset)
  "Put the inactive mark OFFSET characters after point and read the selection.
The value is the one of `hel-selection-tests--read'."
  (set-marker (mark-marker) (+ (point) offset))
  (should-not (region-active-p))
  (hel-selection-tests--read))

(ert-deftest hel-selection-without-mark ()
  "Without a mark, the cursor character is the character after point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (should-not (mark t))
    (should (equal (hel-selection-tests--read) '("c" -1)))))

(ert-deftest hel-selection-after-h ()
  "With the mark just before point, the cursor character is before point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (should (equal (hel-selection-tests--read-with-mark -1) '("b" 1)))))

(ert-deftest hel-selection-after-l ()
  "With the mark just after point, the cursor character is after point."
  (hel-test-with #'fundamental-mode "ab|cd"
    (should (equal (hel-selection-tests--read-with-mark 1) '("c" -1)))))

(ert-deftest hel-selection-after-j ()
  "A vertical motion keeps the side of the cursor character."
  (hel-test-with #'fundamental-mode "abc\nde|fg"
    (should (equal (hel-selection-tests--read-with-mark -1) '("e" 1)))
    (should (equal (hel-selection-tests--read-with-mark 1) '("f" -1)))))

(ert-deftest hel-selection-after-j-onto-a-longer-line ()
  (hel-test-with #'fundamental-mode "ab|c\ndefgh"
    (dolist (case '((1 ("e" 1)) (-1 ("f" -1))))
      (-let [(dir expected) case]
        (hel-test-goto "c")
        (hel-set-cursor (point) dir)
        (forward-line 1)
        (move-to-column 2)
        (hel-set-cursor (point) dir)
        (should (equal (hel-selection-tests--read) expected))))))

(ert-deftest hel-selection-after-j-onto-a-shorter-line ()
  "On a short line, point stops before the newline.
Before point, the cursor character is the last character of the line;
after point, it is the newline."
  (hel-test-with #'fundamental-mode "ab|c\nd\nxyz"
    (dolist (case '((1 ("d" 1)) (-1 ("\n" -1))))
      (-let [(dir expected) case]
        (hel-test-goto "c")
        (hel-set-cursor (point) dir)
        (forward-line 1)
        (move-to-column 2)
        (should (= (current-column) 1))
        (hel-set-cursor (point) dir)
        (should (equal (hel-selection-tests--read) expected))))))

(ert-deftest hel-selection-after-j-onto-an-empty-line ()
  "On an empty line, the cursor character is its newline on either side."
  (hel-test-with #'fundamental-mode "ab|c\n\nxyz"
    (dolist (dir '(1 -1))
      (hel-test-goto "c")
      (hel-set-cursor (point) dir)
      (forward-line 1)
      (move-to-column 2)
      (hel-set-cursor (point) dir)
      (should (= (point) 5))
      (should (equal (hel-selection-tests--read) '("\n" -1))))))

(ert-deftest hel-selection-after-escape ()
  "Leaving Insert state makes the last character before point the cursor
character.  At the start of a line, that character is a newline, so the
cursor character is the character after point."
  (hel-test-with #'fundamental-mode "foo|"
    (should (equal (hel-selection-tests--read-with-mark -1) '("o" 1))))
  (hel-test-with #'fundamental-mode "foo\n|bar"
    (should (equal (hel-selection-tests--read-with-mark -1) '("b" -1)))))

(ert-deftest hel-selection-stale-mark ()
  "A mark that is not next to point does not count."
  (hel-test-with #'fundamental-mode "abcd|efgh"
    (should (equal (hel-selection-tests--read-with-mark -3) '("e" -1)))
    (should (equal (hel-selection-tests--read-with-mark 3) '("e" -1)))
    (should (equal (hel-selection-tests--read-with-mark 0) '("e" -1)))))

(ert-deftest hel-selection-across-a-newline ()
  "The cursor character is a newline only after point."
  (hel-test-with #'fundamental-mode "abc|\ndef"
    (should (equal (hel-selection-tests--read-with-mark 1) '("\n" -1)))
    (should (equal (hel-selection-tests--read-with-mark -1) '("c" 1))))
  (hel-test-with #'fundamental-mode "abc\n|def"
    (should (equal (hel-selection-tests--read-with-mark -1) '("d" -1)))))

(ert-deftest hel-selection-at-the-end-of-the-buffer ()
  "At the end of the buffer, the cursor character after point is empty."
  (hel-test-with #'fundamental-mode "abc|"
    (should (equal (hel-selection) (list (cons 4 4) -1)))
    (should (equal (hel-selection-tests--read-with-mark -1) '("c" 1))))
  (hel-test-with #'fundamental-mode "abc\n|"
    (should (equal (hel-selection-tests--read-with-mark -1) '("" -1)))))

(ert-deftest hel-selection-at-the-start-of-a-narrowed-buffer ()
  "A mark just before the accessible part of the buffer does not count."
  (hel-test-with #'fundamental-mode "abc|def"
    (narrow-to-region 4 7)
    (should (equal (hel-selection-tests--read-with-mark -1) '("d" -1)))))

(ert-deftest hel-selection-active-region ()
  "An active region that is not empty is the selection."
  (hel-test-with #'fundamental-mode "abcdefgh|"
    (hel-select (cons 3 6) 1)
    (should (equal (hel-selection) (list (cons 3 6) 1)))
    (hel-select (cons 3 6) -1)
    (should (equal (hel-selection) (list (cons 3 6) -1)))
    ;; A region of one character before point, as a cursor character
    ;; would be, keeps the region.
    (hel-select (cons 3 4) -1)
    (should (equal (hel-selection) (list (cons 3 4) -1)))))

(ert-deftest hel-selection-empty-active-region ()
  "An empty active region reads as a bare cursor."
  (hel-test-with #'fundamental-mode "ab|cd"
    (push-mark (point) t t)
    (should (region-active-p))
    (should (equal (hel-selection-tests--read) '("c" -1)))))

;;; The round-trip laws

(ert-deftest hel-selection-law-of-select ()
  "After `hel-select', `hel-selection' returns what `hel-select' took."
  (hel-test-with #'fundamental-mode "ab\n\ncd|"
    (cl-loop for begin from (point-min) below (point-max)
             do (cl-loop for end from (1+ begin) to (point-max)
                         do (dolist (dir '(1 -1))
                              (let ((selection (list (cons begin end) dir)))
                                (apply #'hel-select selection)
                                (should (equal (hel-selection) selection))))))))

(ert-deftest hel-selection-law-of-set-cursor ()
  "After `hel-set-cursor', the cursor character is on the side it names.
The exception is the side before point when a newline or the start of the
buffer is there: then the cursor character is the one after point."
  (hel-test-with #'fundamental-mode "ab\n\ncd|"
    (hel-select (cons 1 3))
    (cl-loop for pos from (point-min) to (point-max)
             do (dolist (side '(1 -1))
                  (hel-set-cursor pos side)
                  (should-not (region-active-p))
                  (should (= (point) pos))
                  (should (equal (hel-selection)
                                 (if (and (< 0 side)
                                          (< (point-min) pos)
                                          (/= ?\n (char-before pos)))
                                     (list (cons (1- pos) pos) 1)
                                   (list (cons pos (min (1+ pos) (point-max)))
                                         -1))))))))

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
      ;; The cursor character is before point, where only the mark puts it.
      (should (equal (car before) (list (cons (1- (point)) (point)) 1)))
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

(ert-deftest hel-window-selection-first-switch-after-split ()
  (hel-test-with #'fundamental-mode "one two thr|ee four five six"
    (hel-test-press! "h")
    (let ((before (hel-selection)))
      (should (equal before (list (cons (1- (point)) (point)) 1)))
      (hel-test-press! "C-x 3 C-x o")
      (should-not (eq (selected-window) (frame-first-window)))
      (should (equal (hel-selection) before)))))

(ert-deftest hel-window-selection-deleted-owner ()
  (hel-test-with #'fundamental-mode "one |two three four five six"
    (hel-selection-tests--split)
    (hel-test-press! "w C-x o w w")
    (should (equal (hel-test-selection) "three "))
    (let ((mark (mark t)))
      ;; The first window takes the selection of the deleted one as it is,
      ;; and does not restore its own.
      (hel-test-press! "C-x 0")
      (should (one-window-p))
      (should (= (mark t) mark))
      (should mark-active))))

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

(ert-deftest hel-cursor-char-drawn-on-each-side ()
  "The face covers the cursor character before or after the bar."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-set-cursor 3 1)
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(2 3 hel-cursor-highlight nil)))
    (hel-set-cursor 3 -1)
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 4 hel-cursor-highlight nil)))))

(ert-deftest hel-cursor-char-drawn-on-a-newline ()
  "A cursor character on a newline is a cell after the last character."
  (hel-test-with #'fundamental-mode "ab|\ncd"
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 3 nil hel-cursor-highlight)))
    (should (get-text-property
             0 'cursor
             (overlay-get (window-parameter nil 'hel--cursor-highlight-overlay)
                          'after-string)))))

(ert-deftest hel-cursor-char-drawn-at-the-end-of-buffer ()
  "The empty bounds at the end of the buffer are a cell too."
  (hel-test-with #'fundamental-mode "ab|"
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 3 nil hel-cursor-highlight)))))

(ert-deftest hel-cursor-char-hidden-with-an-active-region ()
  "An active region that is not empty hides the cursor character."
  (hel-test-with #'fundamental-mode "ab|cd"
    (hel-test-redisplay)
    (should (hel-test-drawn-cursor-char))
    (hel-select (cons 1 3))
    (hel-test-redisplay)
    (should-not (hel-test-drawn-cursor-char))
    (push-mark (point) t t)
    (should (region-active-p))
    (hel-test-redisplay)
    (should (equal (hel-test-drawn-cursor-char)
                   '(3 4 hel-cursor-highlight nil)))))

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

(ert-deftest hel-cursor-char-only-in-the-selected-window ()
  "Only the selected window shows its cursor character."
  (hel-test-with #'fundamental-mode "ab|cd"
    (let ((first (selected-window))
          (second (split-window)))
      (hel-test-redisplay)
      (should (hel-test-drawn-cursor-char first))
      (should (eq first (overlay-get (window-parameter
                                      first 'hel--cursor-highlight-overlay)
                                     'window)))
      (select-window second)
      (hel-test-redisplay)
      (should-not (hel-test-drawn-cursor-char first))
      (should (hel-test-drawn-cursor-char second))
      (should (eq second (overlay-get (window-parameter
                                       second 'hel--cursor-highlight-overlay)
                                      'window)))
      (select-window first)
      (hel-test-redisplay)
      (should (hel-test-drawn-cursor-char first))
      (should-not (hel-test-drawn-cursor-char second)))))

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
    (hel-test-add-secondary-cursor 5 4)       ; "c", before point
    (hel-test-add-secondary-cursor 8 9)       ; "f": the mark is not before point
    (hel-test-add-secondary-cursor 3 4)       ; the newline after "b"
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
    (hel-create-secondary-cursor '(1 . 5) 1 t)    ; "ab\nc" forward: "c"
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((4 5 hel-normal-state-secondary-cursor nil))))
    (hel-disable-multiple-cursors-mode)
    (hel-create-secondary-cursor '(7 . 9) -1 t)   ; "ef" backward: "e"
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((7 8 hel-normal-state-secondary-cursor nil))))
    (hel-disable-multiple-cursors-mode)
    (hel-create-secondary-cursor '(4 . 7) 1 t)    ; "cd\n" forward: a cell
    (should (equal (hel-test-drawn-secondary-cursors)
                   '((6 6 nil hel-normal-state-secondary-cursor))))))

(ert-deftest hel-cursor-look-for-each-state ()
  "The shape of a cursor follows from the state and the kind of display."
  (hel-test-with #'fundamental-mode "|ab"
    (pcase-dolist (`(,state ,graphic ,main ,secondary)
                   '((normal t   bar  box)
                     (normal nil bar  box)
                     (insert t   bar  bar)
                     (insert nil box  box)
                     (emacs  t   hbar nil)
                     (emacs  nil hbar nil)))
      (funcall (hel-state-property state :function) 1)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&rest _) graphic)))
        (should (equal (list state graphic
                             (car (hel--cursor-look t))
                             (car (hel--cursor-look nil)))
                       (list state graphic main secondary)))))))

(ert-deftest hel-cursor-look-faces ()
  "The faces come from the state, and extending gives its own face."
  (hel-test-with #'fundamental-mode "|ab"
    (should (equal (cadr (hel--cursor-look t)) 'hel-normal-state-main-cursor))
    (should (equal (cadr (hel--cursor-look nil))
                   'hel-normal-state-secondary-cursor))
    (hel-extend-selection 1)
    (should (equal (cadr (hel--cursor-look t)) 'hel-extend-selection-cursor))
    (should (equal (cadr (hel--cursor-look nil)) 'hel-extend-selection-cursor))
    (hel-insert-state 1)
    (should (equal (cadr (hel--cursor-look t)) 'hel-insert-state-main-cursor))
    (should (equal (cadr (hel--cursor-look nil))
                   'hel-insert-state-secondary-cursor))))

(ert-deftest hel-main-cursor-drawn-by-state ()
  "The main cursor is a bar in Normal state, a bar or a box in Insert state,
and an underscore in Emacs state."
  (hel-test-with #'fundamental-mode "|ab"
    (pcase-dolist (`(,state ,graphic ,type)
                   '((normal t bar) (insert t bar) (insert nil box)
                     (emacs t (hbar . 4))))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&rest _) graphic)))
        (funcall (hel-state-property state :function) 1)
        (hel-draw-cursor)
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
  (should (equal (hel--merge-cursors '((1 (1 . 10) 1 t) (2 (2 . 3) -1 t)))
                 '((1 (1 . 10) 1 t))))
  ;; Chained: the third range overlaps only the second.
  (should (equal (hel--merge-cursors '((1 (1 . 3) 1 t)
                                       (2 (2 . 5) 1 t)
                                       (3 (4 . 6) -1 t)))
                 '((1 (1 . 6) 1 t))))
  ;; Touching.
  (let ((cursors '((1 (1 . 3) 1 t) (2 (3 . 5) 1 t))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; Equal: two cursors on the same cursor character.
  (should (equal (hel--merge-cursors '((1 (2 . 3) 1 nil) (2 (2 . 3) -1 nil)))
                 '((1 (2 . 3) 1 nil))))
  ;; Two cursors with point at 3, and their cursor characters on the two
  ;; sides of it, only touch.
  (let ((cursors '((1 (2 . 3) 1 nil) (2 (3 . 4) -1 nil))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; Equal and empty, at the end of the buffer.
  (should (equal (hel--merge-cursors '((1 (9 . 9) -1 nil) (2 (9 . 9) -1 nil)))
                 '((1 (9 . 9) -1 nil))))
  ;; An empty range at the end of another range only touches it.
  (let ((cursors '((1 (7 . 9) 1 t) (2 (9 . 9) -1 nil))))
    (should (equal (hel--merge-cursors cursors) cursors)))
  ;; The region of the result is active when one of the group is.
  (should (equal (hel--merge-cursors '((1 (2 . 3) -1 nil) (2 (2 . 5) 1 t)))
                 '((1 (2 . 5) -1 t))))
  ;; Groups keep their order.
  (should (equal (hel--merge-cursors '((1 (1 . 3) 1 t)
                                       (2 (2 . 4) 1 t)
                                       (3 (5 . 6) -1 nil)
                                       (4 (7 . 9) 1 t)
                                       (5 (8 . 9) 1 t)))
                 '((1 (1 . 4) 1 t) (3 (5 . 6) -1 nil) (4 (7 . 9) 1 t))))
  (should (equal (hel--merge-cursors nil) nil)))

(ert-deftest hel-merge-cursors-main-survives ()
  "The main cursor survives in its group, with its direction."
  (let ((after '((7 (8 . 9) 1 nil))))
    ;; The main cursor first, inside and last in its group.
    (should (equal (hel--merge-cursors `((0 (1 . 4) -1 t)
                                         (5 (2 . 6) 1 t)
                                         (6 (3 . 7) 1 nil)
                                         ,@after))
                   `((0 (1 . 7) -1 t) ,@after)))
    (should (equal (hel--merge-cursors `((5 (1 . 4) 1 t)
                                         (0 (2 . 6) -1 t)
                                         (6 (3 . 7) 1 t)
                                         ,@after))
                   `((0 (1 . 7) -1 t) ,@after)))
    (should (equal (hel--merge-cursors `((5 (1 . 4) 1 t)
                                         (6 (2 . 6) 1 t)
                                         (0 (3 . 7) -1 nil)
                                         ,@after))
                   `((0 (1 . 7) -1 t) ,@after)))))

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

(ert-deftest hel-cursor-selections-law-of-reader-and-writer ()
  "Writing the values that the reader returned changes nothing."
  ;; a=1 b=2 c=4 d=5 e=7 f=8 g=10 h=11 i=13 j=14, and the end is 15.
  (hel-test-with #'fundamental-mode "ab\ncd\nef\ngh\nij"
    (hel-select '(1 . 3) -1)
    (hel-selection-tests--secondary-cursor 5 4 nil)   ; "c", before point
    (hel-selection-tests--secondary-cursor 9 7 t)     ; region "ef"
    (hel-selection-tests--secondary-cursor 10 10 t)   ; empty region: "g"
    (hel-selection-tests--secondary-cursor 13 15 nil) ; stale mark: "i"
    (hel-selection-tests--secondary-cursor 15 14 nil) ; "j", before point
    (hel-selection-tests--secondary-cursor 15 15 nil) ; the end of the buffer
    (let ((values (hel-cursor-selections))
          (state (hel-selection-tests--cursors-state)))
      (should (equal (mapcar #'cdr values)
                     '(((1 . 3) -1 t)
                       ((4 . 5) 1 nil)
                       ((7 . 9) 1 t)
                       ((10 . 11) -1 nil)
                       ((13 . 14) -1 nil)
                       ((14 . 15) 1 nil)
                       ((15 . 15) -1 nil))))
      (hel-set-cursor-selections values)
      (should (equal (hel-selection-tests--cursors-state) state))
      (should (equal (hel-cursor-selections) values)))))

(ert-deftest hel-secondary-cursor-law-of-writer-and-reader ()
  "A selection written into a secondary cursor reads back unchanged.
The writer goes straight into the cursor, both when it creates the
cursor and when it moves one."
  ;; a=1 b=2 c=4 d=5 e=7 f=8 g=10 h=11 i=13 j=14, and the end is 15.
  (hel-test-with #'fundamental-mode "|ab\ncd\nef\ngh\nij"
    (let ((selections '(((4 . 5) 1 nil)       ; "c", before point
                        ((7 . 8) -1 nil)      ; "e", after point
                        ((7 . 9) 1 t)         ; region "ef" forward
                        ((10 . 12) -1 t)      ; region "gh" backward
                        ((15 . 15) -1 nil)))) ; the end of the buffer
      (dolist (selection selections)
        (let ((cursor (apply #'hel-create-secondary-cursor selection)))
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         selection))
          (apply #'hel--set-secondary-cursor cursor (car selections))
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         (car selections)))
          (apply #'hel--set-secondary-cursor cursor selection)
          (should (equal (cdr (hel--secondary-cursor-value cursor))
                         selection)))))))

(ert-deftest hel-set-cursor-selections-moves-creates-and-deletes ()
  "The writer moves a cursor by its ID, creates and deletes cursors."
  (hel-test-with #'fundamental-mode "|abcdefgh"
    (let* ((kept (hel-selection-tests--secondary-cursor 3 3 nil))
           (deleted (hel-selection-tests--secondary-cursor 5 5 nil))
           (kept-id (overlay-get kept 'id)))
      (overlay-put kept 'kill-ring '("kept"))
      (setq kill-ring '("main"))
      (hel-set-cursor-selections `((0 (2 . 3) 1 nil)
                                   (,kept-id (4 . 6) -1 t)
                                   (nil (7 . 8) -1 nil)))
      (should (equal (hel-test-render) "ab|c¦de¬f¦gh"))
      (should (equal (hel-test-cursor-chars)
                     '(("b" 1) ("de" -1) ("g" -1))))
      (should (eq kept (gethash kept-id hel--cursors-table)))
      (should (equal (overlay-get kept 'kill-ring) '("kept")))
      (should (equal kill-ring '("main")))
      (should-not (overlay-buffer deleted))
      (should (= 3 (hel-number-of-cursors)))
      ;; A value whose cursor was deleted since gets a cursor with its ID.
      (let ((id (overlay-get deleted 'id)))
        (hel-set-cursor-selections `((0 (2 . 3) 1 nil)
                                     (,id (5 . 6) -1 nil)))
        (should (gethash id hel--cursors-table))
        (should (equal (hel-test-cursor-chars) '(("b" 1) ("e" -1))))))))

(ert-deftest hel-merge-overlapping-cursors-keeps-main-state ()
  "A merge keeps the main cursor with its own state and deletes the others."
  (hel-test-with #'fundamental-mode "|abcdefgh"
    (setq kill-ring '("main"))
    (hel-select '(3 . 6) -1)
    (overlay-put (hel-selection-tests--secondary-cursor 4 1 t)
                 'kill-ring '("secondary"))
    (hel-selection-tests--secondary-cursor 7 8 nil)   ; "g", apart
    (hel-merge-overlapping-cursors)
    (should (equal (hel-test-render) "|abcde^f¦gh"))
    (should (equal kill-ring '("main")))
    (should (= 1 (length (hel-all-secondary-cursors))))))

;;; hel-selection-tests.el ends here
