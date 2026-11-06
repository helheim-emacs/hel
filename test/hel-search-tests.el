;;; hel-search-tests.el --- Tests for hel-search -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Run from a running Emacs with Hel loaded:
;;
;;   (load-file "test/hel-search-tests.el")
;;   (ert "^hel-")
;;
;;; Code:

(require 'ert)
(require 'hel-lib)
(require 'hel-search)

;;; hel-partition-index

(ert-deftest hel-partition-index-empty ()
  (should (= 0 (hel-partition-index [] #'identity))))

(ert-deftest hel-partition-index-all-nil ()
  (should (= 3 (hel-partition-index [1 2 3] (lambda (x) (< 10 x))))))

(ert-deftest hel-partition-index-all-true ()
  (should (= 0 (hel-partition-index [1 2 3] (lambda (x) (< 0 x))))))

(ert-deftest hel-partition-index-split ()
  (dotimes (k 8)
    (let ((vec (vconcat (number-sequence 0 6))))
      (should (= (min k 7)
                 (hel-partition-index vec (lambda (x) (<= k x))))))))

;;; hel-lower-bound, hel-upper-bound

(ert-deftest hel-lower-bound ()
  (let ((vec [1 3 3 5]))
    (should (= 0 (hel-lower-bound vec 0)))
    (should (= 1 (hel-lower-bound vec 3)))
    (should (= 3 (hel-lower-bound vec 4)))
    (should (= 4 (hel-lower-bound vec 6)))
    (should (= 0 (hel-lower-bound [] 1)))))

(ert-deftest hel-upper-bound ()
  (let ((vec [1 3 3 5]))
    (should (= 0 (hel-upper-bound vec 0)))
    (should (= 3 (hel-upper-bound vec 3)))
    (should (= 4 (hel-upper-bound vec 5)))
    (should (= 0 (hel-upper-bound [] 1)))))

(ert-deftest hel-bound-key ()
  (let ((vec [(1 . a) (3 . b) (5 . c)]))
    (should (= 1 (hel-lower-bound vec 2 :key #'car)))
    (should (= 2 (hel-upper-bound vec 3 :key #'car)))))

;;; Match indices

(defmacro hel-search-tests--with-matches (&rest body)
  "Run BODY in a buffer \"foo bar foo baz foo\" with `matches' bound.
`matches' is a vector of overlays on the three \"foo\": [1,4) [9,12) [17,20)."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "foo bar foo baz foo")
     (let ((matches (vector (make-overlay 1 4)
                            (make-overlay 9 12)
                            (make-overlay 17 20))))
       (ignore matches)
       ,@body)))

(defun hel-search-tests--cached-index (matches direction pos)
  "Return the index of the match that a session with MATCHES finds.
The session searches from POS in DIRECTION over its cached vector."
  (let ((ss (hel-search-session--create :buffer (current-buffer)
                                        :matches matches)))
    (car (hel-search-session-next-match-from-pos ss pos direction))))

(ert-deftest hel-search-cached-next ()
  "The next match is the first one that starts at or after POS."
  (hel-search-tests--with-matches
    (should (= 0 (hel-search-tests--cached-index matches 1 1)))
    (should (= 1 (hel-search-tests--cached-index matches 1 4)))
    ;; A match that starts at POS counts.
    (should (= 1 (hel-search-tests--cached-index matches 1 9)))
    (should (= 2 (hel-search-tests--cached-index matches 1 10)))
    ;; Wrap to the first match.
    (should (= 0 (hel-search-tests--cached-index matches 1 18)))))

(ert-deftest hel-search-cached-previous ()
  "The previous match is the last one that ends at or before POS."
  (hel-search-tests--with-matches
    (should (= 2 (hel-search-tests--cached-index matches -1 20)))
    (should (= 1 (hel-search-tests--cached-index matches -1 17)))
    ;; A match that ends at POS counts.
    (should (= 1 (hel-search-tests--cached-index matches -1 12)))
    (should (= 0 (hel-search-tests--cached-index matches -1 11)))
    ;; Wrap to the last match.
    (should (= 2 (hel-search-tests--cached-index matches -1 3)))))

(ert-deftest hel-search-cached-adjacent ()
  "Adjacent matches: each step goes to the neighbour, none is skipped."
  (with-temp-buffer
    (insert "aaa")
    (let ((matches (vector (make-overlay 1 2)
                           (make-overlay 2 3)
                           (make-overlay 3 4))))
      ;; The middle match is selected: the region is [2,3).
      (should (= 0 (hel-search-tests--cached-index matches -1 2)))
      (should (= 2 (hel-search-tests--cached-index matches 1 3))))))

(ert-deftest hel-search-match-at-pos ()
  "The match at POS is the one that contains POS, edges included."
  (hel-search-tests--with-matches
    (cl-flet ((current (matches pos)
                (hel-search-session-match-at-pos
                 (hel-search-session--create :buffer (current-buffer)
                                             :matches matches)
                 pos)))
      (should (equal (current matches 1) '(0 1 4)))
      (should (equal (current matches 4) '(0 1 4)))
      (should (equal (current matches 10) '(1 9 12)))
      (should (equal (current matches 20) '(2 17 20)))
      (should-not (current matches 6))
      (should-not (current [] 6))
      ;; The scan is incomplete.
      (should-not (current nil 6)))))

(ert-deftest hel-search-counter ()
  "The counter shows the match at point, the total alone, or nothing."
  (hel-search-tests--with-matches
    (let ((hel-search--session
           (hel-search-session--create :buffer (current-buffer)
                                       :tick (buffer-chars-modified-tick)
                                       :matches matches)))
      (goto-char 10)
      (should (equal (hel-search-counter) '(2 . 3)))
      ;; Between matches only the total remains.
      (goto-char 6)
      (should (equal (hel-search-counter) '(nil . 3)))
      ;; After an edit the counter is hidden.
      (goto-char 10)
      (insert "x")
      (should-not (hel-search-counter)))))

;;; hel-matches-in-range

(ert-deftest hel-matches-in-range-all ()
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (should (equal (hel-matches-in-range "foo" 1 20 100)
                   '(((1 . 4) (9 . 12) (17 . 20)) . 20)))))

(ert-deftest hel-matches-in-range-limit ()
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (should (equal (hel-matches-in-range "foo" 1 20 2)
                   '(((1 . 4) (9 . 12)) . 12)))
    ;; The next chunk resumes at STOP.
    (should (equal (hel-matches-in-range "foo" 12 20 2)
                   '(((17 . 20)) . 20)))))

(ert-deftest hel-matches-in-range-bounds ()
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (should (equal (hel-matches-in-range "foo" 2 19 100)
                   '(((9 . 12)) . 19)))))

(ert-deftest hel-matches-in-range-group ()
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (should (equal (car (hel-matches-in-range "b\\(a\\)" 1 20 100))
                   '((6 . 7) (14 . 15))))
    ;; Group 1 that does not take part in the match falls back to group 0.
    (should (equal (car (hel-matches-in-range "\\(x\\)?\\(bar\\)" 1 20 100))
                   '((5 . 8))))))

(ert-deftest hel-matches-in-range-zero-length ()
  (with-temp-buffer
    (insert "a\nb\nc")
    (should (equal (hel-matches-in-range "^" 1 6 100)
                   '(nil . 6)))
    ;; An empty match at the end of the buffer ends the scan.
    (should (equal (hel-matches-in-range "$" 1 6)
                   '(nil . 6)))
    ;; An empty match at TO is the last one: STOP does not pass TO.
    (should (equal (hel-matches-in-range "$" 1 2 1)
                   '(nil . 2)))
    ;; Empty matches are skipped, non-empty ones are kept.
    (should (equal (car (hel-matches-in-range "b*" 1 6 100))
                   '((3 . 4))))))

(ert-deftest hel-matches-in-range-invisible ()
  (with-temp-buffer
    (insert "foo bar foo baz foo")
    (setq buffer-invisibility-spec '(t))
    ;; Folded: the overlay can be opened, so the match is kept.
    (-doto (make-overlay 9 12)
      (overlay-put 'invisible t)
      (overlay-put 'isearch-open-invisible #'delete-overlay))
    ;; Truly invisible: the text property cannot be opened.
    (put-text-property 17 20 'invisible t)
    (should (equal (car (hel-matches-in-range "foo" 1 20 100))
                   '((1 . 4) (9 . 12))))))

;;; hel-search

(ert-deftest hel-search-group-visibility ()
  "The bounds that `hel-match' returns decide visibility, not the whole match."
  (with-temp-buffer
    (insert "xfoo yfoo")
    (setq buffer-invisibility-spec '(t))
    ;; Only the "x" is hidden: the group "foo" is visible.
    (put-text-property 1 2 'invisible t)
    ;; In the second match the group itself is hidden.
    (put-text-property 7 10 'invisible t)
    (goto-char (point-min))
    (should (equal (hel-match (car (hel-search "[xy]\\(foo\\)")))
                   '(2 . 5)))
    (should-not (hel-search "[xy]\\(foo\\)"))))

(ert-deftest hel-search-zero-length ()
  "Zero-length matches are skipped, and the search ends."
  (with-temp-buffer
    (insert "a\nb\nc")
    (goto-char (point-min))
    (should-not (hel-search "^"))
    (should (= (point) (point-max)))
    (should-not (hel-search "$" nil -1))
    (should (= (point) (point-min)))
    ;; A bound stops the stepping too.
    (should-not (hel-search "^" 3))
    (should (= (point) 3))))

;;; hel-complement-ranges

(ert-deftest hel-complement-ranges ()
  (should (equal (hel-complement-ranges '((1 . 4) (9 . 12) (17 . 20)) 1 20)
                 '((4 . 9) (12 . 17))))
  (should (equal (hel-complement-ranges '((3 . 5) (8 . 9)) 1 10)
                 '((1 . 3) (5 . 8) (9 . 10))))
  ;; Adjacent ranges leave no empty part between them.
  (should (equal (hel-complement-ranges '((2 . 3) (3 . 4)) 1 6)
                 '((1 . 2) (4 . 6))))
  ;; Without ranges the whole region remains.
  (should (equal (hel-complement-ranges nil 1 20)
                 '((1 . 20)))))

;;; Session

(ert-deftest hel-search-session-scan ()
  "Run the chunked scan by hand, without timers or a window."
  (with-temp-buffer
    (dotimes (_ 50) (insert "foo bar\n"))
    (let ((hel-search-max-at-a-time 7)
          (ss (hel-search-session--create :regexp "foo"
                                          :buffer (current-buffer)
                                          :tick (buffer-chars-modified-tick)))
          window-matches)
      (unwind-protect
          (cl-letf (((symbol-function 'run-at-time) #'ignore))
            ;; Pretend that the window shows lines 20 to 29.
            (setq window-matches
                  (-map #'hel-search-session--highlight
                        (car (hel-matches-in-range
                              "foo" (1+ (* 19 8)) (1+ (* 29 8)) 1000))))
            (setf (hel-search-session-window-range ss)
                  (cons (1+ (* 19 8)) (1+ (* 29 8))))
            (setf (hel-search-session-window-matches ss) window-matches)
            (dotimes (_ 10)
              (unless (hel-search-session-matches ss)
                (hel-search-session--scan-buffer ss)))
            (let* ((matches (append (hel-search-session-matches ss) nil))
                   (starts (-map #'overlay-start matches)))
              (should (= 50 (length matches)))
              (should (= 50 (length (overlays-in (point-min) (point-max)))))
              (should (-all? (lambda (ov) (memq ov matches)) window-matches))
              (should (equal starts (-sort #'< starts))))
            ;; Cached queries wrap around at both ends.
            (should (equal (hel-search-session-next-match-from-pos ss 10 1)
                           '(2 17 20 nil)))
            (should (= 0 (car (hel-search-session-next-match-from-pos
                               ss (point-max) 1))))
            (should (= 49 (car (hel-search-session-next-match-from-pos ss 3 -1))))
            ;; An edit makes the session stale: the next chunk restarts it.
            (goto-char (point-min))
            (insert "x")
            (hel-search-session--scan-buffer ss)
            (should-not (hel-search-session-matches ss))
            (should-not (overlays-in (point-min) (point-max)))
            ;; While the scan runs, the query searches the buffer.
            (should (equal (hel-search-session-next-match-from-pos ss 1 -1)
                           '(nil 394 397 nil))))
        (setf (hel-search-session-timer ss) nil)
        (hel-search-session-cleanup ss)))))

(ert-deftest hel-search-session-scan-skips-window ()
  "The buffer scan steps over the window region instead of searching it."
  (with-temp-buffer
    (dotimes (_ 50) (insert "foo bar\n"))
    (let ((hel-search-max-at-a-time 7)
          (ss (hel-search-session--create :regexp "foo"
                                          :buffer (current-buffer)
                                          :tick (buffer-chars-modified-tick))))
      (unwind-protect
          (cl-letf (((symbol-function 'run-at-time) #'ignore))
            ;; A window region over lines 20 to 29 without window overlays:
            ;; a scan that searched the region again would find 10 more.
            (setf (hel-search-session-window-range ss)
                  (cons (1+ (* 19 8)) (1+ (* 29 8))))
            (dotimes (_ 10)
              (unless (hel-search-session-matches ss)
                (hel-search-session--scan-buffer ss)))
            (should (= 40 (length (hel-search-session-matches ss))))
            (should-not (hel-search-session-window-range ss)))
        (setf (hel-search-session-timer ss) nil)
        (hel-search-session-cleanup ss)))))

(ert-deftest hel-search-session-cleanup-window ()
  "Cleanup deletes the window overlays that the buffer scan has not reached."
  (with-temp-buffer
    (insert "foo bar foo")
    (let ((ss (hel-search-session--create :regexp "foo"
                                          :buffer (current-buffer))))
      (setf (hel-search-session-window-matches ss)
            (list (hel-search-session--highlight (cons 9 12))))
      (hel-search-session-cleanup ss)
      (should-not (overlays-in (point-min) (point-max))))))

(provide 'hel-search-tests)
;;; hel-search-tests.el ends here
