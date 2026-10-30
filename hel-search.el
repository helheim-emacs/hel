;;; hel-search.el --- Search related functionality -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.13.0
;; Homepage: https://github.com/helheim-emacs/hel
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; Hel is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Hel is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; - /, ?, n, N, *, M-* — Search engine.
;; - s, S — Select in selections or split them on a pattern.
;; - K, M-K — Keep or remove the selections that match
;; - f, F, t, T — Jump to char with repeat
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'pcre2el)
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-multiple-cursors-core)
(require 'hel-core)

;;; Utils

(defun hel-read-regexp (prompt)
  (let ((message-log-max nil)
        (history-add-new-input nil))
    (read-string prompt nil 'hel-regex-history nil t)))

(defun hel-add-to-regex-history (regex)
  (when (length> regex 2) ;; Reduce the noise level.
    (let ((history-delete-duplicates t))
      (add-to-history 'hel-regex-history regex hel-regex-history-max)))
  (set-register '/ regex)
  (message "Register / set: %s" regex))

(defun hel-last-search-pattern ()
  "Return regexp from \"/\" register."
  (if-let* ((pattern (get-register '/))
            ((stringp pattern))
            ((not (string-empty-p pattern))))
      (hel-pcre-to-elisp pattern)
    (user-error "Register / is empty")))

(cl-defun hel-match (&optional (match-data (match-data t)))
  "Return cons cell with bounds of the first match group in `match-data'.
If there were no match groups in the last used regexp — return the bounds
of the full regexp match."
  (setq match-data (-partition 2 match-data))
  ;; Group 1 is nil when it did not take part in the match.
  (-let [(start end) (if (car (-second-item match-data))
                         (-second-item match-data)
                       (-first-item match-data))]
    (cons start end)))

(cl-defun hel-search (regexp &optional bound (direction 1))
  "Find the first visible match for the REGEXP toward the DIRECTION.
A match is visible when the bounds that `hel-match' returns for it are.
Return list (MATCH-DATA OVERLAYS) where:
- MATCH-DATA is the same as `match-data' returns with integer positions;
- OVERLAYS is a list with openable overlays that currently hide the match.

This function modifies the match data that `match-beginning',
`match-end' and `match-data' access."
  (or bound (setq bound (if (< 0 direction) (point-max) (point-min))))
  (let (found)
    (while (and (not found)
                (/= (point) bound)
                (re-search-forward regexp bound t direction))
      (if (= (match-beginning 0) (match-end 0))
          ;; Step over a zero-length match like "^" or "$" to ensure
          ;; progress: the search finds it again at the same point.
          (unless (= (point) bound)
            (forward-char direction))
        (-let* ((mdata (match-data t))
                ((beg . end) (hel-match mdata)))
          (when-let* ((visible (hel-range-visible? beg end)))
            (setq found (list mdata (if (consp visible)
                                        visible)))))))
    found))

(cl-defun hel-matches-in-region ( regexp from to
                                  &optional (max-matches most-positive-fixnum))
  "Find the visible matches of REGEXP in the current buffer from FROM to TO.
MAX-MATCHES limits how many regexp matches, visible or not, will be
examined. It is unlimited by default.

Return (RANGES . STOP) cons-cell, where:
- RANGES is a list of (START . END) positions of the visible non-empty
  matches in buffer order, as `hel-match' returns them;
- STOP is the position where the scan ended when it examined MAX-MATCHES
  matches.  STOP equals TO when the range is exhausted."
  (save-excursion
    (goto-char from)
    (let ((n 0)
          ranges)
      (while (and (< n max-matches)
                  (< (point) to)
                  (re-search-forward regexp to t))
        (cl-incf n)
        (if (= (match-beginning 0) (match-end 0))
            ;; Step over a zero-length match like "^" or "$" to ensure
            ;; forward progress.
            (unless (= (point) to)
              (forward-char 1))
          (-let [(beg . end) (hel-match)]
            (when (hel-range-visible? beg end)
              (push (cons beg end) ranges)))))
      (cons (nreverse ranges)
            (if (< n max-matches)
                to
              (point))))))

(defun hel-complement-ranges (ranges from to)
  "Return the parts between FROM and TO that RANGES do not cover.
RANGES is a list of (START . END) positions: sorted, non-overlapping,
and between FROM and TO. Empty parts are left out, so adjacent ranges
produce no part between them.

  from (3 . 5) (8 . 9) to  =>  (from . 3) (5 . 8) (9 . to)"
  (let (result)
    (dolist (range ranges)
      (when (< from (car range))
        (push (cons from (car range)) result))
      (setq from (cdr range)))
    (when (< from to)
      (push (cons from to) result))
    (nreverse result)))

;;; Search session

(cl-defstruct (hel-search-session (:constructor hel-search-session--create)
                                  (:predicate nil)
                                  (:copier nil))
  ;; Public
  regexp
  (callback nil :documentation "Executed when searching session is complete.")
  ;; Private
  (buffer   nil :read-only t)
  (tick     nil :documentation "`buffer-chars-modified-tick'")
  (timer    nil :documentation "Active timer for the scanning pipeline.")
  (scan-pos nil :documentation "Where the buffer scan left off.")
  (window-region   nil :documentation "(FROM . TO) positions that the window scan phase has searched.")
  (window-matches  nil :documentation "Overlays from the window scan phase, in buffer order.")
  (scanned-matches nil :documentation "Overlays from the buffer scan phase, last first.")
  (matches  nil :documentation "Sorted vector of all match overlays.
The scan is complete exactly when this slot holds a vector."))

(cl-defun hel-search-session-create ( &optional regexp
                                      &key (callback 'hel-search--show-counter))
  "Create `hel-search-session' object.
Run search session if REGEXP is provided."
  (let ((self (hel-search-session--create
               :regexp regexp
               :callback callback
               :buffer (current-buffer)
               :tick (buffer-chars-modified-tick))))
    (when regexp
      (setf (hel-search-session-timer self)
            (run-at-time nil nil #'hel-search-session--scan-window self)))
    self))

(defun hel-search-session-cleanup (self)
  "Destructor for `hel-search-session' objects."
  (-some-> (hel-search-session-timer self) (cancel-timer))
  (mapc #'delete-overlay (hel-search-session-window-matches self))
  (mapc #'delete-overlay (hel-search-session-scanned-matches self))
  (mapc #'delete-overlay (hel-search-session-matches self))
  (setf (hel-search-session-tick self)            nil
        (hel-search-session-timer self)           nil
        (hel-search-session-scan-pos self)        nil
        (hel-search-session-window-region self)   nil
        (hel-search-session-window-matches self)  nil
        (hel-search-session-scanned-matches self) nil
        (hel-search-session-matches self)         nil))

(defun hel-search-session-restart (self &optional regexp)
  (hel-search-session-cleanup self)
  (if regexp
      (setf (hel-search-session-regexp self) regexp)
    (setf regexp (hel-search-session-regexp self)))
  (when regexp
    (setf (hel-search-session-tick self)
          (buffer-chars-modified-tick (hel-search-session-buffer self)))
    (setf (hel-search-session-timer self)
          (run-at-time (if (length< regexp hel-search-no-delay-length)
                           hel-search-initial-delay)
                       nil 'hel-search-session--scan-window self))))

(defun hel-search-session--scan-window (self)
  (let* ((buffer (hel-search-session-buffer self))
         (window (get-buffer-window buffer t)))
    (cond ((not (buffer-live-p buffer))
           (hel-search-session-cleanup self))
          (window
           (with-current-buffer buffer
             (-let* ((start (window-group-start window))
                     (end   (min (window-group-end window t) (point-max)))
                     ((matches . _) (hel-matches-in-region
                                     (hel-search-session-regexp self)
                                     start end)))
               (setf (hel-search-session-tick self)
                     (buffer-chars-modified-tick))
               (setf (hel-search-session-window-region self)
                     (cons start end))
               (setf (hel-search-session-window-matches self)
                     (-map #'hel-search-session--highlight matches))
               (setf (hel-search-session-timer self)
                     (run-at-time hel-lazy-highlight-interval nil
                                  #'hel-search-session--scan-buffer self))))))))

(defun hel-search-session--scan-buffer (self)
  (let ((buffer (hel-search-session-buffer self)))
    (cond
     ((not (buffer-live-p buffer))
      (hel-search-session-cleanup self))
     ((/= (hel-search-session-tick self) (buffer-chars-modified-tick buffer))
      ;; The text has changed, so the overlays found so far are stale.
      (hel-search-session-restart self))
     (t
      (with-current-buffer buffer
        (-let* ((start (or (hel-search-session-scan-pos self)
                           (point-min)))
                (end   (point-max))
                ((win-start . win-end) (hel-search-session-window-region self))
                ;; Search only up to the region that the window scan
                ;; has searched.
                ((ranges . stop) (hel-matches-in-region
                                  (hel-search-session-regexp self)
                                  start
                                  (or win-start end)
                                  hel-search-max-at-a-time)))
          (dolist (range ranges)
            (push (hel-search-session--highlight range)
                  (hel-search-session-scanned-matches self)))
          (when (eql stop win-start)
            ;; The scan has reached the window region: take the overlays
            ;; of the window scan and resume after the region.
            (setf (hel-search-session-scanned-matches self)
                  (nconc (reverse (hel-search-session-window-matches self))
                         (hel-search-session-scanned-matches self)))
            (setf (hel-search-session-window-region self)  nil
                  (hel-search-session-window-matches self) nil)
            (setq stop win-end))
          (setf (hel-search-session-scan-pos self) stop)
          (if (< stop end)
              (setf (hel-search-session-timer self)
                    (run-at-time hel-lazy-highlight-interval nil
                                 #'hel-search-session--scan-buffer self))
            ;; Search finished.
            (setf (hel-search-session-timer self) nil)
            (setf (hel-search-session-matches self)
                  (-> (hel-search-session-scanned-matches self)
                      (nreverse)
                      (vconcat)))
            (setf (hel-search-session-scanned-matches self) nil)
            (-some-> (hel-search-session-callback self) (funcall self)))))))))

(cl-defun hel-search-session--highlight ((start . end))
  "Put a search highlighting overlay from START to END and return it."
  ;; Bug#77121: highlight overlays must be non-sticky at both ends.
  (-doto (make-overlay start end nil t nil)
    (overlay-put 'face 'hel-search-highlight)
    (overlay-put 'priority '(nil . 50))
    (overlay-put 'modification-hooks '(hel--delete-overlay-on-modification-h))))

(defun hel--delete-overlay-on-modification-h (ov flag _beg _end &optional _len)
  (if flag (delete-overlay ov)))

(defun hel-search-session-next-match (self direction pos)
  "Find the next search match from POS in DIRECTION.

Return (START END INDEX CLOSED-OVERLAYS) on success where:

- START, END       Match bounds.
- INDEX            Position of the match in the session's overlay vector,
                   or nil when this information is unavailable.
- CLOSED-OVERLAYS  Closed overlays that currently hide the match.

Return nil when no match exists."
  (let ((matches (hel-search-session-matches self)))
    (cond ((null matches)
           ;; The scan is still running.
           (hel-search-session--next-match-live self direction pos))
          ((< 0 (length matches))
           (hel-search-session--next-match-cached self direction pos)))))

(defun hel-search-session--next-match-live (self direction pos)
  "Search the buffer for the next match from POS in DIRECTION."
  (with-current-buffer (hel-search-session-buffer self)
    (save-excursion
      (goto-char pos)
      (if-let* ((regexp (hel-search-session-regexp self))
                (match (or (hel-search regexp nil direction)
                           (progn
                             (goto-char (if (< 0 direction) (point-min) (point-max)))
                             (hel-search regexp pos direction)))))
          (-let* (((match-data closed-overlays) match)
                  ((start . end) (hel-match match-data)))
            (list start end nil closed-overlays))))))

(defun hel-search-session--next-match-cached (self direction pos)
  "Find the next match from POS in DIRECTION."
  (with-current-buffer (hel-search-session-buffer self)
    (let* ((matches (hel-search-session-matches self))
           (index (if (< 0 direction)
                      ;; The first match that starts at or after POS.
                      ;; Past the last match `mod' wraps to the first one.
                      (mod (hel-lower-bound matches pos :key #'overlay-start)
                           (length matches))
                    ;; The last match that ends at or before POS.
                    ;; `mod' takes the sign of the divisor, so the index -1
                    ;; before the first match wraps to the last one.
                    (mod (1- (hel-upper-bound matches pos :key #'overlay-end))
                         (length matches))))
           (overlay (elt matches index))
           (start (overlay-start overlay))
           (end   (overlay-end overlay))
           (closed-overlays
            (->> (overlays-in start end)
                 (-filter (lambda (ov)
                            (and (invisible-p (overlay-get ov 'invisible))
                                 (overlay-get ov 'isearch-open-invisible)))))))
      (list start end index closed-overlays))))

(defun hel-search-session-current-match (self pos)
  "Return (START END INDEX) of the match at POS or nil."
  (with-current-buffer (hel-search-session-buffer self)
    (if-let* ((matches (hel-search-session-matches self))
              (index (hel-lower-bound matches pos :key #'overlay-end))
              ((< index (length matches)))
              (overlay (elt matches index))
              (start (overlay-start overlay))
              (end   (overlay-end overlay))
              ((<= start pos)))
        (list start end index))))

(defun hel-search--show-counter (search-session)
  "Show the match counter of SEARCH-SESSION in the mode line.
The counter follows point until `hel-search-stop' ends the session."
  (with-current-buffer (hel-search-session-buffer search-session)
    (hel-search--refresh-counter search-session)
    (add-hook 'post-command-hook #'hel-search--refresh-counter 95 t)))

(cl-defun hel-search--refresh-counter (&optional (search-session hel-search--session))
  "Show the number of matches of SEARCH-SESSION and the index of the match at point.
Show only the number of matches when point is inside no match, and
nothing while the scan of SEARCH-SESSION is incomplete or the buffer
text differs from the scanned one."
  (-let [(current . total)
         (if-let* ((ss search-session)
                   (matches (hel-search-session-matches ss))
                   ((< 0 (length matches)))
                   ;; After an edit the overlays may be deleted or stale.
                   ((eql (buffer-chars-modified-tick)
                         (hel-search-session-tick ss))))
             (cons (-if-let* (((_ _ index)
                               (hel-search-session-current-match ss (point))))
                       (1+ index))
                   (length matches)))]
    (unless (and (eql current hel-search--current)
                 (eql total hel-search--total))
      (setq hel-search--current current
            hel-search--total   total)
      (force-mode-line-update))))

(defun hel-search-stop ()
  "End the search session in the current buffer and clear the match counter."
  (remove-hook 'post-command-hook #'hel-search--refresh-counter t)
  (-some-> hel-search--session (hel-search-session-cleanup))
  (setq hel-search--session nil
        hel-search--current nil
        hel-search--total nil))

;;; Search: /, ?, n, N, *

(cl-defun hel-search-interactively (prompt &optional (direction 1))
  (redisplay) ; To ensure `window-start' position is not stale.
  (let* ((ss (hel-search-session-create))
         (point (point))
         (win-start (window-start))
         (win-hscroll (window-hscroll))
         target opened-overlays timer)
    (cl-flet*
        ((highlight (start end &optional overlay)
           (-doto (if overlay
                      (move-overlay overlay start end)
                    ;; Bug#77121: highlight overlays must be non-sticky at both ends.
                    (make-overlay start end nil t nil))
             (overlay-put 'face 'region)
             (overlay-put 'priority 100)))
         (update (regexp)
           (with-minibuffer-selected-window
             (hel-recenter-point-on-jump
               (goto-char point)
               (set-window-start nil win-start :noforce)
               (set-window-hscroll nil win-hscroll)
               (if-let* ((regexp)
                         (match (progn
                                  (hel-search-session-restart ss regexp)
                                  (hel-search-session-next-match ss direction (point)))))
                   (-let [(start end _ closed-overlays) match]
                     (goto-char (if (< direction 0) start end))
                     (setq target (highlight start end target))
                     (-each closed-overlays #'hel-temporary-open-overlay)
                     (cl-callf append opened-overlays closed-overlays))
                 ;; else
                 (-some-> target (delete-overlay))
                 (hel-search-session-cleanup ss)
                 (setq target nil
                       hel-search--total nil
                       hel-search--current nil)
                 (let ((message-log-max nil))
                   (message (propertize "No matches" 'face 'error)))))))
         (refresh (input)
           (update (unless (string-empty-p input)
                     (hel-pcre-to-elisp input))))
         (after-change (_beg _end _len)
           ;; Update once Emacs has handled all pending input.  An input
           ;; method commits a character together with events that change
           ;; no text, so a skipped update would never run again.
           (when timer (cancel-timer timer))
           (setq timer (run-with-idle-timer 0 nil #'refresh
                                            (minibuffer-contents-no-properties))))
         (stop ()
           (when timer (cancel-timer timer)))
         (start ()
           (add-hook 'after-change-functions #'after-change nil t)
           (add-hook 'minibuffer-exit-hook #'stop nil t)))
      ;; main
      (deactivate-mark)
      (when-let* ((input (condition-case nil
                             (minibuffer-with-setup-hook #'start
                               (hel-read-regexp prompt))
                           (t
                            (hel-search-session-cleanup ss))))
                  ((stringp input))
                  ((not (string-empty-p input))))
        (hel-add-to-regex-history input)
        (let ((regexp (hel-pcre-to-elisp input)))
          (unless (equal regexp (hel-search-session-regexp ss))
            (update regexp)))
        (when target
          (hel-recenter-point-on-jump
            (hel-set-region (overlay-start target) (overlay-end target)))
          (delete-overlay target))
        (-let [(open close)
               (->> opened-overlays
                    (-separate (lambda (ov)
                                 (and (< (overlay-start ov) (point))
                                      (<= (point) (overlay-end ov))))))]
          (-each open #'hel-open-overlay)
          (-each close #'hel-close-temporary-opened-overlay))
        ss))))

;; /
(hel-define-command hel-search-forward ()
  "Search forward for an interactively entered regexp; select the first match."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (hel-search-stop)
  (setq hel-search--session (hel-search-interactively "/" 1)
        hel-search--direction 1
        this-command 'hel-search-forward))

;; ?
(hel-define-command hel-search-backward ()
  "Search backward for an interactively entered regexp; select the first match."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (hel-search-stop)
  (setq hel-search--session (hel-search-interactively "?" -1)
        hel-search--direction -1
        this-command 'hel-search-backward))

;; n
(hel-define-command hel-search-next (count)
  "Select next COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when-let* ((regexp (hel-last-search-pattern)))
    (if (memq last-command '(hel-search-forward    ; /
                             hel-search-backward   ; ?
                             hel-search-next       ; n
                             hel-search-previous)) ; N
        ;; After "?" command, "n" and "N" keys are swapped:
        ;; "n" jump backward, "N" — forward.
        (if (< hel-search--direction 0) (cl-callf - count))
      ;; else
      (setq hel-search--direction 1)
      (hel-push-point))
    ;;
    (if-let* ((ss hel-search--session))
        (unless (and (eql (hel-search-session-tick ss)
                          (buffer-chars-modified-tick))
                     (equal (hel-search-session-regexp ss)
                            regexp))
          (hel-search-session-restart ss regexp))
      (setq hel-search--session (hel-search-session-create regexp)))
    ;;
    (let ((region-dir (if (use-region-p) (hel-region-direction) 1)))
      (hel-recenter-point-on-jump
        (hel-motion-loop (dir count)
          (when-let* ((pos (if (use-region-p)
                               (if (< dir 0) (region-beginning) (region-end))
                             (point)))
                      (match (hel-search-session-next-match hel-search--session dir pos)))
            (-let [(start end _ closed-overlays) match]
              (when (and hel--extend-selection (use-region-p))
                (hel-create-fake-cursor-from-point))
              (hel-set-region start end region-dir)
              (-each closed-overlays #'hel-open-overlay))))))))

;; N
(hel-define-command hel-search-previous (count)
  "Select previous COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-search-next (- count)))

;; *
(hel-define-command hel-construct-search-pattern ()
  "Construct search pattern from all current selections and store it to / register.
Auto-detect word boundaries at the beginning and end of the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote-fn (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (open-word-boundary
                (cond ((= beg (pos-bol))
                       (->> (buffer-substring-no-properties beg (1+ beg))
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- beg) (1+ beg))
                            (string-match-p "[^[:word:]][[:word:]]")))))
               (close-word-boundary
                (cond ((= end (pos-eol))
                       (->> (buffer-substring-no-properties (1- end) end)
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- end) (1+ end))
                            (string-match-p "[[:word:]][^[:word:]]")))))
               (string (->> (buffer-substring-no-properties (point) (mark))
                            (funcall quote-fn))))
          (push (concat (if open-word-boundary "\\b")
                        string
                        (if close-word-boundary "\\b"))
                patterns))))
    (setq patterns (nreverse (-uniq patterns)))
    (when patterns
      (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
             (regexp (apply #'concat (-interpose separator patterns))))
        (hel-add-to-regex-history regexp)
        (hel-search-stop)
        (setq hel-search--session (hel-search-session-create
                                   (hel-pcre-to-elisp regexp)))))))

;; M-*
(hel-define-command hel-construct-search-pattern-no-bounds ()
  "Construct search pattern from all current selection and store it to / register.
Do not auto-detect word boundaries in the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (push (funcall quote (buffer-substring-no-properties (point) (mark)))
              patterns)))
    (cl-callf nreverse patterns)
    (when patterns
      (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
             (regexp (apply #'concat (-interpose separator patterns))))
        (hel-add-to-regex-history regexp)
        (hel-search-stop)
        (setq hel-search--session (hel-search-session-create
                                   (hel-pcre-to-elisp regexp)))))))

;;; Select: s, S

;; s
(hel-define-command hel-select-in-selections (&optional invert)
  "Create new selection withing current for all matches to the regexp."
  :multiple-cursors nil
  (interactive)
  (when (region-active-p)
    (let* ((cursors-positions (hel-cursors-positions))
           (regions (->> cursors-positions
                         (-map (-lambda ((_id point mark))
                                 (if (< point mark)
                                     (cons point mark)
                                   (cons mark point)))))))
      (deactivate-mark)
      (-each (hel-all-fake-cursors) #'hel--delete-fake-cursor)
      (setq hel--extend-selection nil)
      (if (setq regions (hel-select-interactively-in-regions regions invert))
          (hel-recenter-point-on-jump
            ;; Real cursor
            (-let [(start . end) (car regions)]
              (hel-set-region start end)
              (-each (overlays-in start end) #'hel-open-overlay))
            ;; Fake cursors
            (save-excursion
              (cl-loop for (start . end) in (cdr regions) do
                       (goto-char start)
                       (-each (overlays-in start end) #'hel-open-overlay)
                       (hel-create-fake-cursor end start))))
        ;; Else restore original cursors.
        (hel-place-cursors cursors-positions)))
    (hel-auto-multiple-cursors-mode)))

;; S
(hel-define-command hel-split-selections ()
  "Create new selections withing current for all regions that NOT match to
the regexp."
  :multiple-cursors nil
  (interactive)
  (hel-select-in-selections t))

(defun hel-select-interactively-in-regions (regions &optional invert)
  "Return ranges matching an interactively entered regexp within REGIONS.
REGIONS is a list of cons cells (START . END) that defines the limits within
which search is performed. With INVERT non-nil, return the complement ranges.
Shows a live preview, a match count, and scrolls the first match into view."
  (let ((mark-active nil) ; temporarily deactivate selection in this function body
        (start-pos (->> regions (-map #'car) (apply #'min)))
        (win-start (window-start))
        (win-hscroll (window-hscroll))
        ;; Closed overlays that hide REGIONS.
        (closed-overlays (->> regions
                              (-mapcat (-lambda ((beg . end))
                                         (overlays-in beg end)))
                              (-uniq)
                              (-filter (lambda (ov)
                                         (invisible-p (overlay-get ov 'invisible))))))
        hl-overlays
        timer)
    (cl-flet*
        ((ranges-for (regexp)
           (let ((matches (-map (-lambda ((from . to))
                                  (car (hel-matches-in-region regexp from to)))
                                regions)))
             ;; Return nil when REGEXP matches in no region. The complement is
             ;; never empty, so test the matches before taking it.
             (when (-any? #'identity matches)
               (if invert
                   (->> (-zip-with (cl-function
                                    (lambda (ranges (from . to))
                                      (hel-complement-ranges ranges from to)))
                                   matches regions)
                        (-flatten-n 1))
                 (-flatten-n 1 matches)))))
         (highlight ((start . end))
           (-doto (make-overlay start end nil t nil)
             (overlay-put 'face 'region)
             (overlay-put 'priority 100)))
         (update (regexp)
           (with-minibuffer-selected-window
             (goto-char start-pos)
             (set-window-start nil win-start :noforce)
             (set-window-hscroll nil win-hscroll)
             (if-let* ((regexp)
                       (ranges (ranges-for regexp)))
                 (hel-recenter-point-on-jump
                   (goto-char (->> ranges (-map #'car) (apply #'min)))
                   (setq hl-overlays (-map #'highlight ranges)))
               ;; else no matches
               (setq hl-overlays (-map #'highlight regions))
               (let ((message-log-max nil))
                 (message (propertize "No matches" 'face 'error))))))
         (refresh (input)
           (-each hl-overlays #'delete-overlay)
           (update (unless (string-empty-p input)
                     (hel-pcre-to-elisp input))))
         (after (_beg _end _len)
           ;; Update once Emacs has handled all pending input.  An input
           ;; method commits a character together with events that change
           ;; no text, so a skipped update would never run again.
           (when timer (cancel-timer timer))
           (setq timer (run-with-idle-timer 0 nil #'refresh
                                            (minibuffer-contents-no-properties))))
         (stop ()
           (when timer (cancel-timer timer))
           (-each hl-overlays #'delete-overlay)
           (-each closed-overlays #'hel-close-temporary-opened-overlay))
         (start ()
           (-each closed-overlays #'hel-temporary-open-overlay)
           (setq hl-overlays (-map #'highlight regions))
           (add-hook 'after-change-functions #'after nil t)
           (add-hook 'minibuffer-exit-hook #'stop nil t)))
      ;; main body
      (when-let* ((regexp (condition-case nil
                             (minibuffer-with-setup-hook #'start
                               (hel-read-regexp (if invert "split: " "select: ")))
                           (quit))) ;; "C-g"
                  ((stringp regexp))
                  ((not (string-empty-p regexp))))
        (hel-add-to-regex-history regexp)
        (ranges-for (hel-pcre-to-elisp regexp))))))

;;; Filter: K, M-K

;; K
(hel-define-command hel-keep-selections ()
  "Keep selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (hel-filter-selections))

;; M-K
(hel-define-command hel-remove-selections ()
  "Remove selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (hel-filter-selections t))

(defun hel-filter-selections (&optional invert)
  "Keep selections that match regexp entered.
If INVERT is non-nil — remove selections that match regexp."
  (unless hel-multiple-cursors-mode
    (user-error "No multiple selections"))
  (hel-with-real-cursor-as-fake
    (let* ((cursors (hel-all-fake-cursors))
           (regions-overlays (-map (lambda (cursor)
                                     (overlay-get cursor 'fake-region))
                                   cursors))
           (regions-content (-map (lambda (cursor)
                                    (buffer-substring-no-properties
                                     (overlay-get cursor 'point)
                                     (overlay-get cursor 'mark)))
                                  cursors))
           timer)
      (cl-flet*
          ((update (regexp)
             (if-let* ((regexp)
                       (flags (-map (lambda (str)
                                      (xor (string-match regexp str)
                                           invert))
                                    regions-content))
                       ((-any #'identity flags)))
                 (-each (-zip-pair regions-overlays flags)
                   (-lambda ((overlay . flag))
                     (overlay-put overlay 'face (if flag 'region))))
               ;; else no matches
               (let ((message-log-max nil))
                 (message (propertize "No matches" 'face 'error)))
               (-each regions-overlays (lambda (ov)
                                         (overlay-put ov 'face 'region)))))
           (refresh (input)
             (update (unless (string-empty-p input)
                       (hel-pcre-to-elisp input))))
           (after (_beg _end _len)
             ;; Update once Emacs has handled all pending input.  An input
             ;; method commits a character together with events that change
             ;; no text, so a skipped update would never run again.
             (when timer (cancel-timer timer))
             (setq timer (run-with-idle-timer 0 nil #'refresh
                                              (minibuffer-contents-no-properties))))
           (stop ()
             (when timer (cancel-timer timer)))
           (start ()
             (add-hook 'after-change-functions #'after nil t)
             (add-hook 'minibuffer-exit-hook #'stop nil t)))
        ;; main
        (deactivate-mark)
        (-each cursors #'delete-overlay)
        (if-let* ((input (condition-case nil
                             (minibuffer-with-setup-hook #'start
                               (hel-read-regexp (if invert "remove: " "keep: ")))
                           (quit)))
                  ((stringp input))
                  ((not (string-empty-p input)))
                  (regexp (hel-pcre-to-elisp input))
                  (flags (-map (lambda (str)
                                 (xor (string-match regexp str)
                                      invert))
                               regions-content))
                  ((-any #'identity flags)))
            (-each (-zip-pair cursors flags)
              (-lambda ((cursor . flag))
                (if (not flag)
                    (hel--delete-fake-cursor cursor)
                  (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
                  (-> (overlay-get cursor 'fake-region)
                      (overlay-put 'face 'region)))))
          ;; Else restore all cursors
          (dolist (cursor cursors)
            (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
            (-> (overlay-get cursor 'fake-region)
                (overlay-put 'face 'region))))))))

;;; Find char: f F t T

;; f
(hel-define-command hel-find-char-forward ()
  "Prompt user for CHAR and move to the next occurrence of it.
Right after this command while hints are active, you can use n and N
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "f" t) 1 nil))

;; F
(hel-define-command hel-find-char-backward ()
  "Prompt user for CHAR and move to the previous occurrence of it.
Right after this command while hints are active, you can use n and N
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "F" t) -1 nil))

;; t
(hel-define-command hel-till-char-forward ()
  "Prompt user for CHAR and move before the next occurrence of it.
Right after this command while hints are active, you can use n and N
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "t" t) 1 t))

;; T
(hel-define-command hel-till-char-backward ()
  "Prompt user for CHAR and move before the previous occurrence of it.
Right after this command while hints are active, you can use n and N
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "T" t) -1 t))

(defun hel-find-char (char direction exclusive?)
  (let* ((case (let (case-fold-search)
                 (not (string-match-p "[A-Z]" (char-to-string char)))))
         (case-fold-search case)
         (deactivate-mark nil)
         (regexp (pcase char
                   (?\t "\t") ;; TAB
                   ((or ?\r ?\n) "\n") ;; RET
                   ;; (?\e) ;; ESC
                   ;; (?\d) ;; DEL <backspace>
                   ;; (_ (char-fold-to-regexp (char-to-string char)))
                   (_ (regexp-quote (char-to-string char)))))
         overlays)
    (cl-flet*
        ((next (&optional (dir direction))
           (interactive)
           (if exclusive?
               (cond ((<= 0 dir direction) ;; t n
                      (forward-char))
                     ((<= dir direction 0) ;; T n
                      (backward-char)))
             ;; else
             (cond ((< dir 0 direction) ;; f N
                    (backward-char))
                   ((< direction 0 dir) ;; F N
                    (forward-char))))
           (-each overlays #'delete-overlay)
           (when (hel-find-char--search regexp dir)
             (setq overlays (hel-find-char--highlight regexp dir))
             (if exclusive?
                 (cond ((<= 0 dir direction) ;; t n
                        (backward-char))
                       ((<= dir direction 0) ;; T n
                        (forward-char)))
               ;; not exclusive?
               (cond ((< dir 0 direction) ;; f N
                      (forward-char))
                     ((< direction 0 dir) ;; F N
                      (backward-char))))
             ;; Open fold at the landing position.
             (-each (overlays-at (point)) #'hel-open-overlay)
             t))
         (previous ()
           (interactive)
           (next (- direction)))
         (on-exit ()
           (-each overlays #'delete-overlay)))
      ;; main
      (when (next direction)
        (set-transient-map (define-keymap
                             "n" #'next
                             "N" #'previous)
                           t #'on-exit)))))

(defun hel-find-char--search (regexp dir)
  (let ((limit (if (< dir 0) (window-start) (window-end)))
        (found nil))
    (while (and (not found)
                (re-search-forward regexp limit t dir))
      (if (hel-range-visible? (match-beginning 0) (match-end 0))
          (setq found t)))
    found))

(defun hel-find-char--highlight (regexp dir)
  (save-excursion
    (save-match-data
      (let ((limit (if (< dir 0) (window-start) (window-end)))
            (i 0)
            overlays)
        (while (and (< i 16)
                    (re-search-forward regexp limit t dir))
          (unless (invisible-p (point))
            (cl-incf i)
            (push (-doto (make-overlay (match-beginning 0) (match-end 0) nil t nil)
                    (overlay-put 'face 'hel-search-highlight))
                  overlays)))
        overlays))))

;;; .
(provide 'hel-search)
;;; hel-search.el ends here
