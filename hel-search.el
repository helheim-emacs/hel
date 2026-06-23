;;; hel-search.el --- Search related functionality -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.10.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'hel-vars)
(require 'hel-common)
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

(cl-defun hel-match (&optional (match-data (match-data)))
  "Return cons cell with bounds of the first match group in `match-data'.
If there were no match groups in the last used regexp — return the bounds
of the full regexp match."
  (setq match-data (-partition 2 match-data))
  (-let [(start end) (or (-second-item match-data)
                         (-first-item match-data))]
    (cons start end))
  ;; (set-match-data match-data)
  ;; (if (match-beginning 1)
  ;;     (cons (match-beginning 1) (match-end 1))
  ;;   (cons (match-beginning 0) (match-end 0)))
  )

(cl-defun hel-search (regexp &optional bound (direction 1))
  "Find the first match for the REGEXP toward the DIRECTION.
Return list (MATCH-DATA OVERLAYS) where:
- MATCH-DATA is the same as `match-data' returns;
- OVERLAYS is a list with openable overlays that currently hide the match.

This function modifies the match data that `match-beginning',
`match-end' and `match-data' access."
  (let (found)
    (while (and (not found)
                (re-search-forward regexp bound t direction))
      (when-let* ((val (hel-range-visible? (match-beginning 0) (match-end 0))))
        (setq found (list (match-data) (if (consp val)
                                           val)))))
    found))

(defun hel-put-highlight (start end face &optional priority overlay)
  (-doto (if overlay
             (move-overlay overlay start end)
           ;; Bug#77121: highlight overlays must be non-sticky at both ends.
           (make-overlay start end nil t nil))
    (overlay-put 'face face)
    (overlay-put 'priority priority)))

(defun hel-search-highlight-all-matches (match-data &optional overlays)
  "Highlight all submatches in MATCH-DATA.
Return list with overlays. Reuse OVERLAYS if provided.

The faces used to do the highlights are named `isearch-group-1', `isearch-group-2',
etc. (By default, only these 2 are defined.) When there are more matches than
faces, then faces are reused from the beginning, in a cyclical manner, so the
`isearch-group-1' face is isreused for the third match. If you want to use more
distinctive colors, you can define more of these faces using the same numbering
scheme."
  (setq match-data (-partition 2 match-data))
  (prog1 (cons
          ;; Whole match
          (-let [(start end) (car match-data)]
            (hel-put-highlight start end 'isearch '(nil . 51)
                               (if overlays (pop overlays))))
          ;; Submatches
          (let ((group 0))
            (->> (cdr match-data)
                 (-map (-lambda ((start end))
                         (when (and (integer-or-marker-p start)
                                    (integer-or-marker-p end))
                           (let* ((next (intern-soft (format "isearch-group-%d"
                                                             (cl-incf group))))
                                  (face (if (facep next)
                                            next
                                          (setq group 0)
                                          'isearch-group-1)))
                             (hel-put-highlight start end face '(nil . 52)
                                                (if overlays (pop overlays))))))))))
    ;; Remove remaining overlays
    (-each overlays #'delete-overlay)))

(defun hel-invert-ranges (ranges start end)
  "Return the list with complements to RANGES withing [START, END] positions.

  ((1 . 2) (3 . 4) (5 . 6))  =>  ((start . 1) (2 . 3) (4 . 5) (6 . end))

RANGES is a list of cons cells with positions (START . END)."
  (when ranges
    (let (result)
      (dolist (range ranges)
        (when (< start (car range))
          (push (cons start (car range)) result))
        (setq start (cdr range)))
      (when (< start end)
        (push (cons start end) result))
      (nreverse result))))

;;; Search session

(cl-defstruct (hel-search-session (:constructor hel-search-session--create)
                                  (:predicate nil)
                                  (:copier nil))
  ;; Public
  regexp
  (start    nil :documentation "Limit that bounds the search area.")
  (end      nil :documentation "Limit that bounds the search area.")
  (callback nil :documentation "Executed when searching session is complete.")
  ;; Private
  (counter  0   :documentation "Running counter.")
  (total    nil :documentation "Total matches; nil while scanning in progress.")
  (timer    nil :documentation "Active timer for the scanning pipeline.")
  (scan-pos nil :documentation "Where the buffer scan left off.")
  ;; It is a list during scanning and a sorted vector after the scan completes.
  (overlays nil :documentation "Search matches overlays.")
  (buffer nil :read-only t)
  (buffer-hash nil))

(cl-defun hel-search-session-create ( &optional regexp start end
                                      &key callback)
  "Create `hel-search-session' object.
Run search session if REGEXP is provided."
  (let ((self (hel-search-session--create
               :regexp regexp
               :start start :end end
               :callback (or callback 'hel-search-session--update-modeline)
               :buffer (current-buffer)
               :buffer-hash (buffer-hash))))
    (when regexp
      (setf (hel-search-session-timer self)
            (run-at-time nil nil 'hel-search-session--scan-window self)))
    self))

(defun hel-search-session-cleanup (self)
  "Destructor for `hel-search-session' objects."
  (-some-> (hel-search-session-timer self) (cancel-timer))
  ;; The overlays slot may be either a list (during a scan) or a vector (after
  ;; scan is completed); `mapc' iterates both.
  (mapc #'delete-overlay (hel-search-session-overlays self))
  (setf (hel-search-session-counter self)     0
        (hel-search-session-total self)       nil
        (hel-search-session-scan-pos self)    nil
        (hel-search-session-overlays self)    nil
        (hel-search-session-timer self)       nil
        (hel-search-session-buffer-hash self) nil))

(defun hel-search-session-restart (self &optional regexp)
  (hel-search-session-cleanup self)
  (if regexp
      (setf (hel-search-session-regexp self) regexp)
    (setf regexp (hel-search-session-regexp self)))
  (when regexp
    (setf (hel-search-session-buffer-hash self)
          (buffer-hash (hel-search-session-buffer self)))
    (setf (hel-search-session-timer self)
          (run-at-time (if (length< regexp hel-search-no-delay-length)
                           hel-search-initial-delay)
                       nil 'hel-search-session--scan-window self))))

(defun hel-search-session--scan-window (self)
  (if (not (buffer-live-p (hel-search-session-buffer self)))
      (hel-search-session-cleanup self)
    (with-current-buffer (hel-search-session-buffer self)
      (let ((regexp (hel-search-session-regexp self))
            (search-start (max (or (hel-search-session-start self) (point-min))
                               (window-group-start)))
            (search-end   (min (or (hel-search-session-end self) (point-max))
                               (window-group-end)))
            window-overlays)
        (save-excursion
          (goto-char search-start)
          (let (match)
            (while (setq match (hel-search regexp search-end))
              (-let* (((match-data _closed-overlays) match)
                      ((beg . end) (hel-match match-data)))
                (if (= beg end)
                    ;; Ensure forward progress on zero-length matches like
                    ;; "^" or "$" to avoid an infinite loop.
                    (unless (eobp) (forward-char 1))
                  (push (hel-search-session--highlight-overlay beg end)
                        window-overlays))))))
        (cl-callf nreverse window-overlays)
        (setf (hel-search-session-timer self)
              (run-at-time hel-lazy-highlight-interval nil
                           'hel-search-session--scan-buffer
                           self window-overlays))))))

(defun hel-search-session--scan-buffer (self window-overlays)
  (if (not (buffer-live-p (hel-search-session-buffer self)))
      (hel-search-session-cleanup self)
    (with-current-buffer (hel-search-session-buffer self)
      (let ((regexp (hel-search-session-regexp self))
            (start (or (hel-search-session-scan-pos self)
                       (hel-search-session-start self)
                       (point-min)))
            (end   (or (hel-search-session-end self) (point-max)))
            (n 0)
            match)
        (save-excursion
          (goto-char start)
          (while (and (< n hel-search-max-at-a-time)
                      (setq match (hel-search regexp end)))
            (-let* (((match-data _closed-overlays) match)
                    ((beg . end) (hel-match match-data)))
              (cl-incf n)
              (if (= beg end)
                  ;; Zero-width match: advance to avoid an infinite loop.
                  ;; Can be when regexp consists only of "^" or "$".
                  (unless (eobp) (forward-char 1))
                (if (and window-overlays
                         (= beg (overlay-start (car window-overlays))))
                    (progn
                      (cl-callf + (hel-search-session-counter self)
                                  (length window-overlays))
                      (setf (hel-search-session-overlays self)
                            (nconc (nreverse window-overlays)
                                   (hel-search-session-overlays self)))
                      (setq window-overlays nil)
                      (goto-char (-> (hel-search-session-overlays self)
                                     (-first-item)
                                     (overlay-end))))
                  ;; else
                  (cl-incf (hel-search-session-counter self))
                  (push (hel-search-session--highlight-overlay beg end)
                        (hel-search-session-overlays self))))))
          (setf (hel-search-session-scan-pos self) (point)))
        (if (>= n hel-search-max-at-a-time)
            ;; Limit hit: reschedule the next cycle.
            (setf (hel-search-session-timer self)
                  (run-at-time hel-lazy-highlight-interval nil
                               'hel-search-session--scan-buffer
                               self window-overlays))
          ;; Search finished.
          (setf (hel-search-session-timer self) nil
                (hel-search-session-total self) (hel-search-session-counter self))
          ;; Reverse list and convert it to vector.
          (setf (hel-search-session-overlays self)
                (vconcat (nreverse (hel-search-session-overlays self))))
          (-some-> (hel-search-session-callback self) (funcall self)))))))

(defun hel-search-session--highlight-overlay (start end)
  ;; Bug#77121: highlight overlays must be non-sticky at both ends.
  (-doto (make-overlay start end nil t nil)
    (overlay-put 'face 'lazy-highlight)
    (overlay-put 'priority '(nil . 50))
    (overlay-put 'modification-hooks '(hel--delete-overlay-on-modification-h))))

(defun hel--delete-overlay-on-modification-h (ov flag _beg _end &optional _len)
  (when flag
    (delete-overlay ov)))

;; TODO: docstring
(defun hel-search-session-next-match (self direction)
  "Find the next match from point in DIRECTION.
Return (START END OVERLAYS INDEX) list where:
- START, END are bounds of match;
- OVERLAYS is a list with openable overlays that currently hide the match.
- INDEX"
  (save-excursion
    (let ((pos (if (use-region-p)
                   (if (< direction 0) (region-beginning) (region-end))
                 (point)))
          (total (hel-search-session-total self)))
      (if total
          ;; Search session is complete.
          (unless (= total 0)
            (let* ((overlays (hel-search-session-overlays self))
                   (index (if (< 0 direction)
                              (hel-search-session--next-match self pos)
                            (hel-search-session--previous-match self pos)))
                   (overlay (elt overlays index))
                   (start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (closed-overlays (let ((val (hel-range-visible? start end)))
                                      (if (consp val) val))))
              (list start end closed-overlays index)))
        ;; else
        (if-let* ((regexp (hel-search-session-regexp self))
                  (match (or (hel-search regexp nil direction)
                             (progn
                               (goto-char (if (< 0 direction)
                                              (point-min)
                                            (point-max)))
                               (hel-search regexp pos direction)))))
            (-let* (((match-data closed-overlays) match)
                    ((start . end) (hel-match match-data)))
              (list start end closed-overlays nil)))))))

(defun hel-search-session--next-match (self pos)
  "Return the index of the next overlay that starts after POS."
  (let* ((overlays (hel-search-session-overlays self))
         (low 0)
         (high (hel-search-session-total self))
         (mid 0)
         (result 0))
    (while (< low high)
      (setq mid (/ (+ low high) 2))
      (if (<= pos (overlay-start (elt overlays mid)))
          (setq result mid
                high mid)
        (setq low (1+ mid))))
    result))

(defun hel-search-session--previous-match (self pos)
  "Return the index of the previous overlay that ends before POS."
  (let ((overlays (hel-search-session-overlays self))
        (low 0)
        (high (hel-search-session-total self))
        (mid 0)
        (result (1- (hel-search-session-total self))))
    (while (< low high)
      (setq mid (/ (+ low high) 2))
      (if (<= (overlay-end (elt overlays mid)) pos)
          (setq result mid
                low (1+ mid))
        (setq high mid)))
    result))

(defun hel-search-session--current-match (self pos)
  (let ((overlays (hel-search-session-overlays self))
        (low 0)
        (high (hel-search-session-total self))
        (mid 0)
        ov found)
    (while (and (not found) (< low high))
      (setq mid (/ (+ low high) 2)
            ov (elt overlays mid))
      (cond ((<= (overlay-start ov) pos (overlay-end ov))
             (setq found ov))
            ((< pos (overlay-start ov))
             (setq high mid))
            (t
             (setq low (1+ mid)))))
    (if found mid)))

(defun hel-search-session--update-modeline (ss)
  (with-current-buffer (hel-search-session-buffer ss)
    (if-let* ((total (hel-search-session-total ss))
              ((not (zerop total))))
        (progn
          (setq hel-search--total total
                hel-search--current (-some-> (hel-search-session--current-match ss (point))
                                      (1+)))
          (add-hook 'post-command-hook 'hel-search--clean-current 95 t))
      ;; else
      (setq hel-search--total nil
            hel-search--current nil))
    (force-mode-line-update)))

(defun hel-search--clean-current ()
  (if-let* ((current hel-search--current)
            (ss hel-search--session)
            (overlays (hel-search-session-overlays ss))
            (ov (elt overlays (1- current)))
            ((<= (overlay-start ov) (point) (overlay-end ov))))
      nil
    (remove-hook 'post-command-hook 'hel-search--clean-current t)
    (setq hel-search--current nil)
    (force-mode-line-update)))

;;; Search: /, ?, n, N, *

(cl-defun hel-search-interactively (prompt &optional (direction 1))
  (redisplay) ; To ensure `window-start' position is not stale.
  (let* ((ss (hel-search-session--create
              :buffer (current-buffer)
              :buffer-hash (buffer-hash)
              :callback #'hel-search-session--update-modeline))
         (point (point))
         (win-start (window-start))
         (win-hscroll (window-hscroll))
         target opened-overlays)
    (cl-flet*
        ((update (regexp)
           (with-minibuffer-selected-window
             (hel-recenter-point-on-jump
               (goto-char point)
               (set-window-start nil win-start :noforce)
               (set-window-hscroll nil win-hscroll)
               (if-let* ((regexp)
                         (match (progn
                                  (hel-search-session-restart ss regexp)
                                  (hel-search-session-next-match ss direction))))
                   (-let [(start end closed-overlays) match]
                     (goto-char (if (< direction 0) start end))
                     (setq target (hel-put-highlight start end 'region 100 target))
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
         (after-change (_beg _end _len)
           (hel-search-session-cleanup ss)
           (unless (input-pending-p)
             (update (let ((s (minibuffer-contents-no-properties)))
                       (unless (string-empty-p s)
                         (hel-pcre-to-elisp s))))))
         (start ()
           (add-hook 'after-change-functions #'after-change nil t)))
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
  (-some-> hel-search--session (hel-search-session-cleanup))
  (setq hel-search--current nil
        hel-search--total nil
        hel-search--session (hel-search-interactively "/" 1)
        hel-search--direction 1)
  (add-hook 'pre-command-hook 'hel-search--clean-current 95 t))

;; ?
(hel-define-command hel-search-backward ()
  "Search backward for an interactively entered regexp; select the first match."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (-some-> hel-search--session (hel-search-session-cleanup))
  (setq hel-search--current nil
        hel-search--total nil
        hel-search--session (hel-search-interactively "?" -1)
        hel-search--direction -1)
  (add-hook 'pre-command-hook 'hel-search--clean-current 95 t))

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
        (unless (and (equal (buffer-hash) (hel-search-session-buffer-hash ss))
                     (equal regexp (hel-search-session-regexp ss)))
          (hel-search-session-restart ss regexp))
      (setq hel-search--session (hel-search-session-create regexp)))
    ;;
    (let ((region-dir (if (use-region-p) (hel-region-direction) 1)))
      (hel-recenter-point-on-jump
        (hel-motion-loop (dir count)
          (when-let* ((match (hel-search-session-next-match hel-search--session dir)))
            (-let [(start end closed-overlays index) match]
              (when (and hel--extend-selection (use-region-p))
                (hel-create-fake-cursor-from-point))
              (hel-set-region start end region-dir)
              (-each closed-overlays #'hel-open-overlay)
              (setq hel-search--current (if index (1+ index)))
              (add-hook 'post-command-hook 'hel-search--clean-current 95 t))))))))

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
        (-some-> hel-search--session (hel-search-session-cleanup))
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
        (-some-> hel-search--session (hel-search-session-cleanup))
        (setq hel-search--session (hel-search-session-create
                                   (hel-pcre-to-elisp regexp)))))))

;;; Select: s, S

;; s
(hel-define-command hel-select-in-selections (&optional invert)
  "Create new selection withing current for all matches to the regexp."
  :multiple-cursors nil
  (interactive)
  (when (region-active-p)
    (let ((cursors-positions (hel-cursors-positions)))
      (hel-with-real-cursor-as-fake
        (let* ((cursors (hel-all-fake-cursors))
               (regions (->> cursors
                             (-map (lambda (cursor)
                                     (if (overlay-get cursor 'mark-active)
                                         (let ((point (-> cursor
                                                          (overlay-get 'point)
                                                          (marker-position)))
                                               (mark  (-> cursor
                                                          (overlay-get 'mark)
                                                          (marker-position))))
                                           (if (< point mark)
                                               (cons point mark)
                                             (cons mark point))))))
                             (delq nil)))
               ;; Closed overlays that overlap regions.
               (overlays (->> regions
                              (-mapcat (-lambda ((beg . end))
                                         (overlays-in beg end)))
                              (-uniq)
                              (-filter (lambda (ov)
                                         (invisible-p (overlay-get ov 'invisible)))))))
          (-each overlays #'hel-temporary-open-overlay)
          (-each cursors  #'hel--delete-fake-cursor)
          (setq hel--extend-selection nil)
          (if (setq regions (hel-select-interactively-in-regions regions invert))
              (progn
                (-each regions (-lambda ((mark . point))
                                 (hel-create-fake-cursor point mark)))
                (when overlays
                  ;; Sort overlays by starting position.
                  (setq overlays (sort overlays (lambda (ov1 ov2)
                                                  (< (overlay-start ov1)
                                                     (overlay-start ov2)))))
                  (-let [(overlap not-overlap) (hel-select--partition-overlays-by-regions
                                                overlays regions)]
                    (-each overlap #'hel-open-overlay)
                    (-each not-overlap #'hel-close-temporary-opened-overlay))))
            ;; Else restore original cursors.
            (hel-place-cursors cursors-positions)
            (-each overlays #'hel-close-temporary-opened-overlay)))))
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
        overlays)
    (cl-flet*
        ((highlight (bounds)
           (hel-put-highlight (car bounds) (cdr bounds) 'region 100))
         (update (regexp)
           (with-minibuffer-selected-window
             (goto-char start-pos)
             (set-window-start nil win-start :noforce)
             (set-window-hscroll nil win-hscroll)
             (if-let* ((regexp)
                       (ranges (->> regions
                                    (-mapcat (-lambda ((beg . end))
                                               (hel-select--search regexp beg end invert)))
                                    (delq nil))))
                 (hel-recenter-point-on-jump
                   ;; (unless (pos-visible-in-window-p min-pos))
                   (goto-char (->> ranges (-map #'car) (apply #'min)))
                   (setq overlays (-map #'highlight ranges)))
               ;; else no matches
               (setq overlays (-map #'highlight regions))
               (let ((message-log-max nil))
                 (message (propertize "No matches" 'face 'error))))))
         (after (_beg _end _len)
           (unless (input-pending-p)
             (-each overlays #'delete-overlay)
             (update (let ((s (minibuffer-contents-no-properties)))
                       (unless (string-empty-p s)
                         (hel-pcre-to-elisp s))))))
         (stop ()
           (-each overlays #'delete-overlay))
         (start ()
           (add-hook 'after-change-functions #'after nil t)
           (add-hook 'minibuffer-exit-hook #'stop nil t)))
      ;; main body
      (when-let* ((input (condition-case nil
                             (minibuffer-with-setup-hook #'start
                               (setq overlays (-map #'highlight regions))
                               (hel-read-regexp (if invert "split: " "select: ")))
                           (quit))) ;; "C-g"
                  ((stringp input))
                  ((not (string-empty-p input))))
        (hel-add-to-regex-history input)
        (let ((regexp (hel-pcre-to-elisp input)))
          (->> regions
               (-mapcat (-lambda ((beg . end))
                          (hel-select--search regexp beg end invert)))
               (delq nil)))))))

(defun hel-select--search (regexp start end &optional invert)
  (save-excursion
    (goto-char start)
    (let ((continue t) ranges)
      (while (and continue (re-search-forward regexp end t))
        (-let ((bounds (hel-match)))
          (if (/= (car bounds) (cdr bounds))
              (push bounds ranges)
            ;; Break on zero-length match like "^" or "$", to avoid an
            ;; infinite loop.
            (setq continue nil
                  ranges nil))))
      (cl-callf nreverse ranges)
      (if invert
          (hel-invert-ranges ranges start end)
        ranges))))

(defun hel-select--partition-overlays-by-regions (overlays regions)
  "Separate OVERLAYS into those that overlap any region in REGIONS and those
that do not. Return a list of two lists: (OVERLAP NOT-OVERLAP).

Both REGIONS and OVERLAYS should be sorted by starting position."
  (let ( region rs re
         overlay ovs ove
         overlap not-overlap)
    (while (and regions overlays)
      (unless region (-setq (region &as rs . re) (car regions)))
      (unless overlay (setq overlay (car overlays)
                            ovs (overlay-start overlay)
                            ove (overlay-end overlay)))
      (cond ((not (or (<= re ovs) (<= ove rs)))
             (push (pop overlays) overlap)
             (setq overlay nil))
            ((< ove re)
             (push (pop overlays) not-overlap)
             (setq overlay nil))
            (t
             (pop regions)
             (setq region nil))))
    (list overlap (append not-overlap overlays))))

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
                                  cursors)))
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
           (after (_beg _end _len)
             (unless (input-pending-p)
               (update (let ((s (minibuffer-contents-no-properties)))
                         (unless (string-empty-p s)
                           (hel-pcre-to-elisp s))))))
           (start ()
             (add-hook 'after-change-functions #'after nil t)))
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
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "f" t) 1 nil))

;; F
(hel-define-command hel-find-char-backward ()
  "Prompt user for CHAR and move to the previous occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "F" t) -1 nil))

;; t
(hel-define-command hel-till-char-forward ()
  "Prompt user for CHAR and move before the next occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections hel--extend-selection
  (interactive)
  (hel-maybe-set-mark)
  (hel-find-char (read-char "t" t) 1 t))

;; T
(hel-define-command hel-till-char-backward ()
  "Prompt user for CHAR and move before the prevous occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
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
                    (overlay-put 'face 'lazy-highlight))
                  overlays)))
        overlays))))

;;; .
(provide 'hel-search)
;;; hel-search.el ends here
