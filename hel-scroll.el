;;; hel-scroll.el --- Smooth scrolling -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.11.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1") (ultra-scroll "0.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(eval-when-compile (require 'hel-macros))
(require 'hel-vars)
(require 'hel-lib)
(require 'hel-multiple-cursors-core)
(require 'ultra-scroll)

;; XXX: Necessary for smooth scrolling to work.
(setq scroll-conservatively 101
      scroll-margin 0)

;;; Animation

(defvar hel--in-animate-scroll nil
  "Non-nil is inside `hel--animate-scroll' loop.")

(defun hel-smooth-scroll (delta duration)
  "Smoothly scroll the view by DELTA pixels over DURATION seconds.
When there are multiple cursors in the buffer the scroll is capped
to one screen so the main cursor stay in sync with fake ones."
  (unless hel--in-animate-scroll
    (let* ((y-at-point (or (cdr (posn-x-y (posn-at-point)))
                           0)) ; because sometimes `posn-at-point' returns nil
           (line-height (default-line-height))
           ;; The number of pixels the view can scroll before point is dragged
           ;; along the window edge to stay on screen. Scrolling towards the end
           ;; of the buffer drifts point to the top, towards the beginning — to
           ;; the bottom.
           (window-space (if (< delta 0)
                             (- (window-body-height nil t) y-at-point line-height)
                           (- y-at-point line-height))))
      (when (< window-space (abs delta))
        (if hel-multiple-cursors-mode
            ;; Cap the scroll so point stays put, in sync with the fake cursors.
            (setq delta (* (if (< delta 0) -1 1) (max 0 (1- window-space))))
          (hel-maybe-deactivate-mark)))
      (if hel-multiple-cursors-mode
          ;; Whatever happens, cursor must not move, so
          ;; it stays in sync with the fake cursors.
          (hel-save-region
            (hel--animate-scroll delta duration))
        (hel--animate-scroll delta duration t)))))

(defun hel--animate-scroll (delta duration &optional allow-extend?)
  "Smoothly scroll the selected window by DELTA pixels.

Positive DELTA scrolls the view down towards the end of the buffer,
negative — up towards the beginning.

DURATION is the number of seconds the scrolling animation should take
to finish. If ALLOW-EXTEND? is non-nil, the scroll can be extended by
subsequent scrolling commands — by convention, a scrolling command is
one that has the `hel-scroll' property and returns a (DELTA DURATION)
list.

A non-scroll key (or any key, if ALLOW-EXTEND? is nil) ends the
animation at the current position. Every frame leaves the window in
a fully consistent state, so ending mid-way is safe."
  (let* ((gc-cons-percentage (max gc-cons-percentage 0.67))
         (make-cursor-line-fully-visible nil)
         (hel--in-animate-scroll t)
         (duration (or duration hel-scroll-page-duration))
         (init-point (point))
         (start-col (or (car (posn-x-y (posn-at-point)))
                        0)) ; because sometimes `posn-at-point' returns nil
         (now (float-time))
         (traveled 0)   ; Signed number of pixels scrolled so far.
         (target delta) ; Signed number of pixels to scroll in total.
         ;; A pulse is one animation run spanning many frames: fast at the
         ;; start and slowing to a stop at the end. A scroll key tapped
         ;; mid-flight (see the `input-pending-p' branch) restarts the
         ;; animation from the current position, which produces this "pulse"
         ;; effect.
         (pulse-origin 0)   ; TRAVELED at the start of the current pulse.
         (pulse-dist delta) ; Signed distance of the current pulse.
         (pulse-start now)  ; Timestamp the current animation began.
         (pulse-duration duration)
         (frame-time hel-scroll--frame-time)
         (max-step (1- (window-body-height nil t))) ; in pixels
         (frame-deadline now)
         (restore-cursor-fn nil))
    ;; Nonzero vscroll in another window showing this buffer slows
    ;; redisplay down (ultra-scroll issue#32).
    (dolist (win (cdr (get-buffer-window-list (window-buffer))))
      (set-window-vscroll win 0))
    (unwind-protect
        (condition-case err
            (if (or executing-kbd-macro noninteractive)
                ;; No one watches the animation: scroll straight to the
                ;; target. `hel--scroll-pixels' can only move MAX-STEP
                ;; pixels at a time, so do it in as many calls as needed.
                (let ((remaining target))
                  (while (/= remaining 0)
                    (let ((step (hel-clamp (- max-step) remaining max-step)))
                      (hel--scroll-pixels step)
                      (cl-decf remaining step))))
              (while (/= traveled target)
                (cond
                 ((input-pending-p)
                  (let ((event (read-event)))
                    (-if-let* ((allow-extend?)
                               (cmd (key-binding (vector event) t))
                               ((symbolp cmd))
                               ((delta duration) (funcall cmd)))
                        ;; Extend the TARGET and restart the pulse from the
                        ;; current position.
                        (progn
                          (cl-incf target delta)
                          (setq pulse-origin traveled
                                pulse-dist (- target traveled)
                                pulse-start (float-time)
                                pulse-duration duration))
                      ;; Not a scroll key — stop scrolling and return the event
                      ;; back to the command loop.
                      (push event unread-command-events)
                      (setq target traveled))))
                 ;; Too early for the next frame. Busy wait until the DEADLINE.
                 ;;   HACK: Wait in `read-event', which blocks in the C event
                 ;; loop without spinning the CPU, temporarily suspending
                 ;; active timers so they don't fire while waiting.
                 ((< (float-time) frame-deadline)
                  (let ((timer-list nil)
                        (timer-idle-list nil)
                        (wait (- frame-deadline (float-time))))
                    (when-let* ((event (read-event nil nil wait)))
                      (push event unread-command-events))))
                 ;; Render a frame. Advance the DEADLINE by one FRAME-TIME.
                 ;; If redisplay takes longer than the interval, schedule the
                 ;; next frame from the current time instead of accumulating
                 ;; an ever-growing backlog.
                 (t
                  (cl-callf + frame-deadline frame-time)
                  (or (<= (float-time) frame-deadline)
                      (setq frame-deadline (float-time)))
                  (let* ((tau (min 1.0 (/ (- (float-time) pulse-start)
                                          pulse-duration)))
                         ;; TARGET-POS is where the view should be right now:
                         ;; PULSE-ORIGIN plus the eased part of PULSE-DIST.
                         ;; It's recomputed from scratch every frame (instead
                         ;; of adding previous steps), so rounding never
                         ;; accumulates, and the scroll lands exactly on target
                         ;; once TAU reaches 1.
                         (target-pos (+ pulse-origin
                                        (round (* pulse-dist (hel--scroll-ease tau)))))
                         (step (hel-clamp (- max-step)
                                          (- target-pos traveled)
                                          max-step)))
                    (when (/= step 0)
                      (hel--scroll-pixels step)
                      (cl-callf + traveled step)
                      (when (and hel-scroll-hide-cursor
                                 (not restore-cursor-fn)
                                 (/= (point) init-point))
                        (setq restore-cursor-fn (hel-hide-cursor)))
                      (redisplay t)))))))
          ;; We have reached the buffer limit.
          (beginning-of-buffer
           (set-window-start nil (point-min))
           (set-window-vscroll nil 0 t t)
           (message (error-message-string err)))
          (end-of-buffer
           (set-window-vscroll nil 0 t t)
           (message (error-message-string err))))
      (-some-> restore-cursor-fn (funcall))
      (when (and hel-scroll-preserve-column
                 start-col
                 (/= (point) init-point))
        (vertical-motion (cons (/ start-col (frame-char-width)) 0))))))

(defun hel--scroll-ease (x)
  "Map linear progress X in [0.0, 1.0] to eased progress in [0.0, 1.0].
The curve is selected by `hel-scroll-easing'. Every curve but `linear' is
an ease-out: fast at the start, slowing to a gentle stop at X = 1."
  (pcase hel-scroll-easing
    ('linear x)
    ('cubic (- 1.0 (expt (- 1.0 x) 3)))
    ('quartic (- 1.0 (expt (- 1.0 x) 4)))
    ('sine (sin (* x (/ float-pi 2))))
    (_ (- 1.0 (expt (- 1.0 x) 2))))) ; quadratic (default)

(defun hel--scroll-pixels (delta)
  "Scroll the current window by DELTA pixels.
DELTA should be less than the window's height. Positive DELTA scrolls
the view towards the end of the buffer, negative — towards the beginning.

Signals `beginning-of-buffer' or `end-of-buffer' at the buffer boundary."
  ;; We use `ultra-scroll-up' and `ultra-scroll-down'
  ;; instead of `pixel-scroll-precision-scroll-up-page'
  ;; and `pixel-scroll-precision-scroll-down-page' because
  ;; they do the same but better.
  (cond ((< 0 delta) (ultra-scroll-down delta))
        ((< delta 0) (ultra-scroll-up (- delta)))))

;;; Commands

;; C-d
(hel-define-command hel-scroll-down (&optional count)
  "Smoothly scroll the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down `hel-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  :multiple-cursors nil
  (interactive "P")
  (setq count (if (natnump count)
                  (setq hel-scroll-count count)
                hel-scroll-count))
  (let ((delta (if (zerop count)
                   (/ (window-body-height nil t) 2)
                 (* count (default-line-height)))))
    (hel-smooth-scroll delta hel-scroll-half-page-duration)
    (list delta hel-scroll-half-page-duration)))

(put 'hel-scroll-down 'scroll-command t)
(put 'hel-scroll-down 'hel-scroll t)

;; C-u
(hel-define-command hel-scroll-up (&optional count)
  "Smoothly scroll the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls up `hel-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  :multiple-cursors nil
  (interactive "P")
  (setq count (if (natnump count)
                  (setq hel-scroll-count count)
                hel-scroll-count))
  (let ((delta (- (if (zerop count)
                      (/ (window-body-height nil t) 2)
                    (* count (default-line-height))))))
    (hel-smooth-scroll delta hel-scroll-half-page-duration)
    (list delta hel-scroll-half-page-duration)))

(put 'hel-scroll-up 'scroll-command t)
(put 'hel-scroll-up 'hel-scroll t)

;; C-f
(hel-define-command hel-scroll-page-down (&optional count)
  "Smoothly scroll the window COUNT pages downwards.
If multiple cursors are active, rotate the main selection forward COUNT times
instead."
  :multiple-cursors nil
  (interactive "p")
  (or count (setq count 1))
  (let* ((win-height (window-body-height nil t))
         (delta (* count win-height)))
    (hel-smooth-scroll delta hel-scroll-page-duration)
    (list delta hel-scroll-page-duration)))

(put 'hel-scroll-page-down 'scroll-command t)
(put 'hel-scroll-page-down 'hel-scroll t)

;; C-b
(hel-define-command hel-scroll-page-up (&optional count)
  "Smoothly scroll the window COUNT pages upwards.
If multiple cursors are active, rotate the main selection COUNT times
backward instead."
  :multiple-cursors nil
  (interactive "p")
  (or count (setq count 1))
  (let* ((win-height (window-body-height nil t))
         (delta (- (* count win-height))))
    (hel-smooth-scroll delta hel-scroll-page-duration)
    (list delta hel-scroll-page-duration)))

(put 'hel-scroll-page-up 'scroll-command t)
(put 'hel-scroll-page-up 'hel-scroll t)

;; C-e
(hel-define-command hel-scroll-line-down (&optional count)
  "Scroll the window COUNT lines downwards."
  :multiple-cursors nil
  (interactive "p")
  (or count (setq count 1))
  (let ((beginning-of-window? (or (null (posn-at-point))
                                  (= 0 (cdr (posn-col-row (posn-at-point))))))
        (delta (* count (default-line-height))))
    (unless (and beginning-of-window?
                 hel-multiple-cursors-mode)
      (if (= count 1)
          (progn
            (if beginning-of-window? (hel-maybe-deactivate-mark))
            (let ((scroll-preserve-screen-position nil))
              (scroll-up 1)))
        ;; else
        (hel-smooth-scroll delta hel-scroll-line-duration)))
    (list delta hel-scroll-line-duration)))

(put 'hel-scroll-line-down 'scroll-command t)
(put 'hel-scroll-line-down 'hel-scroll t)

;; C-y
(hel-define-command hel-scroll-line-up (&optional count)
  "Scroll the window COUNT lines upwards."
  :multiple-cursors nil
  (interactive "p")
  (or count (setq count 1))
  (let ((end-of-window? (= (1- (window-body-height))
                           (or (cdr (posn-col-row (posn-at-point)))
                               0)))
        (delta (- (* count (default-line-height)))))
    (unless (and end-of-window?
                 hel-multiple-cursors-mode)
      (if (= count 1)
          (progn
            (if end-of-window? (hel-maybe-deactivate-mark))
            (let ((scroll-preserve-screen-position nil))
              (scroll-down 1)))
        ;; else
        (hel-smooth-scroll delta hel-scroll-line-duration)))
    (list delta hel-scroll-line-duration)))

(put 'hel-scroll-line-up 'scroll-command t)
(put 'hel-scroll-line-up 'hel-scroll t)

;; zz
(hel-define-command hel-scroll-line-to-eye-level ()
  "Smoothly scroll current line not to the very top of the window."
  :multiple-cursors nil
  (interactive)
  (let* ((posn-y-target (ceiling (/ (window-body-height nil t) 5)))
         (y-at-point (or (cdr (posn-x-y (posn-at-point)))
                         0)) ;; because sometimes `posn-at-point' returns nil
         (delta (- y-at-point
                   posn-y-target)))
    (hel--animate-scroll delta hel-scroll-half-page-duration)))

(put 'hel-scroll-line-to-eye-level 'scroll-command t)

;; zz (Vim version)
(hel-define-command hel-scroll-line-to-center ()
  "Smoothly scroll current line to the center of the window."
  (interactive)
  :multiple-cursors nil
  (let* ((posn-y-target (ceiling (/ (window-body-height nil t) 2)))
         (y-at-point (or (cdr (posn-x-y (posn-at-point)))
                         0)) ;; because sometimes `posn-at-point' returns nil
         (delta (- y-at-point
                   posn-y-target)))
    (hel--animate-scroll delta hel-scroll-half-page-duration)))

(put 'hel-scroll-line-to-center 'scroll-command t)

;; zt
(hel-define-command hel-scroll-line-to-top ()
  "Smoothly scroll current line to the top of the window."
  :multiple-cursors nil
  (interactive)
  (hel-save-region
    (hel--animate-scroll (-> (cdr (posn-x-y (posn-at-point)))
                             (or 0) ; because sometimes `posn-at-point' returns nil
                             (1-)) ; minus 1 pixel so that the cursor doesn't move
                         hel-scroll-half-page-duration)
    (recenter 0)))

(put 'hel-scroll-line-to-top 'scroll-command t)

;; zb
(hel-define-command hel-scroll-line-to-bottom ()
  "Smoothly scroll current line to the bottom of the window."
  :multiple-cursors nil
  (interactive)
  (let* ((win-height (window-body-height nil t))
         (line-height (default-line-height))
         (y-at-point (or (cdr (posn-x-y (posn-at-point)))
                         0))
         (delta (- win-height y-at-point line-height 1)))
    (hel-save-region
      (hel--animate-scroll (- delta) hel-scroll-half-page-duration))))

(put 'hel-scroll-line-to-bottom 'scroll-command t)

;;; .
(provide 'hel-scroll)
;;; hel-scroll.el ends here
