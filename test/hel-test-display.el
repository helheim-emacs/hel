;;; hel-test-display.el --- Run the Hel tests in a frame with redisplay -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; A batch Emacs never redisplays: `posn-at-point' and
;; `pos-visible-in-window-p' return nil, `window-end' returns the end of
;; the buffer, and the visual line commands do not move.  The code of Hel
;; that depends on the display runs untested there.  This file runs the
;; loaded tests in an interactive Emacs instead, such as `emacs -nw' in
;; a terminal:
;;
;;   python3 test/hel-test-tty.py 30 100 600 \
;;     emacs -nw -Q -l test/hel-test-setup.el -l test/hel-behaviour-tests.el \
;;       -l test/hel-test-display.el
;;
;; `make test-display' runs this command for every suite.  The terminal
;; shows nothing useful, so the results go to the file in the environment
;; variable HEL_TEST_REPORT, one line for each test that fails and a last
;; line in the format of ERT.  Emacs exits with status 0 when every result
;; is as expected, and 1 otherwise.  The environment variable
;; HEL_TEST_SELECTOR limits the tests, as in a batch run.
;;
;;; Code:

(require 'ert)

(defun hel-test-display--report (format-string &rest args)
  "Append a line made from FORMAT-STRING and ARGS to the report file."
  (let ((file (or (getenv "HEL_TEST_REPORT") "hel-test-display.txt")))
    (write-region (concat (apply #'format format-string args) "\n")
                  nil file t 'silent)))

(defun hel-test-display--listener (event-type &rest args)
  "Write the failing tests and the summary to the report file.
EVENT-TYPE and ARGS are those of an ERT listener."
  (pcase event-type
    ('test-ended
     (pcase-let ((`(,_stats ,test ,result) args))
       (unless (ert-test-result-expected-p test result)
         (hel-test-display--report
          "   FAILED  %s: %S" (ert-test-name test)
          (and (ert-test-result-with-condition-p result)
               (ert-test-result-with-condition-condition result))))))
    ('run-ended
     (pcase-let ((`(,stats ,_aborted) args))
       (hel-test-display--report
        "Ran %d tests in a %dx%d window, %d results as expected, %d unexpected, %d skipped"
        (ert-stats-total stats) (window-body-width) (window-body-height)
        (ert-stats-completed-expected stats)
        (ert-stats-completed-unexpected stats)
        (ert-stats-skipped stats))))))

(let* ((selector (let ((s (getenv "HEL_TEST_SELECTOR")))
                   (if (and s (not (string-empty-p s))) s t)))
       (stats (condition-case err
                  (ert-run-tests selector #'hel-test-display--listener)
                (error
                 (hel-test-display--report "Error: %s" (error-message-string err))
                 nil))))
  (kill-emacs (if (and stats (zerop (ert-stats-completed-unexpected stats)))
                  0
                1)))

;;; hel-test-display.el ends here
