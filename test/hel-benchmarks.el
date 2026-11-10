;;; hel-benchmarks.el --- Time and profile Hel commands with many cursors -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Every command runs once for the real cursor and once more for each fake
;; cursor, one after another, in the single thread of Emacs.  These
;; benchmarks measure how the time of a key sequence grows with the number
;; of cursors.
;;
;; A scenario is a key sequence and a buffer.  The buffer has one line for
;; each cursor, and each line starts with a cursor, so 1000 cursors work on
;; 1000 lines.  Run the scenarios from the root of the checkout:
;;
;;   make bench                       # time every scenario
;;   make bench CURSORS="100 2000"    # with these numbers of cursors
;;   make bench SCENARIO=insert       # only the scenarios that match
;;   make profile SCENARIO=insert     # profile with elp
;;   make profile PROFILER=cpu        # profile with the sampling profiler
;;
;; `make bench' prints, for each scenario and each number of cursors, the
;; median time of the key sequence, that time divided by the number of
;; cursors, and the number of garbage collections.  When the time per
;; cursor grows with the number of cursors, the cost is worse than linear.
;;
;; `make profile' runs each scenario with the largest number of cursors.
;; The elp profiler counts the calls of every `hel-' function and their
;; time, and it sees only the functions that it instruments.  The cpu
;; profiler samples the whole call stack, C functions and garbage collection
;; included; it prints the functions that use the most time themselves,
;; then the call tree.
;;
;;; Code:

(require 'cl-lib)
(require 'elp)
(require 'profiler)
(require 'hel)

;;; Scenarios

(defvar hel-bench-scenarios nil
  "The benchmark scenarios, in the order of their definition.
Each scenario is a plist with the keys :name, :doc, :keys, :mode, :line and
:cursors.  See `hel-bench-define'.")

(cl-defmacro hel-bench-define (name doc &key keys (mode 'text-mode)
                                    (line "line %d: some words in the line")
                                    (cursors t))
  "Define a benchmark scenario NAME, described by DOC.
KEYS is the key sequence, in the format of `kbd'.  MODE is the major mode of
the buffer.  LINE is a `format' string for each line of the buffer, which
gets the line number.  CURSORS nil puts only the real cursor at the start
of the buffer; by default, each line starts with a cursor."
  (declare (indent 1))
  `(progn
     (setq hel-bench-scenarios
           (append (cl-remove ',name hel-bench-scenarios
                              :key (lambda (s) (plist-get s :name)))
                   (list (list :name ',name :doc ,doc :keys ,keys :mode ',mode
                               :line ,line :cursors ,cursors))))
     ',name))

(hel-bench-define motion-word
  "Move every cursor to the next word start."
  :keys "w")

(hel-bench-define motion-line
  "Move every cursor one line down; the cursors merge at the last line."
  :keys "j")

(hel-bench-define select-line
  "Select the line of every cursor."
  :keys "x")

(hel-bench-define text-object-word
  "Select the word under every cursor."
  :keys "w m i w")

(hel-bench-define cut-word
  "Select a word with every cursor and cut it."
  :keys "w d")

(hel-bench-define insert-text
  "Type three characters in Insert state with every cursor."
  :keys "i f o o <escape>")

(hel-bench-define copy-paste
  "Copy a word with every cursor and paste it after the selection."
  :keys "w y p")

(hel-bench-define undo
  "Cut a word with every cursor, then undo the cut."
  :keys "w d u")

(hel-bench-define surround
  "Surround a word with parentheses with every cursor."
  :keys "w m s (")

(hel-bench-define expand-selection
  "Expand the selection of every cursor from a symbol to its enclosing form."
  :keys "l l M-o M-o"
  :mode emacs-lisp-mode
  :line "(foo %d (bar baz) \"qux\")")

(hel-bench-define select-matches
  "Start with one cursor and put a cursor on every match of a regexp.
Each line has two matches, so the scenario ends with twice as many cursors
as the buffer has lines."
  :keys "% s l i n e <return>"
  :cursors nil)

;;; Running a scenario

(defun hel-bench--env (variable default)
  "Return the value of the environment VARIABLE, or DEFAULT when it is empty."
  (let ((value (getenv variable)))
    (if (and value (not (string-empty-p value))) value default)))

(defun hel-bench--selected-scenarios ()
  "Return the scenarios whose names match the regexp in HEL_BENCH_SCENARIO."
  (let* ((regexp (hel-bench--env "HEL_BENCH_SCENARIO" ""))
         (scenarios (cl-remove-if-not
                     (lambda (s)
                       (string-match-p regexp (symbol-name (plist-get s :name))))
                     hel-bench-scenarios)))
    (or scenarios
        (error "No scenario matches the regexp %S; the scenarios are: %s"
               regexp (mapconcat (lambda (s) (symbol-name (plist-get s :name)))
                                 hel-bench-scenarios " ")))))

(defun hel-bench--cursor-counts ()
  "Return the numbers of cursors in HEL_BENCH_CURSORS, in ascending order."
  (sort (mapcar #'string-to-number
                (split-string (hel-bench--env "HEL_BENCH_CURSORS" "1 100 1000")))
        #'<))

(defun hel-bench--prepare (scenario lines)
  "Make a fresh buffer for SCENARIO with LINES lines and its cursors.
Return the buffer, which is current and shown in the selected window."
  (let ((buffer (generate-new-buffer "*hel-bench*"))
        (line (plist-get scenario :line)))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (funcall (plist-get scenario :mode))
    (dotimes (i lines)
      (insert (format line (1+ i)) "\n"))
    (goto-char (point-min))
    (deactivate-mark)
    (when (plist-get scenario :cursors)
      (save-excursion
        (forward-line 1)
        (while (not (eobp))
          (hel-create-fake-cursor (point))
          (forward-line 1)))
      (hel-auto-multiple-cursors-mode))
    (setq buffer-undo-list nil
          last-command nil
          this-command nil)
    buffer))

(defun hel-bench--release (buffer)
  "Kill BUFFER and its cursors."
  (with-current-buffer buffer
    (set-buffer-modified-p nil))
  (kill-buffer buffer))

(defun hel-bench--press (scenario)
  "Press the keys of SCENARIO.  Signal an error when the keys fail.
The messages of the commands are not shown: with many cursors, they would
fill the output of the benchmark."
  (let ((inhibit-message t)
        (message-log-max nil))
    (execute-kbd-macro (kbd (plist-get scenario :keys)))))

(defun hel-bench--time-once (scenario lines)
  "Run SCENARIO once on LINES lines and return (SECONDS . GCS)."
  (let ((buffer (hel-bench--prepare scenario lines))
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (unwind-protect
        (let ((gcs gcs-done)
              (start (float-time)))
          (hel-bench--press scenario)
          (cons (- (float-time) start) (- gcs-done gcs)))
      (hel-bench--release buffer))))

(defun hel-bench--median (numbers)
  "Return the median of NUMBERS."
  (let ((sorted (sort (copy-sequence numbers) #'<)))
    (nth (/ (length sorted) 2) sorted)))

;;; Entry points

(defun hel-bench-run ()
  "Time every selected scenario and print a table, then exit Emacs."
  (let ((scenarios (hel-bench--selected-scenarios))
        (repeat (string-to-number (hel-bench--env "HEL_BENCH_REPEAT" "5")))
        (counts (hel-bench--cursor-counts))
        (failed nil))
    (princ (format "%-18s %8s %12s %14s %5s\n"
                   "scenario" "cursors" "median ms" "us per cursor" "gcs"))
    (dolist (scenario scenarios)
      (condition-case err
          (dolist (lines counts)
            ;; The first run loads code and fills caches; it does not count.
            (hel-bench--time-once scenario lines)
            (let* ((runs (cl-loop repeat repeat
                                  collect (hel-bench--time-once scenario lines)))
                   (seconds (hel-bench--median (mapcar #'car runs)))
                   (gcs (hel-bench--median (mapcar #'cdr runs))))
              (princ (format "%-18s %8d %12.2f %14.1f %5d\n"
                             (plist-get scenario :name) lines
                             (* 1000 seconds) (/ (* 1e6 seconds) lines) gcs))))
        (error
         (push (plist-get scenario :name) failed)
         (princ (format "%-18s failed: %s\n" (plist-get scenario :name)
                        (error-message-string err))))))
    (kill-emacs (if failed 1 0))))

(defun hel-bench--profile-scenarios (scenarios lines)
  "Run each of SCENARIOS once on LINES lines."
  (dolist (scenario scenarios)
    (let ((buffer (hel-bench--prepare scenario lines))
          (kill-ring nil)
          (kill-ring-yank-pointer nil))
      (unwind-protect (hel-bench--press scenario)
        (hel-bench--release buffer)))))

(defun hel-bench--print-cpu-profile (profile)
  "Print PROFILE: the functions with the most own time, then the call tree."
  (let ((buffer (profiler-report-setup-buffer profile)))
    (with-current-buffer buffer
      (profiler-report-render-reversed-calltree)
      (princ "\nFunctions that use the most time themselves:\n")
      (princ (buffer-substring-no-properties
              (point-min)
              (save-excursion (goto-char (point-min)) (forward-line 40) (point))))
      (profiler-report-render-calltree)
      (goto-char (point-min))
      (while (not (eobp))
        (profiler-report-expand-entry t)
        (forward-line 1))
      (princ "\nCall tree:\n")
      (princ (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer buffer)))

(defun hel-bench-profile ()
  "Profile the selected scenarios with the largest number of cursors, then exit.
The profiler comes from HEL_BENCH_PROFILER: elp or cpu."
  (let ((scenarios (hel-bench--selected-scenarios))
        (lines (car (last (hel-bench--cursor-counts))))
        (profiler (intern (hel-bench--env "HEL_BENCH_PROFILER" "elp"))))
    (princ (format "Profiling %s with %d cursors, profiler %s\n"
                   (mapconcat (lambda (s) (symbol-name (plist-get s :name)))
                              scenarios " ")
                   lines profiler))
    ;; The first run loads code and fills caches; it is not profiled.
    (hel-bench--profile-scenarios scenarios lines)
    (pcase profiler
      ('elp
       (elp-instrument-package "hel-")
       (elp-reset-all)
       (hel-bench--profile-scenarios scenarios lines)
       (let ((elp-use-standard-output t)
             (elp-sort-by-function #'elp-sort-by-total-time))
         (elp-results))
       (elp-restore-all))
      ('cpu
       (profiler-start 'cpu)
       (unwind-protect (hel-bench--profile-scenarios scenarios lines)
         (profiler-stop))
       (hel-bench--print-cpu-profile (profiler-cpu-profile)))
      (_ (error "Unknown profiler %s, use elp or cpu" profiler)))
    (kill-emacs 0)))

;;; hel-benchmarks.el ends here
