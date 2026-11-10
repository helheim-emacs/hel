;;; hel-test-helpers.el --- The harness of the Hel tests -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Every test file builds on this file.  A test makes a buffer from a marked
;; text, does something in it, and compares the buffer with another marked
;; text:
;;
;;   (hel-test-with #'emacs-lisp-mode "(foo |bar baz)"
;;     (hel-test-run #'hel-forward-word-start)
;;     (should (equal (hel-test-render) "(foo ^bar |baz)")))
;;
;; Notation of a marked text:
;;
;;   |   main cursor (point)
;;   ^   the other end of the main selection (mark); it is shown only while
;;       the selection is active and not empty
;;   ¦   point of a secondary cursor
;;   ¬   the other end of the selection of a secondary cursor
;;
;; A literal `|' or `^' in the text is written twice.  An input text marks
;; only the main cursor; `hel-test-render' shows all of them.
;;
;; All access to the internals of Hel is in the section "Reading the state
;; of Hel".  Adapt the functions of that section when the internals change.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'treesit)
(require 'hel)

(defvar hel-test--unexpected-prompts)

;;; Reading the state of Hel

(defun hel-test-state ()
  "Return the current Hel state: `normal', `insert' or `emacs'."
  hel-state)

(defun hel-test-fake-cursors ()
  "Return the secondary cursors as a list of (POINT . MARK), sorted by POINT.
MARK is nil when the cursor has no selection, or its selection is empty."
  (sort (mapcar (lambda (cursor)
                  (let ((point (marker-position (overlay-get cursor 'point)))
                        (mark (and (overlay-get cursor 'mark-active)
                                   (marker-position (overlay-get cursor 'mark)))))
                    (cons point (unless (eql point mark) mark))))
                (hel-all-fake-cursors))
        (lambda (a b) (< (car a) (car b)))))

(defun hel-test-selection ()
  "Return the text of the main selection, or nil when there is none."
  (and (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))))

(defun hel-test-selections ()
  "Return the selected text of every cursor, in buffer order.
A cursor without a selection gives the empty string."
  (let ((all (cons (if (use-region-p)
                       (cons (region-beginning) (hel-test-selection))
                     (cons (point) ""))
                   (mapcar (lambda (cursor)
                             (let ((point (marker-position (overlay-get cursor 'point)))
                                   (mark (marker-position (overlay-get cursor 'mark))))
                               (cons (min point mark)
                                     (buffer-substring-no-properties
                                      (min point mark) (max point mark)))))
                           (hel-all-fake-cursors)))))
    (mapcar #'cdr (sort all (lambda (a b) (< (car a) (car b)))))))

;;; Marked text

(defun hel-test--parse (input)
  "Return (TEXT POINT MARK) for INPUT, a marked text."
  (let ((text "") (i 0) (n (length input)) point mark)
    (while (< i n)
      (let ((char (aref input i))
            (next (and (< (1+ i) n) (aref input (1+ i)))))
        (cond ((and (memq char '(?| ?^)) (eql char next))
               (setq text (concat text (string char))
                     i (+ i 2)))
              ((eql char ?|)
               (when point (error "Two main cursors in %S" input))
               (setq point (1+ (length text))
                     i (1+ i)))
              ((eql char ?^)
               (setq mark (1+ (length text))
                     i (1+ i)))
              (t
               (setq text (concat text (string char))
                     i (1+ i))))))
    (list text (or point 1) mark)))

(defun hel-test--quote (text)
  "Return TEXT with each `|' and `^' doubled, as a marked text writes them."
  (replace-regexp-in-string "[|^]" (lambda (s) (concat s s)) text t t))

(defun hel-test-text (input)
  "Return the plain text of INPUT, a marked text, without its marks."
  (car (hel-test--parse input)))

(defun hel-test-mark (text needle)
  "Return TEXT as a marked text with the cursor before the first NEEDLE.
TEXT and NEEDLE are plain texts.  This is for a text that several tests
share, each with the cursor at another place."
  (let ((pos (or (string-search needle text)
                 (error "No %S in %S" needle text))))
    (concat (hel-test--quote (substring text 0 pos))
            "|"
            (hel-test--quote (substring text pos)))))

(defun hel-test-render ()
  "Return the accessible text of the buffer as a marked text."
  (let (marks)
    (push (cons (point) ?|) marks)
    (when (use-region-p)
      (push (cons (mark) ?^) marks))
    (pcase-dolist (`(,point . ,mark) (hel-test-fake-cursors))
      (push (cons point ?¦) marks)
      (when mark (push (cons mark ?¬) marks)))
    (let* ((order '(?¦ ?| ?¬ ?^))
           (rank (lambda (char) (cl-position char order)))
           (marks (sort marks (lambda (a b)
                                (if (= (car a) (car b))
                                    (< (funcall rank (cdr a))
                                       (funcall rank (cdr b)))
                                  (< (car a) (car b))))))
           (from (point-min))
           (result ""))
      (pcase-dolist (`(,pos . ,char) marks)
        (setq result (concat result
                             (hel-test--quote (buffer-substring-no-properties from pos))
                             (string char))
              from pos))
      (concat result (hel-test--quote (buffer-substring-no-properties from (point-max)))))))

;;; Buffers

(defvar hel-test-queries-directory
  (expand-file-name "../queries/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The tree-sitter queries of the checkout under test.")

(defvar hel-test--query-cache (make-hash-table :test #'eq)
  "The compiled queries, shared by all tests.
Compiling a query takes much longer than a test.")

(defun hel-test-call-with-buffer (mode input function)
  "Call FUNCTION in a fresh buffer in major MODE with the marked text INPUT.
The buffer is shown in the only window, and the windows come back
afterwards.  A tree-sitter MODE without its grammar skips the test."
  (let ((buffer (generate-new-buffer "*hel-test*"))
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (killed-rectangle nil)
        (register-alist nil)
        (global-mark-ring nil)
        (hel-treesit-queries-directory hel-test-queries-directory)
        (hel-treesit--queries hel-test--query-cache)
        (hel-test--unexpected-prompts nil)
        (inhibit-message t))
    (save-window-excursion
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer buffer)
            (funcall mode)
            (when (and (string-suffix-p "-ts-mode" (symbol-name mode))
                       (null (treesit-parser-list)))
              (ert-skip (format "%s has no grammar" mode)))
            (setq-local indent-tabs-mode nil)
            (pcase-let ((`(,text ,point ,mark) (hel-test--parse input)))
              (let ((inhibit-read-only t))
                (insert text))
              (goto-char point)
              (setq mark-ring nil)
              (if mark
                  (push-mark mark t t)
                (deactivate-mark)))
            (setq buffer-undo-list nil
                  deactivate-mark nil
                  last-command nil
                  this-command nil
                  prefix-arg nil
                  hel-test--last-command nil)
            (set-buffer-modified-p nil)
            (funcall function))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(defmacro hel-test-with (mode input &rest body)
  "Evaluate BODY in a fresh buffer in major MODE with the marked text INPUT."
  (declare (indent 2) (debug t))
  `(hel-test-call-with-buffer ,mode ,input (lambda () ,@body)))

(defun hel-test-goto (needle)
  "Drop the selection and move point to the start of the first NEEDLE."
  (deactivate-mark)
  (goto-char (point-min))
  (search-forward needle)
  (goto-char (match-beginning 0)))

(defun hel-test-select (needle)
  "Select the first NEEDLE, with point at its end."
  (goto-char (point-min))
  (search-forward needle)
  (set-mark (match-beginning 0)))

;;; Running commands

(defun hel-test-run (command &optional count)
  "Run COMMAND with COUNT for every cursor, as the command loop would."
  (let ((current-prefix-arg count)
        (this-command command))
    (hel-execute-command-for-all-cursors command))
  (setq last-command command))

(defun hel-test-steps (command n &optional count)
  "Run COMMAND N times with COUNT and return the selection after each."
  (cl-loop repeat n
           collect (progn (hel-test-run command count)
                          (hel-test-selection))))

;;; Pressing keys

(defvar hel-test--end-reached nil
  "Non-nil when the last key sequence ran to its end.")

(defvar hel-test--end-in-minibuffer nil
  "Non-nil when the last key sequence ended inside an open minibuffer.")

(defvar hel-test--last-command nil
  "The value of `last-command' at the end of the last key sequence.
The command loop forgets it when a keyboard macro ends.  With this variable
one test can press keys in several steps, and each step continues the previous
one the same way as keys that the user presses one by one.")

(defun hel-test--end-of-keys ()
  "Mark that all keys of a sequence were consumed by commands.
This command is transparent for the next command, which sees the same
`last-command' as if this one never ran."
  (interactive)
  (setq this-command last-command
        real-this-command real-last-command
        hel-test--last-command last-command)
  (if (minibufferp)
      (progn (setq hel-test--end-in-minibuffer t)
             (abort-recursive-edit))
    (setq hel-test--end-reached t)))

(put 'hel-test--end-of-keys 'multiple-cursors nil)
(global-set-key [f35] #'hel-test--end-of-keys)

(defun hel-test--keys (keys)
  "Return the events of KEYS as a vector.
KEYS is a string of plain characters mixed with key names, and spaces between
them have no meaning: \"miw\", \"vll<escape>w\", \"M-;vl\", \"]SPC\", \"C-<i>\"."
  (let ((pos 0)
        (token (rx (or (seq (* (any "CMS") "-") "<" (+ (not (any "> "))) ">")
                       "RET" "SPC" "DEL" "TAB" "ESC"
                       (seq (+ (any "CMS") "-") anychar)
                       anychar)))
        (events []))
    (while (< pos (length keys))
      (if (eql (aref keys pos) ?\s)
          (cl-incf pos)
        (string-match token keys pos)
        (setq events (vconcat events (kbd (match-string 0 keys)))
              pos (match-end 0))))
    events))

(defun hel-test-press (keys)
  "Press KEYS, a string in the format of `hel-test--keys'.
Every key sequence ends with the extra key F35, so a command that still
waits for input gets this key instead of reading the terminal: `read-char'
signals an error, and the minibuffer is aborted and reported.

Return nil on success, and the message of an error that stopped the keys."
  (setq hel-test--end-reached nil
        hel-test--end-in-minibuffer nil
        this-command hel-test--last-command)
  (let ((failure (condition-case err
                     (progn (execute-kbd-macro (vconcat (hel-test--keys keys) [f35]))
                            nil)
                   (error (error-message-string err))
                   (quit "Quit"))))
    (when hel-test--unexpected-prompts
      (ert-fail (list "Keys caused a prompt" keys hel-test--unexpected-prompts)))
    (when hel-test--end-in-minibuffer
      (ert-fail (list "Keys left the minibuffer open" keys)))
    (when (and (not failure) (not hel-test--end-reached))
      (setq failure "Keys stopped before their end"))
    failure))

(defun hel-test-press! (keys)
  "Press KEYS and fail the test when they do not run to their end."
  (when-let* ((failure (hel-test-press keys)))
    (ert-fail (list "Keys failed" keys failure))))

(defun hel-test-expect (keys expected &optional state)
  "Press KEYS and check the rendered buffer against EXPECTED, and the STATE."
  (hel-test-press! keys)
  (should (equal (hel-test-render) expected))
  (should (eq (hel-test-state) (or state 'normal))))

(defun hel-test-unchanged-p (command input mode &optional count)
  "Return non-nil when COMMAND with COUNT leaves INPUT in MODE as it is.
INPUT is a marked text.  The mark ring must stay as it is too."
  (hel-test-with mode input
    (hel-test-run command count)
    (and (equal (hel-test-render) input)
         (null mark-ring))))

;;; Skipping

(defun hel-test-grouped-p ()
  "Return non-nil when `treesit-query-capture' takes the GROUPED argument."
  (and (fboundp 'treesit-query-capture)
       (let ((arity (func-arity 'treesit-query-capture)))
         (and (numberp (cdr arity)) (<= 6 (cdr arity))))))

(defmacro hel-test-need (&rest languages)
  "Skip the test unless every grammar in LANGUAGES is installed.
A macro, because `skip-unless' exists only inside `ert-deftest'."
  `(progn
     (skip-unless (fboundp 'treesit-language-available-p))
     ,@(mapcar (lambda (language)
                 `(skip-unless (treesit-language-available-p ,language)))
               languages)))

(defmacro hel-test-need-queries (&rest languages)
  "Skip unless LANGUAGES have grammars and Emacs runs the text object queries."
  `(progn
     (hel-test-need ,@languages)
     (skip-unless (hel-test-grouped-p))))

(provide 'hel-test-helpers)

;;; hel-test-helpers.el ends here
