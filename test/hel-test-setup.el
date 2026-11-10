;;; hel-test-setup.el --- Batch environment for the Hel tests -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Load this file first in a batch Emacs, before a test file:
;;
;;   timeout 300 emacs -Q --batch -l test/hel-test-setup.el \
;;     -l test/hel-behaviour-tests.el -f ert-run-tests-batch-and-exit
;;
;; `make test' does this for every suite; see `test/README.org'.
;;
;; It finds Hel and its dependencies through `hel-test-paths.el', isolates
;; the session from the real user configuration, and turns on `hel-mode'.
;;
;;; Code:

(require 'cl-lib)

(load (expand-file-name "hel-test-paths"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst hel-test--tmp-dir (make-temp-file "hel-test-" t)
  "Temporary directory for every file that a test session writes.")

(add-hook 'kill-emacs-hook
          (lambda () (delete-directory hel-test--tmp-dir t)))

;;; Isolation from the user configuration

(setq user-emacs-directory (file-name-as-directory hel-test--tmp-dir)
      hel-whitelist-file (expand-file-name "hel-multiple-cursors"
                                           hel-test--tmp-dir)
      custom-file (expand-file-name "custom.el" hel-test--tmp-dir)
      auto-save-list-file-prefix nil
      make-backup-files nil
      create-lockfiles nil
      interprogram-cut-function nil
      interprogram-paste-function nil)

;; Loading must not start a compiler that writes files.
(setq native-comp-jit-compilation nil)

;;; Settings that an interactive Emacs has by default and batch Emacs has not

;; Hel selections are the active region, and the region is active only with
;; `transient-mark-mode'.  Batch Emacs starts with this mode disabled.
(transient-mark-mode 1)

;;; Guard against prompts

(defvar hel-test--unexpected-prompts nil
  "Prompts that a command tried to show during the current test.
The test harness fails a test when this list is not empty.")

(defun hel-test--refuse-prompt (&optional prompt &rest _)
  "Record PROMPT and signal an error, so that a prompt cannot wait for input.
Hel catches errors in `post-command-hook', so the record is what makes the
test fail."
  (push prompt hel-test--unexpected-prompts)
  (error "Unexpected prompt in a test: %s" prompt))

;; A command without a recorded multiple-cursors choice asks with `y-or-n-p'.
(advice-add 'y-or-n-p :override #'hel-test--refuse-prompt)
(advice-add 'yes-or-no-p :override #'hel-test--refuse-prompt)

;;; Hel

(require 'hel)
(hel-mode 1)

;;; hel-test-setup.el ends here
