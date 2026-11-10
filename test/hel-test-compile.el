;;; hel-test-compile.el --- Byte-compile Hel files and fail on any warning -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Byte-compile the Hel files named on the command line in a batch Emacs,
;; and exit with status 1 when a file has an error or a warning:
;;
;;   HEL_TEST_OUTPUT=build emacs -Q --batch -l test/hel-test-compile.el hel-lib.el
;;
;; `make compile' runs this command once for every file.  Each file needs
;; an Emacs of its own: a file that is compiled later in the same Emacs
;; sees the files that the earlier ones loaded, and a missing `require'
;; gives no warning there.
;;
;; The `.elc' files go to the directory in the environment variable
;; HEL_TEST_OUTPUT, or to a temporary directory when it is not set.  No
;; compiled file goes next to the sources, where it would shadow them.
;; The compilation loads the other Hel files from their sources, so a stale
;; compiled file cannot change the result.
;;
;;; Code:

(require 'bytecomp)

(load (expand-file-name "hel-test-paths"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst hel-test--compile-dir
  (let ((output (getenv "HEL_TEST_OUTPUT")))
    (if (and output (not (string-empty-p output)))
        (let ((dir (expand-file-name output hel-test--root)))
          (make-directory dir t)
          dir)
      (let ((dir (make-temp-file "hel-compile-" t)))
        (add-hook 'kill-emacs-hook (lambda () (delete-directory dir t)))
        dir)))
  "Directory for the compiled files.")

;; When a Hel file loads `pcre2el' through another Hel file during the
;; compilation, the compiler warns that `rxt--re-builder-switch-pcre-mode'
;; might not be defined at runtime.  No Hel code calls it, and loading
;; `pcre2el' first prevents this false warning.
(require 'pcre2el)

(setq byte-compile-warnings t
      byte-compile-error-on-warn t
      byte-compile-dest-file-function
      (lambda (file)
        (expand-file-name (concat (file-name-nondirectory file) "c")
                          hel-test--compile-dir)))

(let ((files (mapcar #'expand-file-name command-line-args-left))
      (failed nil))
  (setq command-line-args-left nil)
  (dolist (file files)
    (unless (byte-compile-file file)
      (push (file-name-nondirectory file) failed)))
  (when failed
    (message "Compilation failed: %s" (string-join (nreverse failed) ", "))
    (kill-emacs 1)))

;;; hel-test-compile.el ends here
