;;; hel-test-deps.el --- Install the dependencies of Hel for the tests -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Install the packages that Hel requires into the directory `.deps' at the
;; root of the checkout:
;;
;;   emacs -Q --batch -l test/hel-test-deps.el
;;
;; `make deps' runs this command.  `hel-test-paths.el' uses `.deps' when the
;; variable HEL_TEST_DEPS is not set, so a machine without an Emacs
;; configuration, such as a CI runner, can run the tests.
;;
;; GNU ELPA and NonGNU ELPA have priority over MELPA, so the stable releases
;; are installed.  Only `ultra-scroll' comes from MELPA, because no other
;; archive has it.
;;
;;; Code:

(require 'package)

(defconst hel-test--deps-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the checkout that holds this file.")

(defconst hel-test--deps
  '(dash avy pcre2el ultra-scroll)
  "The packages that Hel requires, from `Package-Requires' in `hel.el'.")

(setq package-user-dir (expand-file-name ".deps" hel-test--deps-root)
      package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 10) ("nongnu" . 10) ("melpa" . 0))
      package-native-compile nil)

(package-initialize)
(package-refresh-contents)

(dolist (name hel-test--deps)
  (unless (package-installed-p name)
    (package-install name)))

(message "Dependencies in %s: %s"
         package-user-dir
         (mapconcat (lambda (name)
                      (format "%s %s" name
                              (package-version-join
                               (package-desc-version (cadr (assq name package-alist))))))
                    hel-test--deps ", "))

;;; hel-test-deps.el ends here
