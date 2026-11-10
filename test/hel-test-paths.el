;;; hel-test-paths.el --- Load path of a batch Emacs that tests Hel -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Put the dependencies of Hel and the checkout under test on `load-path'.
;; `hel-test-setup.el' and `hel-test-compile.el' load this file first.
;;
;; Three environment variables tell where things are.  Each one holds a list
;; of directories separated by colons:
;;
;;   HEL_TEST_DEPS     directories with one subdirectory per package, such
;;                     as the Elpaca build directory or ~/.emacs.d/elpa.
;;                     By default, this is `.deps' in the checkout when
;;                     `make deps' has created it, and otherwise
;;                     ~/.config/emacs/var/elpaca/builds.
;;   HEL_TEST_BUILD    directories with compiled Hel files.  They come before
;;                     the sources on `load-path', so the tests run the
;;                     compiled code.  `make' sets it to `build'.
;;   HEL_TEST_TREESIT  extra directories with tree-sitter grammars.
;;
;; A package directory named `hel' or `hel-*' holds another copy of Hel or
;; an extension, and it must never shadow the checkout under test, so this
;; file skips it.
;;
;;; Code:

(defconst hel-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the checkout that holds this file.")

(defun hel-test--env-dirs (variable default)
  "Return the directories in the environment VARIABLE, or DEFAULT.
An empty or unset VARIABLE gives DEFAULT.  Relative directories are relative
to the root of the checkout."
  (let ((value (getenv variable)))
    (mapcar (lambda (dir) (expand-file-name dir hel-test--root))
            (if (and value (not (string-empty-p value)))
                (split-string value path-separator t)
              default))))

(defun hel-test--default-deps ()
  "Return the default list of directories with the dependencies of Hel."
  (let ((local (expand-file-name ".deps" hel-test--root)))
    (if (file-directory-p local)
        (list local)
      (list "~/.config/emacs/var/elpaca/builds"))))

(dolist (parent (hel-test--env-dirs "HEL_TEST_DEPS" (hel-test--default-deps)))
  (dolist (dir (file-expand-wildcards (expand-file-name "*" parent)))
    (let ((name (file-name-nondirectory dir)))
      (when (and (file-directory-p dir)
                 (not (string= name "hel"))
                 (not (string-prefix-p "hel-" name)))
        (add-to-list 'load-path dir t)))))

;; `add-to-list' puts each directory first, so the build directories end
;; up before the sources.
(add-to-list 'load-path (directory-file-name hel-test--root))
(dolist (dir (reverse (hel-test--env-dirs "HEL_TEST_BUILD" nil)))
  (add-to-list 'load-path (directory-file-name dir)))

(setq load-prefer-newer t)

;; A compiled file in a build directory has no source next to it, and the
;; native compiler warns that it cannot find the `.eln' file for it.  The
;; tests run the byte code, so they do not look for native code at all.
(setq load-no-native t)

(defvar treesit-extra-load-path)
(dolist (dir (hel-test--env-dirs "HEL_TEST_TREESIT" nil))
  (add-to-list 'treesit-extra-load-path dir))

;;; hel-test-paths.el ends here
