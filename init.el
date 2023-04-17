;; -*- lexical-binding: t; -*-

(require 'package)
(dolist (archive '(("gnu" . "https://elpa.gnu.org/packages/")
		   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")
                   ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive t))

;; Prefer built-in repos. Don't use, e.g., MELPA, if we don't have to.
;; Also, prefer MELPA over MELPA Stable. We only use MELPA Stable if
;; some package is broken or too unstable on MELPA, and we do this by
;; pinning its archive to "melpa-stable" with `use-package'.
(setq package-archive-priorities
      '(("elpa" . 3)
	("nongnu" . 2)
        ("melpa" . 1)))

(package-initialize)

;; Ensure `use-package' is installed and load it.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Get package list from the file packages.txt, and ensure they are
;; installed.
;;
;; FIXME: This is legacy. I'm using `use-package' to install and
;; configure packages now, and slowly migrating the packages in
;; "packages.txt" until it can be removed.
(setq package-selected-packages
      (mapcar #'intern
              (split-string
               (with-temp-buffer
                 (insert-file-contents (concat user-emacs-directory
                                               "packages.txt"))
                 (buffer-string)))))
(package-install-selected-packages)

;; Store customizations managed by Custom in a separate file.
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Add directories with local Lisp code to `load-path'.
(dolist (lisp-dir '("lisp" "vendor-lisp"))
  (let ((default-directory (concat user-emacs-directory lisp-dir "/")))
    (add-to-list 'load-path (directory-file-name default-directory))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'init-ui)
(require 'init-defaults)
(require 'init-minibuffer)
(require 'init-editing)
(require 'init-vc)
(require 'init-org)
(require 'init-completion)
(require 'init-syntax-check)
(require 'init-lisp)
(require 'init-go)
(require 'init-python)
(require 'init-perl)
(require 'init-sh)
(require 'init-markdown)
(require 'init-http)

;;; Gnus
(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))

;;; Custom defuns, macros, etc.
(require 'tsp)

;; Load Custom file, if it exists.
(when (file-exists-p custom-file)
  (load custom-file))

;; Private init file (not committed to repo).
(require 'init-private)
