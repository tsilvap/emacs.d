;; -*- lexical-binding: t; -*-

(require 'package)
(dolist (archive '(("gnu" . "https://elpa.gnu.org/packages/")
		   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives archive t))

;; Prefer built-in repos. Don't use, e.g., MELPA, if we don't have to.
;; Here, the highest number gets priority (what is not mentioned has
;; priority 0).
(setq package-archive-priorities
      '(("elpa" . 2)
	("nongnu" . 1)))

(package-initialize)

;; Get package list from the file packages.txt, and ensure they are
;; installed.
(setq package-selected-packages
      (mapcar #'intern
              (split-string
               (with-temp-buffer
                 (insert-file-contents (concat user-emacs-directory
                                               "packages.txt"))
                 (buffer-string)))))
(package-install-selected-packages)

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

;; Private init file (not committed to repo).
(require 'init-private)
