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

;; Set manually installed packages, and ensure they are installed.
(setq package-selected-packages '(bbdb citar color-theme-sanityinc-tomorrow corfu eglot flymake-shellcheck fullframe go-mode jenkinsfile-mode magit marginalia markdown-mode nnhackernews nnreddit ob-go orderless org-roam org-roam-ui paredit pyvenv reformatter vertico yaml-mode sudo-edit))
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

;;; Gnus
(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))
