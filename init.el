;;; init.el --- Emacs init file                      -*- lexical-binding: t; -*-
;;; Commentary:

;; The Emacs init file. We use Elpaca for package management and
;; setup.el for configuring most packages and options.

;;; Code:

;; Add directories with local Lisp code to `load-path'.
(dolist (lisp-dir '("lisp" "vendor-lisp"))
  (let ((default-directory (concat user-emacs-directory lisp-dir "/")))
    (add-to-list 'load-path (directory-file-name default-directory))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'init-elpaca)

;; Store customizations managed by Custom in a separate file.
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-setup)
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

;;; IRC client
(require 'init-irc)

;;; Gnus
(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))

;;; Custom defuns, macros, etc.
(require 'tsp)

;; Load Custom file, if it exists.
(when (file-exists-p custom-file)
  (load custom-file))

;; Private init file (not committed to repo).
(require 'init-private)

(provide 'init)
;;; init.el ends here
