;;; init.el --- Emacs init file                      -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;;; Store customizations managed by Custom in a separate file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Add directories with local Lisp code to `load-path'.
(dolist (lisp-dir '("lisp" "vendor-lisp"))
  (let ((default-directory (concat user-emacs-directory lisp-dir "/")))
    (add-to-list 'load-path (directory-file-name default-directory))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'init-package)
(require 'init-ui)
(require 'init-bindings)
(require 'init-defaults)
(require 'init-search)
(require 'init-files)
(require 'init-buffers)
(require 'init-minibuffer)
(require 'init-editing)
(require 'init-completion)
(require 'init-prog)
(require 'init-vc)
(require 'init-calendar)
(require 'init-syntax-check)
(require 'init-treesit)
(require 'init-eshell)
(require 'init-irc)
(require 'init-gnus)

;; Language support
(require 'init-c)
(require 'init-clojure)
(require 'init-go)
(require 'init-http)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-lua)
(require 'init-markdown)
(require 'init-org)
(require 'init-perl)
(require 'init-python)
(require 'init-sh)
(require 'init-sml)
(require 'init-sql)
(require 'init-stats)
(require 'init-web)
(require 'init-yaml)

;; Private init file (not committed to repo).
(require 'init-private)

;;;; Check for recommended features and programs
(require 'doctor-checkup)

(provide 'init)
;;; init.el ends here
