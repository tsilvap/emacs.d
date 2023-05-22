;;; init.el --- Emacs init file                      -*- lexical-binding: t; -*-
;;; Commentary:

;; We use setup.el[1] for configuring most packages and options.
;;
;; [1]: https://www.emacswiki.org/emacs/SetupEl

;;; Code:

;; Store customizations managed by Custom in a separate file.
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Add directories with local Lisp code to `load-path'.
(dolist (lisp-dir '("lisp" "vendor-lisp"))
  (let ((default-directory (concat user-emacs-directory lisp-dir "/")))
    (add-to-list 'load-path (directory-file-name default-directory))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'init-package)
(require 'init-setup)
(require 'init-ui)
(require 'init-bindings)
(require 'init-defaults)
(require 'init-search)
(require 'init-files)
(require 'init-buffers)
(require 'init-minibuffer)
(require 'init-editing)
(require 'init-vc)
(require 'init-org)
(require 'init-completion)
(require 'init-syntax-check)
(require 'init-ls)
(require 'init-lisp)
(require 'init-lua)
(require 'init-go)
(require 'init-python)
(require 'init-perl)
(require 'init-sh)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-http)

;;; IRC client
(require 'init-irc)

;;; Gnus
(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))

;; Check for recommended features and programs.
(require 'doctor-checkup)

;; Private init file (not committed to repo).
(require 'init-private)

;; Load Custom file, if it exists.
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
