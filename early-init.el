;;; early-init.el --- The Emacs early init file      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prevent loading packages before reading the init file, so we can
;; have a choice of which package manager to use.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
