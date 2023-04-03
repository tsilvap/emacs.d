;;; init-editing.el --- General editing configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Indent with spaces, not tabs. This can be overridden for certain
;; modes that prefer tabs, e.g. `go-mode'.
(setq-default indent-tabs-mode nil)

;; Enable Auto-Insert mode.
(auto-insert-mode t)

;; Make file executable if it starts with a shebang line.
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

;; Enable Electric Pair mode globally, for automatic bracket pairing.
(electric-pair-mode t)

;; Edit files with sudo.
(require 'sudo-edit)
(global-set-key (kbd "C-c C-r") 'sudo-edit)

;; Enable ws-butler globally, for cleaning up trailing whitespace
;; (only in lines we've edited).
(require 'ws-butler)
(ws-butler-global-mode)
(setq ws-butler-keep-whitespace-before-point nil)

(provide 'init-editing)
;;; init-editing.el ends here
