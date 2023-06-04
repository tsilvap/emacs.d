;;; init-editing.el --- General editing configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setc
 ;; Indent with spaces, not tabs. This can be overridden for certain
 ;; modes that prefer tabs, e.g. `go-mode'.
 indent-tabs-mode nil

 ;; Sentences end with a single space.
 sentence-end-double-space nil)

;; Make file executable if it starts with a shebang line.
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Enable Auto-Insert mode.
(setup autoinsert
  (auto-insert-mode))

;; Enable Electric Pair mode globally, for automatic bracket pairing.
(setup elec-pair
  (electric-pair-mode))

;; Edit files with sudo.
(setup sudo-edit
  (:global "C-c C-r" sudo-edit))

;; Enable ws-butler globally, for cleaning up trailing whitespace
;; (only in lines we've edited).
(setup ws-butler
  (:hide-mode)
  (:option ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

;; Use dumb-jump for languages that don't have a nice language server
;; or good editor support for jump-to-definition (e.g. Perl).
(setup dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Multiple cursors. Macros can do everything that this can do, but
;; often times using multiple cursors is simpler.
(setup multiple-cursors
  (:global "C-S-c C-S-c" mc/edit-lines
           "C->"         mc/mark-next-like-this
           "C-<"         mc/mark-previous-like-this
           "C-c C-<"     mc/mark-all-like-this))

(provide 'init-editing)
;;; init-editing.el ends here
