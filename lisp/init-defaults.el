;;; init-defaults.el --- General configuration and sane defaults  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq user-mail-address "thiagodasilva@protonmail.com"
      user-full-name "Thiago da Silva Pinto")

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Disable the bell.
(setq ring-bell-function 'ignore)

;; Make UTF-8 the default coding system.
(set-language-environment "UTF-8")

;; Make backup files (Emacs does this by default), with some better
;; behavior.
(setq
 make-backup-files t

 ;; Create backups by copying, to avoid changing the owner/group of
 ;; the file, and to avoid making soft/hard links point to the backup
 ;; file.
 backup-by-copying t

 ;; Keep all backup files in a single directory, rather than littering
 ;; the filesystem. Put Tramp backup files in that directory too.
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 tramp-backup-directory-alist backup-directory-alist

 ;; Make numbered backup files, and delete excess backup files
 ;; silenty.
 version-control t
 kept-new-versions 5
 kept-old-versions 5
 delete-old-versions t)

;; Add the path assigned to the remote user by the remote host to
;; TRAMP's remote path.
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Enable Repeat mode to enable repeatable commands.
(repeat-mode t)

;; Enable Winner Mode globally, to undo and redo changes to window
;; layout.
(winner-mode t)

;; Create repeating map for Winner Mode.
;;
;; With this, instead of:
;;     C-c <left> C-c <left> C-c <right> C-c <left> (...)
;; We can do:
;;     C-c <left> <left> <right> <left> (...)
(defvar winner-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'winner-undo)
    (define-key map (kbd "<right>") 'winner-redo)
    map)
  "Repeating map for Winner Mode.")

(dolist (command '(winner-undo winner-redo))
  (put command 'repeat-map 'winner-repeat-map))

;; Enable Auto Revert globally.
(global-auto-revert-mode t)

(provide 'init-defaults)
;;; init-defaults.el ends here
