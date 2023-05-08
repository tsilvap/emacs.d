;;; init-defaults.el --- General configuration and sane defaults  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup emacs
  (:global "M-o" other-window)          ; alias for "C-x o"

  (:option user-mail-address "thiagodasilva@protonmail.com"
           user-full-name    "Thiago da Silva Pinto")

  (:option
   ;; Disable startup screen.
   inhibit-startup-screen t

   ;; Disable the bell.
   ring-bell-function 'ignore

   ;; Ask for confirmation before closing Emacs.
   confirm-kill-emacs 'y-or-n-p)

  ;; Make backup files (Emacs does this by default), with some better
  ;; behavior.
  (:option
   make-backup-files t

   ;; Create backups by copying, to avoid changing the owner/group of
   ;; the file, and to avoid making soft/hard links point to the
   ;; backup file.
   backup-by-copying t

   ;; Keep all backup files in a single directory, rather than
   ;; littering the filesystem. Put Tramp backup files in that
   ;; directory too.
   backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
   tramp-backup-directory-alist backup-directory-alist

   ;; Make numbered backup files, and delete excess backup files
   ;; silenty.
   version-control t
   kept-new-versions 5
   kept-old-versions 5
   delete-old-versions t)

  ;; Make UTF-8 the default coding system.
  (set-language-environment "UTF-8"))

(setup tramp
  (:when-loaded
    ;; Add the path assigned to the remote user by the remote host to
    ;; TRAMP's remote path.
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

;; Enable Repeat mode to enable repeatable commands.
(setup repeat
  (repeat-mode))

;; Enable Winner Mode to undo and redo changes to window layout.
(setup winner
  (winner-mode)

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
    (put command 'repeat-map 'winner-repeat-map)))

;; Use ripgrep for searching files.
(setup rg
  (:global "C-c s" rg-menu)

  ;; Make Xref use ripgrep instead of grep. This will speed up other
  ;; packages that use Xref under the hood, e.g. Dired, project.el.
  ;;
  ;; Still, as of Mar 13 2023, the `project-find-regexp' implementation
  ;; isn't very optimized to work with ripgrep, so searching with
  ;; `rg-menu' is much faster.
  (:option xref-search-program 'ripgrep))

(provide 'init-defaults)
;;; init-defaults.el ends here
