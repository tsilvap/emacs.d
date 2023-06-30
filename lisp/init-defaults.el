;;; init-defaults.el --- General configuration and sane defaults  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom +user-sync-directory "~/Sync/"
  "Directory whose contents are synchronized between the user's machines.
The files in this directory are not committed to the emacs.d
repository, but are nonetheless expected to be available in all
machines in which the user uses this config.

This can be, for example, a Nextcloud or a Syncthing directory,
or it can be a directory that is version controlled and kept in
sync between machines.

Note that this should end with a directory separator.")

(setup emacs
  (:global
   "M-o" other-window

   ;; Less confusing alternatives for `upcase-region' and
   ;; `downcase-region'.
   [remap upcase-region] upcase-dwim
   [remap downcase-region] downcase-dwim)

  (:option
   ;; Basic user information.
   user-full-name "<your-full-name>"
   user-mail-address "<your-mail-address>"

   ;; Use encrypted authorization source file.
   auth-sources '("~/.authinfo.gpg")

   inhibit-startup-screen t
   ring-bell-function 'ignore
   confirm-kill-emacs 'y-or-n-p
   help-window-select t)

  ;; Make UTF-8 the default coding system.
  (set-language-environment "UTF-8")

  ;; Enable `set-goal-column' command.
  (put 'set-goal-column 'disabled nil))

(setup tramp
  (:when-loaded
    ;; Add the path assigned to the remote user by the remote host to
    ;; TRAMP's remote path.
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

;; Enable repeatable commands.
(setup repeat
  (repeat-mode))

;; Undo and redo changes to window layout.
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

;; A modern spell checker for Emacs.
(setup jinx
  (:hide-mode)
  (:hook-into text-mode)
  (:global [remap ispell-word] jinx-correct)
  (:when-loaded
    (:option jinx-languages "en_US pt_BR")))

(provide 'init-defaults)
;;; init-defaults.el ends here
