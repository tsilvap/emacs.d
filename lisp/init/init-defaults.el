;;; init-defaults.el --- General configuration and sane defaults  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun ansi-color-apply-on-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defconst tsp/sync-directory "~/Dropbox/"
  "Directory whose contents are synchronized between my machines.
The files in this directory are not committed to the emacs.d
repository, but are nonetheless expected to be available in all
machines in which I use this config.

This can be, for example, a Nextcloud or a Syncthing directory,
or it can be a directory that is version controlled and kept in
sync between machines.

This variable should end with a directory separator.")

(defun +os-wsl2p ()
  "Return non-nil if Emacs is running under WSL2."
  (string-match "microsoft-standard-WSL2" operating-system-release))

;; exec-path-from-shell -- Load environment variables from the user's
;; shell.
(when (display-graphic-p)
  (exec-path-from-shell-initialize))

;; An easier alias for `other-window'.
(keymap-global-set "M-o" #'other-window)

;; Less confusing alternatives for `upcase-region' and
;; `downcase-region'.
(keymap-global-set "<remap> <upcase-region>" #'upcase-dwim)
(keymap-global-set "<remap> <downcase-region>" #'downcase-dwim)

(setopt
 ;; Basic user information.
 user-full-name "<your-full-name>"
 user-mail-address "<your-mail-address>"

 ;; Use encrypted authorization source file.
 auth-sources '("~/.authinfo.gpg")

 ;; For the scratch message, see: http://paulgraham.com/todo.html
 inhibit-startup-screen t
 initial-scratch-message
 ";; Don't ignore your dreams; don't work too much;\n;; say what you think; cultivate friendships;\n;; be happy.\n\n"

 ring-bell-function 'ignore
 confirm-kill-emacs 'y-or-n-p
 help-window-select t)

;; Make UTF-8 the default coding system.
(set-language-environment "UTF-8")

;; Enable some useful commands.
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(when (+os-wsl2p)
  (exec-path-from-shell-initialize))

(use-package tramp
  :config
  (setq tramp-default-remote-shell "/bin/bash")

  ;; Add the path assigned to the remote user by the remote host to
  ;; TRAMP's remote path.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Enable repeatable commands.
(use-package repeat
  :config
  (repeat-mode))

;; Undo and redo changes to window layout.
(use-package winner
  :config
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

;;; envrc
(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(provide 'init-defaults)
;;; init-defaults.el ends here
