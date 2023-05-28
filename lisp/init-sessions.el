;;; init-sessions.el --- Save and restore Emacs sessions between restarts  -*- lexical-binding: t; -*-
;;; Commentary:

;; Inspired by and borrowed from:
;; https://github.com/purcell/emacs.d/blob/678073d/lisp/init-sessions.el

;;; Code:

;;;; Desktop sessions

(setup desktop
  (:option desktop-path (list user-emacs-directory)
           desktop-auto-save-timeout 600)
  (desktop-save-mode))

(defun +sessions--desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (* (float-time (time-since start-time)) 1000)))))

(defun +sessions--desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (* (float-time (time-since start-time)) 1000)
               (when filename
                 (abbreviate-file-name filename))))))

(advice-add 'desktop-read :around '+sessions--desktop-time-restore)

(advice-add 'desktop-create-buffer :around '+sessions--desktop-time-buffer-create)

(provide 'init-sessions)
;;; init-sessions.el ends here
