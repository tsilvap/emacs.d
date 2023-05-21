;;; init-files.el --- Configuration related to files  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun tsp/create-ad-hoc-directory ()
  "Create ad hoc directory to place files, scripts, etc."
  (interactive)
  (let ((ad-hoc-directory-name
         (concat "~/Misc/adhoc/"
                 (format-time-string "%Y-%m-%d-%H%M%S" (current-time)))))
    (make-directory ad-hoc-directory-name t)
    (find-file ad-hoc-directory-name)))

(setup emacs
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
   delete-old-versions t))

(provide 'init-files)
;;; init-files.el ends here
