;;; init-files.el --- Configuration related to files  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun tsp/create-ad-hoc-directory (prefix)
  "Create ad hoc directory to place files, scripts, etc.
PREFIX is added to the end of the directory name, if provided."
  (interactive "MPrefix: ")
  (let ((ad-hoc-directory-name
         (concat "~/misc/adhoc/"
                 (format-time-string "%Y-%m-%d-%H%M%S" (current-time))
                 (when prefix (concat "-" prefix)))))
    (make-directory ad-hoc-directory-name t)
    (find-file ad-hoc-directory-name)))

;;; Based on `project-find-file's implementation.
(defun tsp/find-file-in-emacs-config ()
  "Search for a file in `user-emacs-directory'."
  (interactive)
  (let* ((pr (project-current t user-emacs-directory))
         (root (project-root pr))
         (dirs (list root)))
    (project-find-file-in
     (or (thing-at-point 'filename)
         (and buffer-file-name (file-relative-name buffer-file-name root)))
     dirs pr)))

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
   delete-old-versions t)

  ;; Disable lockfiles. They're not that useful in a single-user
  ;; system, and they cause some minor issues with programs that don't
  ;; expect to see a lockfile.
  (:option create-lockfiles nil))

(setup treemacs
  (:global "<f9>" treemacs
           "<C-f9>" treemacs-find-file))

(provide 'init-files)
;;; init-files.el ends here
