;;; tsp.el --- Centralized place for my custom defuns, macros, etc.  -*- lexical-binding: t; -*-
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

(provide 'tsp)
;;; tsp.el ends here
