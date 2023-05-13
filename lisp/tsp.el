;;; tsp.el --- Centralized place for my custom defuns, macros, etc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun tsp/create-binding-for-reformatter (keymap key reformatter)
  "Bind KEY in KEYMAP to run REFORMATTER on region or buffer."
  (let* ((reformatter-name (symbol-name reformatter))
         (reformat-region-or-buffer-function (intern (concat reformatter-name "-region-or-buffer")))
         (reformat-region-function (intern (concat reformatter-name "-region")))
         (reformat-buffer-function (intern (concat reformatter-name "-buffer"))))
    (defalias reformat-region-or-buffer-function
      `(lambda ()
         (interactive)
         (if (use-region-p)
             (,reformat-region-function (region-beginning) (region-end))
           (,reformat-buffer-function)))
      (format "Run \"%s\" on region if the region is active, otherwise run it on buffer.\n\nThis is a generated function." reformatter-name))
    (define-key keymap key reformat-region-or-buffer-function)))

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
