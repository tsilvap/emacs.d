;;; tsp.el --- Centralized place for my custom defuns, macros, etc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun tsp/create-binding-for-formatter (formatter keymap)
  "Create binding in KEYMAP to run FORMATTER on region or buffer."
  (let ((format-region-or-buffer-function (intern (concat formatter "-region-or-buffer")))
        (format-region-function (intern (concat formatter "-region")))
        (format-buffer-function (intern (concat formatter "-buffer"))))
    (defalias format-region-or-buffer-function
      `(lambda ()
         (interactive)
         (if (use-region-p)
             (,format-region-function (region-beginning) (region-end))
           (,format-buffer-function)))
      (format "Run %s on region if the region is active, otherwise run it on buffer.\n\nThis is a generated function." formatter))
    (define-key keymap (kbd "C-c f") format-region-or-buffer-function)))

(defun tsp/create-adhoc-directory ()
  "Create adhoc directory to place files, scripts, etc."
  (interactive)
  (let ((adhoc-directory-name
         (concat "~/Misc/adhoc/"
                 (format-time-string "%Y-%m-%d-%H%M%S" (current-time)))))
    (make-directory adhoc-directory-name t)
    (find-file adhoc-directory-name)))

(provide 'tsp)
;;; tsp.el ends here
