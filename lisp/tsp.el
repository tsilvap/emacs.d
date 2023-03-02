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

(provide 'tsp)
;;; tsp.el ends here
