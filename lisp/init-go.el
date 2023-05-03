;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:bind
   "C-c f" eglot-format-buffer
   "C-c l d" godoc
   "C-c l r" eglot-rename

   ;; We use this for fixing the imports since gopls doesn't seem to
   ;; do that.
   "C-c l i" goimports)
  (:when-loaded
   (:option gofmt-command "goimports")
   (defalias 'goimports #'gofmt
     "Format the current buffer using goimports.")))

(provide 'init-go)
;;; init-go.el ends here
