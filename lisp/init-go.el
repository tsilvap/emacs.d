;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:documentation-handler godoc)

  ;; We use `goimports' for fixing the imports since gopls doesn't
  ;; seem to do that.
  (:localleader "i" goimports)

  (:when-loaded
    (:option gofmt-command "goimports")
    (defalias 'goimports #'gofmt
      "Format the current buffer using goimports.")))

(provide 'init-go)
;;; init-go.el ends here
