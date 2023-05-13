;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:documentation-handler godoc)

  ;; We use `goimports' for fixing the imports since gopls doesn't
  ;; seem to do that.
  (:localleader "i" goimports
                "p" go-playground)

  (:when-loaded
    (:option gofmt-command "goimports")
    (defalias 'goimports #'gofmt
      "Format the current buffer using goimports.")))

(setup go-playground
  (:localleader "r" go-playground-rm))

(provide 'init-go)
;;; init-go.el ends here
