;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  ;; We use `goimports' for fixing the imports since gopls doesn't
  ;; seem to do that.
  (:localleader "i" goimports
                "p" go-playground))

(setup go-playground
  (:localleader "r" go-playground-rm))

(provide 'init-go)
;;; init-go.el ends here
