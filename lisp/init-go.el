;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:bind "C-c f" gofmt)
  (:option gofmt-command "goimports"))

(provide 'init-go)
;;; init-go.el ends here
