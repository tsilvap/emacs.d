;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'go-mode
  ;;;; Formatting
  (setq gofmt-command "goimports")
  (define-key go-mode-map (kbd "C-c f") 'gofmt)

  ;; Enable Org Babel code block evaluation for Go.
  (require 'ob-go))

(provide 'init-go)
;;; init-go.el ends here
