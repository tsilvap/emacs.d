;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'go-mode
  ;; Use goimports as the `gofmt-command'. Mainly for use when gopls is
  ;; not easily available, for example in Org Babel code block editing
  ;; buffers.
  (setq gofmt-command "goimports")

  ;; Enable Org Babel code block evaluation for Go.
  (require 'ob-go))

(provide 'init-go)
;;; init-go.el ends here
