;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c c k" . godoc-at-point))
  :custom
  (godoc-at-point-function 'godoc-gogetdoc)
  (godoc-reuse-buffer t)
  (godoc-use-completing-read t)
  :config
  (add-hook 'go-mode-hook #'apheleia-mode)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local apheleia-formatter 'goimports
                          tab-width 4))))

(provide 'init-go)
;;; init-go.el ends here
