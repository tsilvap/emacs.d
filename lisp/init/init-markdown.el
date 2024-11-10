;;; init-markdown.el --- Markdown language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c l e" . markdown-export)
              ("C-c l l" . markdown-live-preview-mode)
              ("C-c l o" . markdown-open)
              ("C-c l p" . markdown-preview)
              ("C-c l i c" . markdown-insert-code)
              ("C-c l i C" . markdown-insert-gfm-code-block))
  :custom
  (markdown-command "pandoc")
  :config
  (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

(provide 'init-markdown)
;;; init-markdown.el ends here
