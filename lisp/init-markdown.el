;;; init-markdown.el --- Markdown language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(with-eval-after-load 'markdown-mode
  (setq markdown-command "pandoc"))

(provide 'init-markdown)
;;; init-markdown.el ends here
