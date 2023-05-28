;;; init-markdown.el --- Markdown language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup markdown-mode
  (:with-mode gfm-mode
    (:file-match "README\\.md\\'"))
  (:localleader "e" markdown-export
                "l" markdown-live-preview-mode
                "o" markdown-open
                "p" markdown-preview
                "i c" markdown-insert-code
                "i C" markdown-insert-gfm-code-block)
  (:flymake-flycheck-backend markdown-mdl)
  (:when-loaded
    (:option markdown-command "pandoc")))

(provide 'init-markdown)
;;; init-markdown.el ends here
