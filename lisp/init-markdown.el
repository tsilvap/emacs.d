;;; init-markdown.el --- Markdown language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup markdown-mode
  (:with-mode gfm-mode
    (:file-match "README\\.md\\'"))
  (:option markdown-command "pandoc"))

(provide 'init-markdown)
;;; init-markdown.el ends here
