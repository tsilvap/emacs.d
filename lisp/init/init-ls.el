;;; init-ls.el --- LSP-related configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup eglot
  (setq-default eglot-workspace-configuration
                '(:yaml (:keyOrdering nil))))

(provide 'init-ls)
;;; init-ls.el ends here
