;;; init-lua.el --- Support for the Lua programming language  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :bind (:map lua-mode-map
              ("C-c c k" . lua-search-documentation)))

(provide 'init-lua)
;;; init-lua.el ends here
