;;; init-prog.el --- Support for general programming and managing software projects  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup bug-reference
  (:hook-into prog-mode text-mode))

(setup editorconfig
  (:hide-mode)
  (editorconfig-mode))

(setup just-mode
  (:autoload just-mode)
  (:file-match "/[Jj]ustfile\\'"
               "\\.[Jj]ust\\(file\\)?\\'"))

(provide 'init-prog)
;;; init-prog.el ends here
