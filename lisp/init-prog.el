;;; init-prog.el --- Support for general programming and managing software projects  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup bug-reference
  (:hook-into text-mode)
  (:with-mode bug-reference-prog-mode
    (:hook-into prog-mode))
  (:bind-into bug-reference-map
    "C-c C-o" bug-reference-push-button))

(setup editorconfig
  (:hide-mode)
  (editorconfig-mode))

;;;; Major modes for common file formats

(setup dockerfile-ts-mode
  (:file-match "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"))

(setup just-mode
  (:commands just-mode)
  (:file-match "/[Jj]ustfile\\'"
               "\\.[Jj]ust\\(file\\)?\\'"))

(provide 'init-prog)
;;; init-prog.el ends here
