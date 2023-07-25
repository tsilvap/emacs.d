;;; init-prog.el --- Support for general programming and managing software projects  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup project
  ;; Load magit-extras, which contains a hook that adds Magit to
  ;; `project-switch-commands'. That way, "Magit" always shows up when
  ;; running `project-switch-project'.
  (:also-load magit-extras))

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

;;;;; Dockerfile

(setup dockerfile-ts-mode
  (:file-match "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
               "\\(?:Containerfile\\(?:\\..*\\)?\\|\\.[Cc]ontainerfile\\)\\'"))

;;;;; JSON

(setup json-ts-mode
  (:file-match "\\.json\\'"))

;;;;; justfile

;; See: https://just.systems/man/en/
(setup just-mode
  (:commands just-mode)
  (:file-match "/[Jj]ustfile\\'"
               "\\.[Jj]ust\\(file\\)?\\'"))


(provide 'init-prog)
;;; init-prog.el ends here
