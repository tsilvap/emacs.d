;;; init-prog.el --- Support for general programming and managing software projects  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package project
  :defer t
  ;; Load magit-extras, which contains a hook that adds Magit to
  ;; `project-switch-commands'. That way, "Magit" always shows up when
  ;; running `project-switch-project'.
  :config
  (require 'magit-extras nil t))

(use-package bug-reference
  :hook ((text-mode . bug-reference-mode)
         (prog-mode . bug-reference-prog-mode))
  :bind (:map bug-reference-map
              ("C-c C-o" . bug-reference-push-button)))

(use-package eglot
  :hook ((typescript-ts-mode python-mode go-mode) . eglot-ensure)
  :init
  (setq-default eglot-workspace-configuration
                '(:yaml (:keyOrdering nil))))

(use-package yasnippet
  :hook ((typescript-ts-mode python-mode go-mode) . yas-minor-mode)
  :config
  (define-key yas-minor-mode-map "TAB" nil) ; TAB is already bound to `complete-symbol'
  (define-key yas-minor-mode-map "C-j" #'yas-expand))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode))

;;; Major modes for common file formats

;;;; Dockerfile

(use-package dockerfile-ts-mode
  :mode ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
         "\\(?:Containerfile\\(?:\\..*\\)?\\|\\.[Cc]ontainerfile\\)\\'"))

;;;; JSON

(use-package json-ts-mode
  :mode "\\.json\\'")

;;;; justfile

;; See: https://just.systems/man/en/
(use-package just-mode
  :mode ("/[Jj]ustfile\\'"
         "\\.[Jj]ust\\(file\\)?\\'"))

(provide 'init-prog)
;;; init-prog.el ends here
