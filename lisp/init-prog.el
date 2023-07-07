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
  (:file-match "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"))

;;;;; justfile

;; See: https://just.systems/man/en/
(setup just-mode
  (:commands just-mode)
  (:file-match "/[Jj]ustfile\\'"
               "\\.[Jj]ust\\(file\\)?\\'"))

;;;;; web-mode.el: web template files

;; Used to edit HTML with embed parts (CSS/JavaScript) or template
;; blocks (like Go's `html/template', Django templates, Ruby's ERB).
;;
;; If needed, you can customize `web-mode-engines-alist' per-project
;; in a .dir-locals.el file.
(setup web-mode
  (:file-match "\\.html?\\'"
               "\\.phtml\\'"
               "\\.tpl\\.php\\'"
               "\\.[agj]sp\\'"
               "\\.as[cp]x\\'"
               "\\.erb\\'"
               "\\.mustache\\'"
               "\\.djhtml\\'")
  (:hook
   (lambda ()
     (electric-pair-local-mode -1))

   ;; HACK: needed to make setting web-mode engine in .dir-locals.el
   ;; work correctly.
   ;;
   ;; Based on:
   ;;   https://emacs.stackexchange.com/a/59709
   ;;   https://www.emacswiki.org/emacs/LocalVariables#h5o-2
   (lambda ()
     (add-hook 'hack-local-variables-hook
               (lambda ()
                 (web-mode-guess-engine-and-content-type)
                 (web-mode-buffer-fontify))
               nil t)))
  (:when-loaded
    (:option web-mode-markup-indent-offset 2
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 2)))

(provide 'init-prog)
;;; init-prog.el ends here
