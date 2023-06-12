;;; init-treesit.el --- Configuration related to Tree-Sitter  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defsubst +treesit-language-available-p (language &optional detail)
  "Compatibility expansion for `treesit-language-available-p'."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language detail)))

;; Based on:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter#fixing-the-s-expression-commands
(defun +treesit-remove-sexp-changes ()
  "Revert change to S-Expression commands to their default behavior."
  (when (and (boundp 'forward-sexp-function)
             (eq forward-sexp-function #'treesit-forward-sexp))
    (setq forward-sexp-function nil))
  (when (and (boundp 'forward-sentence-function)
             (eq forward-sentence-function #'treesit-forward-sentence))
    (setq forward-sentence-function #'forward-sentence-default-function))
  (when (and (boundp 'transpose-sexps-function)
             (eq transpose-sexps-function #'treesit-transpose-sexps))
    (setq transpose-sexps-function #'transpose-sexps-default-function)))

(setup prog-mode
  (:hook +treesit-remove-sexp-changes))

(setup combobulate
  (:commands combobulate-mode)
  (:hook-into yaml-ts-mode))

(provide 'init-treesit)
;;; init-treesit.el ends here
