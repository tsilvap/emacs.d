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

(add-hook 'prog-mode-hook #'+treesit-remove-sexp-changes)

;;; Taken from the README of: https://github.com/mickeynp/combobulate
(defun +treesit-setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
    (add-to-list 'treesit-language-source-alist grammar)

    ;; Only install `grammar' if we don't already have it installed.
    ;; However, if you want to *update* a grammar then this obviously
    ;; prevents that from happening.
    (unless (+treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(use-package combobulate
  :hook (json-ts-mode yaml-ts-mode)
  :config
  (+treesit-setup-install-grammars))

(provide 'init-treesit)
;;; init-treesit.el ends here
