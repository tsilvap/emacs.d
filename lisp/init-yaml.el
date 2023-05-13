;;; init-yaml.el --- YAML files configuration        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prefer the YAML tree-sitter major mode.
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(setup yaml-ts-mode
  ;; Set `indent-line-function' to `yaml-indent-line'. Ideally this
  ;; should be set automatically by `yaml-ts-mode'... when this is
  ;; fixed we can remove this block.
  (:also-load yaml-mode)
  (:hook (lambda ()
           (setq-local indent-line-function #'yaml-indent-line))))

(provide 'init-yaml)
;;; init-yaml.el ends here
