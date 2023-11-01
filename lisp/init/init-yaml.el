;;; init-yaml.el --- YAML files configuration        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:and
        (+treesit-language-available-p 'yaml)
        yaml-ts-mode)
  (:override-major-mode yaml-mode)

  (:hook #'flymake-yamllint-setup
         (lambda () (auto-fill-mode -1)))

  ;; Set `indent-line-function' to `yaml-indent-line'. Ideally this
  ;; should be set automatically by `yaml-ts-mode'... when this is
  ;; fixed we can remove this block.
  (:also-load yaml-mode)
  (:hook (lambda ()
           (setq-local indent-line-function #'yaml-indent-line))))

(provide 'init-yaml)
;;; init-yaml.el ends here
