;;; init-yaml.el --- YAML files customization.       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup yaml
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (boundp 'major-mode-remap-alist))  ; introduced in 29.1
      (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))))

(provide 'init-yaml)
;;; init-yaml.el ends here
