;;; init-search.el --- Configuration related to search  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Make Xref use ripgrep instead of grep. This will speed up other
;; packages that use Xref under the hood, e.g. Dired, project.el.
;;
;; Still, as of Mar 13 2023, the `project-find-regexp' implementation
;; isn't very optimized to work with ripgrep, so searching with
;; `rg' is much faster.
(when (executable-find "rg")
  (setopt xref-search-program 'ripgrep))

;; Use ripgrep for searching files.
(use-package rg
  :if (executable-find "rg")
  :config
  (setopt rg-keymap-prefix (kbd "C-c s r"))
  (rg-enable-default-bindings))

(provide 'init-search)
;;; init-search.el ends here
