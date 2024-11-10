;;; init-completion.el --- Buffer completion configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Make TAB complete at point (after current line is indented).
(setopt tab-always-indent 'complete)

;;;; Corfu - Enhance `completion-at-point' by displaying a completion
;;;; pop-up.

(use-package corfu
  :config
  ;; Enable Corfu globally. This is consistent with the fact that
  ;; Dabbrev can be used globally.
  (global-corfu-mode))

;;;; Cape - `completion-at-point' extensions.

(use-package cape
  ;; Dedicated completion commands.
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :config
  ;; Add a few useful Cape functions to Capfs.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(provide 'init-completion)
;;; init-completion.el ends here
