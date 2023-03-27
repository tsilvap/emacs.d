;;; init-completion.el --- Buffer completion configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Make TAB complete at point (after current line is indented).
(setq tab-always-indent 'complete)

;;; Corfu - Enhance `completion-at-point' by displaying a completion
;;; pop-up.
(require 'corfu)

;; Enable Corfu globally. This is consistent with the fact that
;; Dabbrev can be used globally.
(global-corfu-mode t)

;; Add a few useful Cape functions to `completion-at-point-functions'.
(require 'cape)
(dolist (cape-func '(cape-file cape-dabbrev))
  (add-to-list 'completion-at-point-functions cape-func t))

;; Bind dedicated completion commands.
(dolist (cmd '(("C-c p p" . completion-at-point)
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
               ("C-c p r" . cape-rfc1345)))
  (global-set-key (kbd (car cmd)) (cdr cmd)))

(provide 'init-completion)
;;; init-completion.el ends here
