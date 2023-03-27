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

(provide 'init-completion)
;;; init-completion.el ends here
