;;; init-completion.el --- Buffer completion configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup emacs
  ;; Make TAB complete at point (after current line is indented).
  (:option tab-always-indent 'complete))

;;;; Corfu - Enhance `completion-at-point' by displaying a completion
;;;; pop-up.

(setup corfu
  ;; Enable Corfu globally. This is consistent with the fact that
  ;; Dabbrev can be used globally.
  (global-corfu-mode))

;;;; Cape - `completion-at-point' extensions.

(setup cape
  ;; Dedicated completion commands.
  (:global "C-c p p" completion-at-point
           "C-c p t" complete-tag
           "C-c p d" cape-dabbrev
           "C-c p h" cape-history
           "C-c p f" cape-file
           "C-c p k" cape-keyword
           "C-c p s" cape-symbol
           "C-c p a" cape-abbrev
           "C-c p i" cape-ispell
           "C-c p l" cape-line
           "C-c p w" cape-dict
           "C-c p \\" cape-tex
           "C-c p _" cape-tex
           "C-c p ^" cape-tex
           "C-c p &" cape-sgml
           "C-c p r" cape-rfc1345)

  ;; Add a few useful Cape functions to Capfs.
  (dolist (cape-func '(cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions cape-func t)))

(provide 'init-completion)
;;; init-completion.el ends here
