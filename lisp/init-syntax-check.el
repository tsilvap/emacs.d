;;; init-syntax-check.el --- Syntax checker configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Mark expression to remove `elisp-flymake-byte-compile' hook as
;; safe. We use this to disable byte-compiler warnings in ~/.emacs.d/
;; with a .dir-locals.el file.
(add-to-list
 'safe-local-eval-forms
 '(remove-hook 'flymake-diagnostic-functions 'elisp-flymake-byte-compile t))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'flymake-mode))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(provide 'init-syntax-check)
;;; init-syntax-check.el ends here
