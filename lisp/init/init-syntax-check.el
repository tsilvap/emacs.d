;;; init-syntax-check.el --- Syntax checker configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :hook (prog-mode text-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; Mark expression to remove `elisp-flymake-byte-compile' hook as
;; safe. We use this to disable byte-compiler warnings in ~/.emacs.d/
;; with a .dir-locals.el file.
(add-to-list
 'safe-local-eval-forms
 '(remove-hook 'flymake-diagnostic-functions 'elisp-flymake-byte-compile t))

(provide 'init-syntax-check)
;;; init-syntax-check.el ends here
