;;; init-syntax-check.el --- Syntax checker configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'flymake-mode))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(provide 'init-syntax-check)
;;; init-syntax-check.el ends here
