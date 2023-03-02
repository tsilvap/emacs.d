;;; init-lisp.el --- Lisp language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable Paredit for Lisp buffers.
(autoload #'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(dolist (hook '(lisp-data-mode-hook scheme-mode-hook))
  ;; NOTE: `emacs-lisp-mode' inherits from `lisp-data-mode'.
  (add-hook hook #'enable-paredit-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here
