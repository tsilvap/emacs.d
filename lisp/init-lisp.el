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

;; By default, C-j runs `print-eval-last-sexp' in Lisp Interaction
;; mode, but Paredit overrides this binding to run `paredit-C-j'.
;;
;; Here, we restore the C-j binding in Lisp Interaction mode.
;; See: https://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (let ((old-map (cdr (assoc 'paredit-mode minor-mode-map-alist)))
                  (new-map (make-sparse-keymap)))
              (set-keymap-parent new-map old-map)
              (define-key new-map (kbd "C-j") nil)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(paredit-mode . ,new-map) minor-mode-overriding-map-alist))))

(provide 'init-lisp)
;;; init-lisp.el ends here
