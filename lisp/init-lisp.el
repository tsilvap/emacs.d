;;; init-lisp.el --- Lisp language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup paredit
  (:with-function enable-paredit-mode
    ;; NOTE: `emacs-lisp-mode' inherits from `lisp-data-mode'.
    (:hook-into lisp-data-mode-hook scheme-mode-hook))

  ;; By default, C-j runs `print-eval-last-sexp' in Lisp Interaction
  ;; mode, but Paredit overrides this binding to run `paredit-C-j'.
  ;;
  ;; Here, we restore the C-j binding in Lisp Interaction mode. See:
  ;; https://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
  (:with-mode lisp-interaction-mode
    (:hook (lambda ()
             (let ((old-map (cdr (assoc 'paredit-mode minor-mode-map-alist)))
                   (new-map (make-sparse-keymap)))
               (set-keymap-parent new-map old-map)
               (define-key new-map (kbd "C-j") nil)
               (make-local-variable 'minor-mode-overriding-map-alist)
               (push `(paredit-mode . ,new-map) minor-mode-overriding-map-alist))))))

;;;; Common Lisp

(setup emacs
  (:option inferior-lisp-program "sbcl"))

(provide 'init-lisp)
;;; init-lisp.el ends here
