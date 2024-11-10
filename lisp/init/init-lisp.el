;;; init-lisp.el --- Lisp language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package paredit
  ;; NOTE: `emacs-lisp-mode' inherits from `lisp-data-mode'.
  :hook ((lisp-data-mode scheme-mode) . enable-paredit-mode)
  :config
  ;; By default, C-j runs `print-eval-last-sexp' in Lisp Interaction
  ;; mode, but Paredit overrides this binding to run `paredit-C-j'.
  ;;
  ;; Here, we restore the C-j binding in Lisp Interaction mode. See:
  ;; https://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (let ((old-map (cdr (assoc 'paredit-mode minor-mode-map-alist)))
                    (new-map (make-sparse-keymap)))
                (set-keymap-parent new-map old-map)
                (define-key new-map (kbd "C-j") nil)
                (make-local-variable 'minor-mode-overriding-map-alist)
                (push `(paredit-mode . ,new-map) minor-mode-overriding-map-alist)))))

(use-package paren-face
  :init
  (custom-set-faces '(parenthesis ((t (:inherit shadow)))))
  :config
  (global-paren-face-mode))

;;;; Common Lisp

(use-package lisp-mode
  :bind (:map lisp-mode-map
              ("C-c l o c" . tsp/sh-shellcheck-browse-description))
  :custom
  (inferior-lisp-program "sbcl"))

(use-package sly
  :defer t
  :config
  (require 'sly-autoloads nil t))

(use-package sly-mrepl
  :defer t
  :bind (:map sly-mrepl-mode-map
              ("RET" . default-indent-new-line)
              ("C-j" . sly-mrepl-return)))

(defvar +cl--cookbook-pages-alist
  (with-temp-buffer
    (insert-file-contents (locate-user-emacs-file
                           "lisp/+cl/cookbook-pages-alist.el"))
    (read (current-buffer))))

(defun +cl-browse-cookbook (page)
  "Open PAGE from Common Lisp Cookbook using a configurable method."
  (interactive
   (list (completing-read "Common Lisp Cookbook page: "
                          +cl--cookbook-pages-alist nil t)))
  (let ((page-filename (alist-get page
                                  +cl--cookbook-pages-alist nil nil #'string=)))
    (browse-url (format "https://lispcookbook.github.io/cl-cookbook/%s"
                        page-filename))))

(provide 'init-lisp)
;;; init-lisp.el ends here
