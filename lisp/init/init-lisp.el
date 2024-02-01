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

(setup paren-face
  (:face parenthesis ((t (:inherit shadow))))
  (global-paren-face-mode))

;;;; Common Lisp

(setup lisp-mode
  (:localleader "c" ("Open CL Cookbook" . +cl-browse-cookbook))
  (:option inferior-lisp-program "sbcl"))

(setup sly
  (:require sly-autoloads))

(setup sly-mrepl
  (:bind "RET" default-indent-new-line
         "C-j" sly-mrepl-return))

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
