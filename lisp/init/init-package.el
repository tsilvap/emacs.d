;;; init-package.el --- Package management configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;; I haven't decided on which package manager to use, so I've defined
;; an interface to be able to test the ones that look interesting
;; until I find the one that works for my use-cases.

;;; Code:

(defcustom +pkg-manager 'package
  "The package manager to be used in this config."
  :type '(choice (const :tag "package.el" package)
                 (const :tag "straight.el" straight))
  :tag "Package manager")

(defvar +pkg-recipes-file
  (concat user-emacs-directory "packages.el")
  "File that contains the list of selected packages.
May contain additional metadata, or \"recipes\", on how to get
the package, that is, which VC repository URL, or package
archive, and similar information.")

(cl-defgeneric +pkg-ensure-package (package)
  "Install PACKAGE, if it is not installed.")

(defun +pkg-install (pkg)
  "Install the package PKG."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package--archives-initialize)
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (unless (package-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  package-archive-contents))
                    nil t)))))
  (let ((packages (with-temp-buffer
        	    (insert-file-contents +pkg-recipes-file)
        	    (read (current-buffer)))))
    (add-to-list 'packages (list pkg))
    (sort packages (lambda (a b)
                     (let ((pkg-a (car a))
                           (pkg-b (car b)))
                       (string-lessp (symbol-name pkg-a)
                                     (symbol-name pkg-b)))))
    (with-temp-file +pkg-recipes-file
      (insert ";;; -*- lisp-data -*-\n")
      (pp packages (current-buffer)))))

(defun +pkg-install-selected-packages ()
  "Ensure the user's selected packages they are installed."
  (interactive)
  (let ((recipes (with-temp-buffer
		   (insert-file-contents +pkg-recipes-file)
		   (read (current-buffer)))))
    (dolist (pkg-recipe recipes)
      (apply #'+pkg-ensure-package pkg-recipe))))

(let ((setup-file-dir
       (expand-file-name "lisp/+pkg/" user-emacs-directory)))
  (cond ((eq +pkg-manager 'package)
         (load (file-name-concat setup-file-dir "package.el") nil t))
        ((eq +pkg-manager 'straight)
         (load (file-name-concat setup-file-dir "straight.el") nil t))
        (t
         (error "Unsupported package manager: `%s'" +pkg-manager))))

(+pkg-install-selected-packages)

(provide 'init-package)
;;; init-package.el ends here
