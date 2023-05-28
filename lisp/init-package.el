;;; init-package.el --- Package management configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(dolist (archive '(("gnu" . "https://elpa.gnu.org/packages/")
		   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives archive t))

;; Prefer the built-in repos (higher number is higher priority here).
(setq package-archive-priorities
      '(("elpa" . 3)
	("nongnu" . 2)
        ("melpa" . 1)))

(package-initialize)

(defvar tsp/packages-file
  (concat user-emacs-directory "packages.el")
  "File that contains the list of selected packages.
Also contains any additional metadata, such as the package
archive or repository URL to use for a specific package.")

(defun tsp/ensure-package (package &optional archive)
  "Install PACKAGE, if it is not installed.

Optionally specify the ARCHIVE to get the package from."
  (let ((pkg (if archive
                 (seq-find (lambda (desc)
                             (string= (package-desc-archive desc) archive))
                           (cdr (assoc package package-archive-contents)))
               package)))
    (unless (package-installed-p package)
      (package-install pkg))))

(defun tsp/package-install (pkg)
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
        	    (insert-file-contents tsp/packages-file)
        	    (read (current-buffer)))))
    (add-to-list 'packages (list pkg))
    (sort packages (lambda (a b)
                     (let ((pkg-a (car a))
                           (pkg-b (car b)))
                       (string-lessp (symbol-name pkg-a)
                                     (symbol-name pkg-b)))))
    (with-temp-file tsp/packages-file
      (insert ";;; -*- lisp-data -*-\n")
      (pp packages (current-buffer)))))

(defun tsp/install-selected-packages ()
  "Ensure the user's selected packages they are installed."
  (interactive)
  (let ((packages (with-temp-buffer
		    (insert-file-contents tsp/packages-file)
		    (read (current-buffer)))))
    (dolist (pkg packages)
      (apply #'tsp/ensure-package pkg))))

(tsp/install-selected-packages)

(provide 'init-package)
;;; init-package.el ends here
