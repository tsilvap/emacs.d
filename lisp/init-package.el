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

(defun tsp/install-selected-packages ()
  "Ensure the user's selected packages they are installed.

The list of selected packages is in the packages.el file in
`user-emacs-directory'."
  (interactive)
  (let ((packages (with-temp-buffer
		    (insert-file-contents
		     (concat user-emacs-directory "packages.el"))
		    (read (current-buffer)))))
    (dolist (pkg packages)
      (apply #'tsp/ensure-package pkg))))

(tsp/install-selected-packages)

(provide 'init-package)
;;; init-package.el ends here
