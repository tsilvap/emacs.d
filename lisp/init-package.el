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

(defun tsp/ensure-package (package)
  "Install PACKAGE, if it is not installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Get package list from the file packages.txt, and ensure they are
;; installed.
(let ((packages (mapcar #'intern
                        (split-string
                         (with-temp-buffer
                           (insert-file-contents
                            (concat user-emacs-directory "packages.txt"))
                           (buffer-string))))))
  (dolist (pkg packages)
    (tsp/ensure-package pkg)))

(provide 'init-package)
;;; init-package.el ends here
