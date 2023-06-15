;;; package.el --- package.el init code              -*- lexical-binding: t; -*-
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

(cl-defmethod +pkg-ensure-package (package-recipe)
  (let* ((package (car package-recipe))
         (archive (cadr package-recipe))
         (pkg (if archive
                  (seq-find (lambda (desc)
                              (string= (package-desc-archive desc) archive))
                            (cdr (assoc package package-archive-contents)))
                package)))
    (unless (package-installed-p package)
      (package-install pkg))))

;;; package.el ends here
