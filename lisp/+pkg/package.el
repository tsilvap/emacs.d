;;; package.el --- package.el init code              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(dolist (archive '(("gnu" . "https://elpa.gnu.org/packages/")
		   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")
                   ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive t))

;; Prefer the built-in repos (higher number is higher priority here).
;; We prefer MELPA over MELPA Stable; we only use the latter in
;; specific cases.
(setq package-archive-priorities
      '(("elpa" . 100)
	("nongnu" . 10)
        ("melpa" . 2)
        ("melpa-stable" . 1)))

(package-initialize)

(cl-defmethod +pkg-ensure-package (package-recipe)
  "Install a package, if not already installed.
PACKAGE-RECIPE specifies the instructions to install the package:
the only required information is the package name, but it can
also specify a package archive, or whether to install from
source, and so on."
  (let* ((package (car package-recipe))
         (props (cadr package-recipe))
         (archive (plist-get props :archive))
         (pkg (if archive
                  (seq-find (lambda (desc)
                              (string= (package-desc-archive desc) archive))
                            (cdr (assoc package package-archive-contents)))
                package)))
    (unless (package-installed-p package)
      (package-install pkg))))

;;; package.el ends here
