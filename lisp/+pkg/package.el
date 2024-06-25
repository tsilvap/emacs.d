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

(cl-defmethod +pkg-ensure-package (package &key vc)
  "Install PACKAGE, if not already installed.
If VC is not nil, install package with `package-vc', otherwise
install it using `package'."
  (when (or (not (package-installed-p package))
            (package-built-in-p package))
    (if vc
	(package-vc-install package)
      (package-install package))))

;;; package.el ends here
