;;; straight.el --- straight.el init code            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "vendor-lisp/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (load bootstrap-file nil 'nomessage))

(cl-defmethod +pkg-ensure-package (melpa-style-recipe
                                   &optional no-clone no-build cause)
  (straight-use-package melpa-style-recipe no-clone no-build cause))

(setq straight-recipe-repositories
      '(gnu-elpa-mirror nongnu-elpa org-elpa melpa el-get emacsmirror-mirror))

;;; straight.el ends here
