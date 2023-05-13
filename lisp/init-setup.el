;;; init-setup.el --- Setup.el configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'setup)

(setup-define :diminish
  (lambda ()
    `(diminish ',(setup-get 'mode)))
  :documentation "Diminish the current mode."
  :after-loaded t)

(setup-define :documentation-handler
  (lambda (handler)
    `(:bind "C-c c k" ,handler))
  :documentation "Set HANDLER as the documentation handler in buffers of the
current mode."
  :debug '(sexp)
  :ensure '(func))

(setup-define :localleader
  (lambda (key command)
    `(:bind (concat "C-c l " ,key) ,command))
  :documentation "Bind KEY to COMMAND in localleader map."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(provide 'init-setup)
;;; init-setup.el ends here
