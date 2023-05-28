;;; init-setup.el --- Setup.el configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'setup)

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature))))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t)

(setup-define :diminish
  (lambda ()
    `(diminish ',(setup-get 'mode)))
  :documentation "Diminish the current mode."
  :after-loaded t)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :documentation-handler
  (lambda (handler)
    `(:bind "C-c c k" ,handler))
  :documentation "Set HANDLER as the documentation handler in current mode."
  :debug '(sexp)
  :ensure '(func))

(setup-define :flymake-flycheck-backend
  (lambda (checker)
    `(:local-hook flymake-diagnostic-functions
                  (flymake-flycheck-diagnostic-function-for ',checker)))
  :documentation "Set the Flycheck CHECKER as a Flymake backend in current mode."
  :repeatable t)

(setup-define :localleader
  (lambda (key command)
    `(:bind ,(concat "C-c l " key) ,command))
  :documentation "Bind KEY to COMMAND in localleader map."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(provide 'init-setup)
;;; init-setup.el ends here
