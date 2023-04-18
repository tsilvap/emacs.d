;;; init-setup.el --- Setup.el configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'setup)

(setup-define :diminish
  (lambda ()
    `(diminish ',(setup-get 'mode)))
  :documentation "Diminish the current mode."
  :after-loaded t)

(provide 'init-setup)
;;; init-setup.el ends here
