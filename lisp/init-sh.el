;;; init-sh.el --- Support for shell scripting languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'reformatter)
(require 'tsp)

(setup sh-script
  (:with-mode sh-mode
    (:hook flymake-shellcheck-load))
  (:when-loaded
    (reformatter-define shfmt :program "shfmt" :args '("-"))
    (tsp/create-binding-for-reformatter sh-mode-map (kbd "C-c f") 'shfmt)))

(provide 'init-sh)
;;; init-sh.el ends here
