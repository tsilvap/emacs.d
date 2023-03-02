;;; init-sh.el --- Support for shell scripting languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'sh-script
  ;; Formatting
  (require 'reformatter)
  (require 'tsp)

  (reformatter-define shfmt :program "shfmt" :args '("-"))
  (tsp/create-binding-for-formatter "shfmt" sh-mode-map)

  ;; Syntax checking
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(provide 'init-sh)
;;; init-sh.el ends here
