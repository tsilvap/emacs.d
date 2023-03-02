;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prefer `cperl-mode' over `perl-mode'.
(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'cperl-mode
  (let ((indent-level 4))
    (setq cperl-close-paren-offset (- indent-level)
	  cperl-continued-statement-offset indent-level
	  cperl-indent-level indent-level
	  cperl-indent-parens-as-block t
	  cperl-invalid-face 'default))

  ;; Formatting
  (require 'reformatter)
  (require 'tsp)

  (reformatter-define perltidy :program "perltidy" :args '("-" "--standard-output"))
  (tsp/create-binding-for-formatter "perltidy" cperl-mode-map))

(provide 'init-perl)
;;; init-perl.el ends here
