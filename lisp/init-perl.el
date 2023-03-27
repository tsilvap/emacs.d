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
  (tsp/create-binding-for-formatter "perltidy" cperl-mode-map)

  ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
  ;; respects the value of `tab-always-indent'.
  (define-key cperl-mode-map [remap cperl-indent-command] #'indent-for-tab-command))

(provide 'init-perl)
;;; init-perl.el ends here
