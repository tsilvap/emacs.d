;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'reformatter)
(require 'tsp)

;; Prefer `cperl-mode' over `perl-mode'.
(if (boundp 'major-mode-remap-alist)
    (add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))
  (defalias 'perl-mode 'cperl-mode))

(use-package cperl-mode
  ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
  ;; respects the value of `tab-always-indent'.
  :bind ([remap cperl-indent-command] . indent-for-tab-command)
  :config
  (let ((indent-level 4))
    (setq cperl-close-paren-offset (- indent-level)
          cperl-indent-level indent-level
          cperl-indent-parens-as-block t
          cperl-invalid-face 'default))

  ;; Create binding for running perltidy.
  (reformatter-define perltidy :program "perltidy" :args '("-" "--standard-output"))
  (tsp/create-binding-for-formatter "perltidy" cperl-mode-map))

(provide 'init-perl)
;;; init-perl.el ends here
