;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cperl-mode
  :bind
  (:map cperl-mode-map
        ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
        ;; respects the value of `tab-always-indent'.
        ([remap cperl-indent-command] . indent-for-tab-command)

        ("C-c c k" . cperl-perldoc)
        ("C-c l o" . flymake-perlcritic-browse-policy))
  :custom
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-file-style "PBP")
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t)
  (cperl-invalid-face 'default)
  :init
  (add-to-list 'major-mode-remap-alist
               '(perl-mode . cperl-mode))
  :config
  (add-hook 'cperl-mode-hook
            (lambda ()
              (setq-local flymake-diagnostic-functions (remove 'perl-flymake flymake-diagnostic-functions))))
  (custom-set-faces '(cperl-array-face ((t (:inherit font-lock-variable-name-face :foreground "#57aff6" :weight bold)))))
  (custom-set-faces '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :foreground "#ff656a" :slant italic :weight bold))))))

(use-package flymake-perlcritic
  :hook ((perl-mode cperl-mode) . flymake-perlcritic-setup)
  :custom
  (flymake-perlcritic-severity 1))

(use-package reply
  :commands (run-reply))

(provide 'init-perl)
;;; init-perl.el ends here
