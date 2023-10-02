;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup cperl-mode
  (:override-major-mode perl-mode)

  ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
  ;; respects the value of `tab-always-indent'.
  (:bind [remap cperl-indent-command] indent-for-tab-command)

  (:localleader "o" flymake-perlcritic-browse-policy)
  (:face cperl-array-face ((t (:inherit font-lock-variable-name-face :foreground "#57aff6" :weight bold)))
         cperl-hash-face ((t (:inherit font-lock-variable-name-face :foreground "#ff656a" :slant italic :weight bold))))

  (:when-loaded
    (let ((indent-level 4))
      (:option cperl-close-paren-offset (- indent-level)
               cperl-continued-statement-offset indent-level
               cperl-file-style "PBP"
               cperl-indent-level indent-level
               cperl-indent-parens-as-block t
               cperl-invalid-face 'default)
      (:local-set (remove flymake-diagnostic-functions) 'perl-flymake))))

(setup flymake-perlcritic
  (:autoload flymake-perlcritic-setup)
  (:with-mode (perl-mode cperl-mode)
    (:hook flymake-perlcritic-setup))
  (:when-loaded
    (:option flymake-perlcritic-severity 1)))

(setup reply
  (:commands run-reply))

(provide 'init-perl)
;;; init-perl.el ends here
