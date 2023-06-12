;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup cperl-mode
  (:override-major-mode perl-mode)

  ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
  ;; respects the value of `tab-always-indent'.
  (:bind [remap cperl-indent-command] indent-for-tab-command)

  (:documentation-handler cperl-perldoc)

  (:when-loaded
    (let ((indent-level 4))
      (:option cperl-close-paren-offset (- indent-level)
               cperl-continued-statement-offset indent-level
               cperl-electric-keywords t
               cperl-file-style "PBP"
               cperl-indent-level indent-level
               cperl-indent-parens-as-block t
               cperl-invalid-face 'default))))

(provide 'init-perl)
;;; init-perl.el ends here
