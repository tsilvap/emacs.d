;;; init-perl.el --- Perl language configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup cperl-mode
  ;; Prefer `cperl-mode' over `perl-mode'.
  (if (boundp 'major-mode-remap-alist)
      (add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

    ;; (defalias 'perl-mode 'cperl-mode) is supposedly a pithier way
    ;; of doing this, but it would sometimes load `perl-mode' anyway.
    ;; The code below works consistently for me.
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'perl-mode)
           (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist)))

  ;; Remap `cperl-indent-command' to `indent-for-tab-command', which
  ;; respects the value of `tab-always-indent'.
  (:bind [remap cperl-indent-command] indent-for-tab-command)

  (:documentation-handler cperl-perldoc)

  (:when-loaded
    (let ((indent-level 4))
      (:option cperl-close-paren-offset (- indent-level)
               cperl-continued-statement-offset indent-level
               cperl-electric-keywords t
               cperl-indent-level indent-level
               cperl-indent-parens-as-block t
               cperl-invalid-face 'default))

    ;; Use PBP indentation style.
    (:option cperl-file-style "PBP")))

(provide 'init-perl)
;;; init-perl.el ends here
