;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (remove-hook 'flymake-diagnostic-functions
                                          'elisp-flymake-byte-compile t)))))
