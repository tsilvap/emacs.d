;;; init-sh.el --- Support for shell scripting languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup sh-script
  (:with-mode sh-mode
    (:file-match "\\.bashrc\\'")
    (:localleader "o" tsp/sh-shellcheck-browse-description)))

(defun tsp/sh-shellcheck-browse-description (code)
  "Open the description page of a Shellcheck diagnostic.
CODE is the code of the diagnostic, for example, \"SC1118\"."
  (interactive
   (let ((msgs-at-point (mapcar #'flymake-diagnostic-text
                                (flymake-diagnostics (point))))
         (msgs-in-buffer (mapcar #'flymake-diagnostic-text
                                 (flymake-diagnostics))))
     (cl-flet ((diag-code (diag-msg)
                 (and (string-match "SC[0-9]+" diag-msg)
                      (match-string 0 diag-msg))))
       (list (diag-code
              (completing-read "ShellCheck diagnostic: "
                               (append msgs-at-point msgs-in-buffer)
                               nil nil nil t
                               (when msgs-at-point
                                 (car msgs-at-point))))))))
  (browse-url (format "https://www.shellcheck.net/wiki/%s" code)))

(provide 'init-sh)
;;; init-sh.el ends here
