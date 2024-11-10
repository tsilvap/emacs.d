;;; init-sh.el --- Support for shell scripting languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(use-package sh-script
  :mode ("\\.bashrc\\'" . sh-mode)
  :bind (:map sh-mode-map
              ("C-c l o" . tsp/sh-shellcheck-browse-description)))

(provide 'init-sh)
;;; init-sh.el ends here
