;;; init-sh.el --- Support for shell scripting languages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup sh-script
  (:with-mode sh-mode
    (:file-match "\\.bashrc\\'")))

(provide 'init-sh)
;;; init-sh.el ends here
