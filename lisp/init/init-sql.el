;;; init-sql.el --- Support for the SQL language.    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sqlformat
  :bind (:map sql-mode-map
              ("C-c c f" . sqlformat))
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g" "-u" "1")))

(provide 'init-sql)
;;; init-sql.el ends here
