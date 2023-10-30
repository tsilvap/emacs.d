;;; init-sql.el --- Support for the SQL language.    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup sqlformat
  (:bind-into sql-mode-map "C-c c f" sqlformat)
  (:option sqlformat-command 'pgformatter
           sqlformat-args '("-s2" "-g")))

(provide 'init-sql)
;;; init-sql.el ends here
