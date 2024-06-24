;;; init-sql.el --- Support for the SQL language.    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup sqlformat
  (:with-map sql-mode-map
    (:bind "C-c c f" sqlformat))
  (:option sqlformat-command 'pgformatter
           sqlformat-args '("-s2" "-g" "-u" "1")))

(provide 'init-sql)
;;; init-sql.el ends here
