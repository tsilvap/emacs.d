;;; init-python.el --- Python language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Formatting
(with-eval-after-load 'python
  (require 'reformatter)
  (require 'tsp)

  (reformatter-define black :program "black" :args '("-"))
  (tsp/create-binding-for-formatter "black" python-mode-map))

(provide 'init-python)
;;; init-python.el ends here
