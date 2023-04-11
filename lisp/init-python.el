;;; init-python.el --- Python language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Formatting
(with-eval-after-load 'python
  (require 'reformatter)
  (require 'tsp)

  (reformatter-define black :program "black" :args '("-"))
  (tsp/create-binding-for-reformatter python-mode-map (kbd "C-c f") 'black))

(provide 'init-python)
;;; init-python.el ends here
