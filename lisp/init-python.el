;;; init-python.el --- Python language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'reformatter)
(require 'tsp)

(setup python
  (:when-loaded
    (reformatter-define black :program "black" :args '("-"))
    (tsp/create-binding-for-reformatter python-mode-map (kbd "C-c f") 'black)))

(provide 'init-python)
;;; init-python.el ends here
