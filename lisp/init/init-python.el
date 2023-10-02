;;; init-python.el --- Python language configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup python-mode
  (:hook apheleia-mode eglot-ensure yas-minor-mode))

(provide 'init-python)
;;; init-python.el ends here
