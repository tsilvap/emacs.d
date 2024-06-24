;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:bind "C-c c k" godoc-at-point)
  (:localleader "p" go-playground)
  (:hook apheleia-mode
         eglot-ensure)
  (:local-set apheleia-formatter 'goimports
              tab-width 4)
  (:when-loaded
    (:option godoc-at-point-function 'godoc-gogetdoc
             godoc-reuse-buffer t
             godoc-use-completing-read t)))

(setup go-playground
  (:localleader "r" go-playground-rm))

(provide 'init-go)
;;; init-go.el ends here
