;;; init-go.el --- Go language configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup go-mode
  (:localleader "p" go-playground)
  (:hook apheleia-mode
         eglot-ensure)
  (:local-set apheleia-formatter 'goimports
              tab-width 4))

(setup go-playground
  (:localleader "r" go-playground-rm))

(provide 'init-go)
;;; init-go.el ends here
