;;; init-sml.el --- Support for the SML programming language  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup sml-mode
  (:localleader
   "'" run-sml                          ; run SML
   "e b" sml-prog-proc-send-buffer      ; run buffer
   "e f" sml-send-function              ; run paragraph
   "e r" sml-prog-proc-send-region      ; run region
   )
  (:when-loaded
    (:option sml-program-name "sml")))

(provide 'init-sml)
;;; init-sml.el ends here
