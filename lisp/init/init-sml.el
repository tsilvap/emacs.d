;;; init-sml.el --- Support for the SML programming language  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sml-mode
  :bind (:map sml-mode-map
              ("C-c l '" . run-sml)                     ; run SML
              ("C-c l e b" . sml-prog-proc-send-buffer) ; run buffer
              ("C-c l e f" . sml-send-function)         ; run paragraph
              ("C-c l e r" . sml-prog-proc-send-region) ; run region
              )
  :custom
  (sml-program-name "sml"))

(provide 'init-sml)
;;; init-sml.el ends here
