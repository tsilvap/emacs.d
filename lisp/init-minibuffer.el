;;; init-minibuffer.el --- Minibuffer configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Use Vertico as the minibuffer completion UI.
(vertico-mode)

;; Save minibuffer history. This integrates well with Vertico, since
;; it sorts by history position.
(savehist-mode)

;; Do not allow the cursor in the minibuffer prompt.
(setq minibuffer-prompt-properties
      '(read-only t face minibuffer-prompt cursor-intangible t))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current
;; mode.
(if (fboundp 'command-completion-default-include-p)
    (setq read-extended-command-predicate
	  #'command-completion-default-include-p))

;; Use the Orderless completion style.
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles partial-completion))))

;; Use Marginalia to show helpful annotations in the minibuffer.
(marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
