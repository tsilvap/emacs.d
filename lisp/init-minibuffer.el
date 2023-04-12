;;; init-minibuffer.el --- Minibuffer configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; Vertico --- vertical minibuffer completion UI

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

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

;;;; Orderless --- orderless completion style

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Marginalia --- helpful annotations in the minibuffer

(use-package marginalia
  :ensure t
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

;;;; Embark --- contextual actions at point

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Show the commands bound in a prefix in a `completing-read'
  ;; interface.
  (setq prefix-help-command #'embark-prefix-help-command))

;;;; Consult --- enhanced search and navigation commands

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ;; M-g bindings in `goto-map'
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi))
  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

;; Embark-Consult integration.
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Enable "Recentf mode" to show recent files in `consult-buffer'.
(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  :init
  (recentf-mode t))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
