;;; init-minibuffer.el --- Minibuffer configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setc enable-recursive-minibuffers t)

;; Save minibuffer history. This integrates well with Vertico, since
;; it sorts by history position.
(setup savehist
  (:option history-length 1000)
  (savehist-mode))

;; Enable "Recentf mode" to show recent files in `consult-buffer'.
(setup recentf
  (:option recentf-max-saved-items 1000)
  (recentf-mode))

;;;; Vertico --- vertical minibuffer completion UI

(setup vertico
  ;; Do not allow the cursor in the minibuffer prompt.
  (:with-mode minibuffer-setup
    (:hook cursor-intangible-mode))
  (:option minibuffer-prompt-properties
           '(read-only t face minibuffer-prompt cursor-intangible t))

  ;; Emacs 28: Hide commands in M-x which do not work in the current
  ;; mode.
  (if (fboundp 'command-completion-default-include-p)
      (:option read-extended-command-predicate
               #'command-completion-default-include-p))

  (vertico-mode))

;;;; Orderless --- orderless completion style

(setup orderless
  (:option completion-styles '(orderless basic)
           completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Marginalia --- helpful annotations in the minibuffer

(setup marginalia
  (:global "M-A" marginalia-cycle)
  (marginalia-mode))

;;;; Embark --- contextual actions at point

(setup embark
  (:global "C-." embark-act
           "C-;" embark-dwim
           "C-h B" embark-bindings)
  ;; Show the commands bound in a prefix in a `completing-read'
  ;; interface.
  (:option prefix-help-command #'embark-prefix-help-command))

;;;; Consult --- enhanced search and navigation commands

(setup consult
  (:global
   ;; C-c bindings in `mode-specific-map'
   "C-c m" consult-man
   "C-c i" consult-info
   ;; C-x bindings in `ctl-x-map'
   [remap switch-to-buffer] consult-buffer
   [remap switch-to-buffer-other-window] consult-buffer-other-window
   [remap switch-to-buffer-other-frame] consult-buffer-other-frame
   [remap bookmark-jump] consult-bookmark
   [remap project-switch-to-buffer] consult-project-buffer
   ;; M-g bindings in `goto-map'
   "M-g i" consult-imenu
   "M-g I" consult-imenu-multi
   ;; M-s bindings in `search-map'
   "M-s r" consult-ripgrep
   ;; Misc
   [remap yank-pop] consult-yank-pop    ; a better `yank-pop'
   )
  ;; Use Consult to select xref locations with preview.
  (:option xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref))

;; Embark-Consult integration.
(setup embark-consult
  (:with-mode embark-collect
    (:hook consult-preview-at-point-mode)))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
