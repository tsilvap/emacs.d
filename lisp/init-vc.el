;;; init-vc.el --- Version control configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'fullframe)

;; Follow symlinks to files under VC without asking.
(setq vc-follow-symlinks t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-file-dispatch)
  :config
  (setq magit-diff-refine-hunk t
        magit-no-confirm '(trash set-and-push))

  ;; Open Magit status buffer in a full frame window.
  (fullframe magit-status magit-mode-bury-buffer))

(provide 'init-vc)
;;; init-vc.el ends here
