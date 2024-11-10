;;; init-vc.el --- Version control configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vc
  :custom
  ;; Follow symlinks to files under VC without asking.
  (vc-follow-symlinks t))

(use-package magit
  :defer t
  :bind ("C-c g" . magit-file-dispatch)
  :custom
  (magit-diff-refine-hunk t)
  (magit-no-confirm '(trash set-and-push))
  :config
  (fullframe magit-status magit-mode-bury-buffer)
  (fullframe magit-project-status magit-mode-bury-buffer))

(provide 'init-vc)
;;; init-vc.el ends here
