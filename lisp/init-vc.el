;;; init-vc.el --- Version control configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Follow symlinks to files under VC without asking.
(setq vc-follow-symlinks t)

;;; Magit

(require 'magit)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(setq magit-diff-refine-hunk t
      magit-no-confirm '(trash set-and-push))

;; Open Magit status buffer in a full frame window.
(require 'fullframe)
(fullframe magit-status magit-mode-bury-buffer)

(provide 'init-vc)
;;; init-vc.el ends here
