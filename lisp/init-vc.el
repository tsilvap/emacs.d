;;; init-vc.el --- Version control configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'fullframe)

(setup vc
  ;; Follow symlinks to files under VC without asking.
  (:option vc-follow-symlinks t))

(setup magit
  (:global "C-c g" magit-file-dispatch)
  (:option magit-diff-refine-hunk t
           magit-no-confirm '(trash set-and-push))
  ;; Open Magit status buffer in a full frame window.
  (fullframe magit-status magit-mode-bury-buffer))

(setup magit-todos
  (:load-after magit)
  (:when-loaded
    (magit-todos-mode)))

(provide 'init-vc)
;;; init-vc.el ends here
