;;; init-vc.el --- Version control configuration     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup vc
  ;; Follow symlinks to files under VC without asking.
  (:option vc-follow-symlinks t))

(setup magit
  (:global "C-c g" magit-file-dispatch)
  (:fullframe (magit-status magit-project-status)
              magit-mode-bury-buffer)
  (:when-loaded
    (:option magit-diff-refine-hunk t
             magit-no-confirm '(trash set-and-push))))

(setup magit-todos
  (:load-after magit)
  (:when-loaded
    (magit-todos-mode)))

(provide 'init-vc)
;;; init-vc.el ends here
