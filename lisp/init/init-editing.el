;;; init-editing.el --- General editing configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable visual-fill-column-mode in text modes.
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-fill-column-mode)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(add-hook 'after-init-hook #'delete-selection-mode)

;; Make file executable if it starts with a shebang line.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setopt
 ;; Indent with spaces, not tabs. This can be overridden for certain
 ;; modes that prefer tabs, e.g. `go-mode'.
 indent-tabs-mode nil

 ;; Use single space after sentences.
 ;;
 ;; It's standard typographic practice. Double spacing is acceptable
 ;; in monospaced fonts, though even there single spacing is more
 ;; common nowadays. Using double spacing only when using monospaced
 ;; fonts is also inconvenient: you'd use double spacing in code
 ;; comments, plain-text email, etc. but would have to switch to
 ;; single spacing everywhere else where proportional fonts are used,
 ;; thus making it harder to develop muscle memory.
 ;;
 ;; Another argument is that double spacing unambiguously determines
 ;; the end of sentence (so sentence-selection commands work
 ;; correctly), but that is not so. Consider:
 ;;
 ;;   Bob said, "I get three apples.  You get two apples.  Ok?"
 ;;
 ;; The first sentence is not `Bob said, "I get three apples.', though
 ;; that's what Emacs would detect. If the goal is to accurately
 ;; determine the end of sentences, we must use more sophisticated
 ;; heuristics; a naïve search for `. ' won't do.
 sentence-end-double-space nil)

;; Enable Auto-Insert mode.
(use-package autoinsert
  :config
  (auto-insert-mode))

;; Enable Electric Pair mode globally, for automatic bracket pairing.
(use-package elec-pair
  :config
  (electric-pair-mode))

;; Edit files with sudo.
(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

;; Enable ws-butler globally, for cleaning up trailing whitespace
;; (only in lines we've edited).
(use-package ws-butler
  :diminish ws-butler-mode
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

;; Use dumb-jump for languages that don't have a nice language server
;; or good editor support for jump-to-definition (e.g. Perl).
(use-package dumb-jump
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Multiple cursors. Macros can do everything that this can do, but
;; often times using multiple cursors is simpler.
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(provide 'init-editing)
;;; init-editing.el ends here
