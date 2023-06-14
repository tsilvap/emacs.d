;;; init-bindings.el --- Custom bindings configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;; Here we define a lot of useful bindings with the "leader key"
;; ("C-c") prefix. The "C-c l" prefix is called "local leader"
;; throughout this config, and is supposed to be a keymap containing
;; bindings that are specific/local to each major mode, whereas the
;; rest of the "C-c" bindings are supposed to be more general and work
;; across many major modes.

;; This is largely borrowed, or otherwise inspired, by the Doom Emacs
;; bindings (which in turn, is inspired by the Spacemacs's bindings).

;;; Code:

(setup emacs
  (:global
   ;; Cycle spacing (the default, M-SPC, is taken by GNOME)
   "C-c SPC" cycle-spacing

;;; <leader> c --- code
   ;; Go to definition
   "C-c c d" xref-find-definitions
   ;; Format buffer/region
   "C-c c f" format-all-buffer
   ;; Go to documentation
   "C-c c k" '()
   ;; Organize imports (LSP)
   "C-c c i" eglot-code-action-organize-imports
   ;; Rename (LSP)
   "C-c c r" eglot-rename

;;; <leader> f --- file
   ;; Create ad hoc directory
   "C-c f d" tsp/create-ad-hoc-directory

;;; <leader> n --- notes
   ;; Org agenda
   "C-c n a" org-agenda
   ;; Open deft
   "C-c n d" deft
   ;; Org store link
   "C-c n l" org-store-link
   ;; Org capture
   "C-c n n" org-capture

;;;; <leader> n j --- notes > journal
   ;; New entry
   "C-c n j j" org-journal-new-entry
   ;; New scheduled entry
   "C-c n j J" org-journal-new-scheduled-entry
   ;; Search forever
   "C-c n j s" org-journal-search-forever

;;;; <leader> n r --- notes > roam
   ;; Find node
   "C-c n r f" org-roam-node-find
   ;; Show graph
   "C-c n r g" org-roam-graph
   ;; Insert node
   "C-c n r i" org-roam-node-insert
   ;; Toggle roam buffer
   "C-c n r r" org-roam-buffer-toggle

;;; <leader> s --- search
   ;; Jump to symbol
   "C-c s i" consult-imenu
   ;; Ripgrep
   "C-c s r" rg-menu
   ;; Search buffer
   "C-c s s" consult-line

;;; <leader> v --- versioning
   ;; Remote URL to current location
   "C-c v y" git-link
   ;; Repository homepage URL
   "C-c v Y" git-link-homepage

;;; <Leader> w --- workspaces/windows
   ;; Swap window position
   "C-c w s" window-swap-states
   ;; Undo window config
   "C-c w u" winner-undo
   ;; Redo window config
   "C-c w U" winner-redo))

(provide 'init-bindings)
;;; init-bindings.el ends here
