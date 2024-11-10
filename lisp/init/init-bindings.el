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

(keymap-global-set "C-=" '("Expand region" . er/expand-region))

;; The default binding, M-SPC, is taken by GNOME.
(keymap-global-set "C-c SPC" '("Cycle spacing" . cycle-spacing))

;;; <leader> l --- <localleader>
(which-key-add-key-based-replacements "C-c l" "<localleader>")

;;; <leader> c --- code
(which-key-add-key-based-replacements "C-c c" "code-prefix")
(keymap-global-set "C-c c d" '("Go to definition" . xref-find-definitions))
(keymap-global-set "C-c c f" '("Format buffer/region" . format-all-region-or-buffer))
(keymap-global-set "C-c c k" '("Go to documentation" . nil))
(keymap-global-set "C-c c i" '("Organize imports (LSP)" . eglot-code-action-organize-imports))
(keymap-global-set "C-c c r" '("Rename (LSP)" . eglot-rename))

;;; <leader> f --- file
(which-key-add-key-based-replacements "C-c f" "file-prefix")
(keymap-global-set "C-c f d" '("Create ad hoc directory" . tsp/create-ad-hoc-directory))
(keymap-global-set "C-c f r" '("Rename this file" . rename-visited-file))
(keymap-global-set "C-c f m" '("Rename/move this file" . rename-visited-file))
(keymap-global-set "C-c f p" '("Find file in personal config" . tsp/find-file-in-emacs-config))

;;; <leader> n --- notes
(which-key-add-key-based-replacements "C-c n" "notes-prefix")
(keymap-global-set "C-c n a" '("Org agenda" . org-agenda))
(keymap-global-set "C-c n d" '("Open deft" . deft))
(keymap-global-set "C-c n l" '("Org store link" . org-store-link))
(keymap-global-set "C-c n n" '("Org capture" . org-capture))

;;; <leader> n r --- notes > roam
(which-key-add-key-based-replacements "C-c n r" "notes-roam-prefix")
(keymap-global-set "C-c n r f" '("Find node" . org-roam-node-find))
(keymap-global-set "C-c n r g" '("Show graph" . org-roam-graph))
(keymap-global-set "C-c n r i" '("Insert node" . org-roam-node-insert))
(keymap-global-set "C-c n r r" '("Toggle roam buffer" . org-roam-buffer-toggle))

;;; <leader> o --- open
(which-key-add-key-based-replacements "C-c o" "open-prefix")
(keymap-global-set "C-c o d" '("Open shortdoc" . shortdoc-display-group))
(keymap-global-set "C-c o e" '("Open eshell" . eshell))
(keymap-global-set "C-c o o" '("Open eshell" . eshell))
(keymap-global-set "C-c o t" '("Open shell" . shell))

;;; <leader> s --- search
(which-key-add-key-based-replacements "C-c s" "search-prefix")
(keymap-global-set "C-c s g" '("Search in files (regexp)" . consult-ripgrep))
(keymap-global-set "C-c s i" '("Jump to symbol" . consult-imenu))
(keymap-global-set "C-c s m" '("Jump to mark" . consult-mark))
(keymap-global-set "C-c s M" '("Jump to global mark" . consult-global-mark))
(keymap-global-set "C-c s o" '("Jump to outline heading" . consult-outline))
(keymap-global-set "C-c s r" '("Ripgrep" . rg-menu))
(keymap-global-set "C-c s s" '("Search buffer" . consult-line))
(keymap-global-set "C-c s S" '("Search and replace (regexp)" . query-replace-regexp))

;;; <leader> t --- toggle
(which-key-add-key-based-replacements "C-c t" "toggle-prefix")
(keymap-global-set "C-c t f" '("Flymake" . flymake-mode))
(keymap-global-set "C-c t m" '("Menu bar" . menu-bar-mode))
(keymap-global-set "C-c t p" '("Electric Pair" . electric-pair-local-mode))

;;; <leader> v --- versioning
(which-key-add-key-based-replacements "C-c v" "versioning-prefix")
(keymap-global-set "C-c v b" '("Magit blame" . magit-blame-addition))
(keymap-global-set "C-c v B" '("Magit blame (menu)" . magit-blame))
(keymap-global-set "C-c v t" '("Git time machine" . git-timemachine-toggle))
(keymap-global-set "C-c v y" '("URL to current location" . git-link))
(keymap-global-set "C-c v Y" '("URL to current commit" . git-link-commit))

;;; <Leader> w --- workspaces/windows
(which-key-add-key-based-replacements "C-c w" "windows-prefix")
(keymap-global-set "C-c w s" '("Swap window position" . window-swap-states))
(keymap-global-set "C-c w u" '("Undo window config" . winner-undo))
(keymap-global-set "C-c w U" '("Redo window config" . winner-redo))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'init-bindings)
;;; init-bindings.el ends here
