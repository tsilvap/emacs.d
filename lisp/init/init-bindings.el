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
  (:gkey
   "C-=" ("Expand region" . er/expand-region)

   ;; The default binding, M-SPC, is taken by GNOME.
   "C-c SPC" ("Cycle spacing" . cycle-spacing))

  ;; <leader> l --- <localleader>
  (which-key-add-key-based-replacements
    "C-c l" "<localleader>")

  ;; <leader> c --- code
  (which-key-add-key-based-replacements
    "C-c c" "code-prefix")
  (:gkey
   "C-c c d" ("Go to definition" . xref-find-definitions)
   "C-c c f" ("Format buffer/region" . format-all-region-or-buffer)
   "C-c c k" ("Go to documentation" . nil)
   "C-c c i" ("Organize imports (LSP)" . eglot-code-action-organize-imports)
   "C-c c r" ("Rename (LSP)" . eglot-rename))

  ;; <leader> f --- file
  (which-key-add-key-based-replacements
    "C-c f" "file-prefix")
  (:gkey
   "C-c f d" ("Create ad hoc directory" . tsp/create-ad-hoc-directory)
   "C-c f r" ("Rename this file" . rename-visited-file)
   "C-c f m" ("Rename/move this file" . rename-visited-file)
   "C-c f p" ("Find file in personal config" . tsp/find-file-in-emacs-config))

  ;; <leader> n --- notes
  (which-key-add-key-based-replacements
    "C-c n" "notes-prefix")
  (:gkey
   "C-c n a" ("Org agenda" . org-agenda)
   "C-c n d" ("Open deft" . deft)
   "C-c n l" ("Org store link" . org-store-link)
   "C-c n n" ("Org capture" . org-capture))

  ;; <leader> n r --- notes > roam
  (which-key-add-key-based-replacements
    "C-c n r" "notes-roam-prefix")
  (:gkey
   "C-c n r f" ("Find node" . org-roam-node-find)
   "C-c n r g" ("Show graph" . org-roam-graph)
   "C-c n r i" ("Insert node" . org-roam-node-insert)
   "C-c n r r" ("Toggle roam buffer" . org-roam-buffer-toggle))

  ;; <leader> o --- open
  (which-key-add-key-based-replacements
    "C-c o" "open-prefix")
  (:gkey
   "C-c o d" ("Open shortdoc" . shortdoc-display-group)
   "C-c o e" ("Open eshell" . eshell)
   "C-c o o" ("Open eshell" . eshell)
   "C-c o t" ("Open shell" . shell))

  ;; <leader> s --- search
  (which-key-add-key-based-replacements
    "C-c s" "search-prefix")
  (:gkey
   "C-c s g" ("Search in files (regexp)" . consult-ripgrep)
   "C-c s i" ("Jump to symbol" . consult-imenu)
   "C-c s m" ("Jump to mark" . consult-mark)
   "C-c s M" ("Jump to global mark" . consult-global-mark)
   "C-c s o" ("Jump to outline heading" . consult-outline)
   "C-c s r" ("Ripgrep" . rg-menu)
   "C-c s s" ("Search buffer" . consult-line)
   "C-c s S" ("Search and replace (regexp)" . query-replace-regexp))

  ;; <leader> t --- toggle
  (which-key-add-key-based-replacements
    "C-c t" "toggle-prefix")
  (:gkey
   "C-c t f" ("Flymake" . flymake-mode)
   "C-c t m" ("Menu bar" . menu-bar-mode)
   "C-c t p" ("Electric Pair" . electric-pair-local-mode))

  ;; <leader> v --- versioning
  (which-key-add-key-based-replacements
    "C-c v" "versioning-prefix")
  (:gkey
   "C-c v b" ("Magit blame" . magit-blame-addition)
   "C-c v B" ("Magit blame (menu)" . magit-blame)
   "C-c v t" ("Git time machine" . git-timemachine-toggle)
   "C-c v y" ("URL to current location" . git-link)
   "C-c v Y" ("URL to current commit" . git-link-commit))

  ;; <Leader> w --- workspaces/windows
  (which-key-add-key-based-replacements
    "C-c w" "windows-prefix")
  (:gkey
   "C-c w s" ("Swap window position" . window-swap-states)
   "C-c w u" ("Undo window config" . winner-undo)
   "C-c w U" ("Redo window config" . winner-redo)))

(setup which-key
  (:hide-mode)
  (which-key-mode))

(provide 'init-bindings)
;;; init-bindings.el ends here
