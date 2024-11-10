;;; init-ui.el --- UI customization                  -*- lexical-binding: t; -*-
;;; Commentary:

;; Ideally, we require this file as soon as possible, so that Emacs
;; doesn't look ugly while loading the rest of the configuration.

;;; Code:

;; Set theme.
(use-package ef-themes
  :config
  (load-theme 'ef-elea-dark t))

;; Customize faces.
(let ((font (if (eq system-type 'windows-nt)
		"JetBrains Mono NL 10"
	      "Input Mono Narrow 13")))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :font font :weight 'regular)))

;; Hide menu bar, scroll bar and tool bar.
;;
;; Sometimes, I'll toggle the menu bar manually when using a mode I'm
;; not familiar with. But I prefer to have it off by default to save
;; space.
(setopt menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil)

;; Show column numbers globally.
(setopt column-number-mode t)

;; Display line numbers in source and configuration files.
(dolist (hook '(conf-mode-hook
                prog-mode-hook
                yaml-mode-hook
                yaml-ts-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(provide 'init-ui)
;;; init-ui.el ends here
