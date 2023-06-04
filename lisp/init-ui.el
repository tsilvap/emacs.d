;;; init-ui.el --- UI customization                  -*- lexical-binding: t; -*-
;;; Commentary:

;; Ideally, we require this file as soon as possible, so that Emacs
;; doesn't look ugly while loading the rest of the configuration.

;;; Code:

;; Set theme.
(setup color-theme-sanityinc-tomorrow
  (load-theme 'sanityinc-tomorrow-night t))

;; Customize faces.
(let ((font (if (eq system-type 'windows-nt)
		"JetBrains Mono NL 10"
	      "JetBrains Mono NL 13")))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :font font)))

;; Hide scroll bar and tool bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(if (eq system-type 'windows-nt)
    (menu-bar-mode -1))

;; Show column numbers globally.
(column-number-mode t)

;; Display line numbers in source and configuration files.
(setup display-line-numbers
  (:hook-into conf-mode prog-mode yaml-mode yaml-ts-mode))

;; Show breadcrumbs in projects.
(setup (:require breadcrumb)
  (breadcrumb-mode))

(provide 'init-ui)
;;; init-ui.el ends here
