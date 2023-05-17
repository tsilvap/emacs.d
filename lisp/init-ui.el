;;; init-ui.el --- UI customization                  -*- lexical-binding: t; -*-
;;; Commentary:

;; Ideally, we require this file as soon as possible, so that Emacs
;; doesn't look ugly while loading the rest of the configuration.

;;; Code:

;; Customize faces.
(let ((font (if (eq system-type 'windows-nt)
		"JetBrains Mono NL 10"
	      "JetBrains Mono NL 13")))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :font font)))

;; Set theme.
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; Hide scroll bar and tool bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(if (eq system-type 'windows-nt)
    (menu-bar-mode -1))

;; Display line numbers in source code/config/etc. files.
(dolist (hook '(conf-mode-hook prog-mode-hook yaml-mode-hook yaml-ts-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode t))))

;; Show column numbers globally.
(column-number-mode t)

;; Show breadcrumbs in projects.
(setup breadcrumb
  (:autoload breadcrumb-mode breadcrumb-local-mode)
  (breadcrumb-mode))

(provide 'init-ui)
;;; init-ui.el ends here
