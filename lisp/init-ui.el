;;; init-ui.el --- UI customization                  -*- lexical-binding: t; -*-
;;; Commentary:

;; Ideally, we require this file as soon as possible, so that Emacs
;; doesn't look ugly while loading the rest of the configuration.

;;; Code:

;; Set theme.
(setup ef-themes
  (load-theme 'ef-elea-dark t))

;; Customize faces.
(let ((font (if (eq system-type 'windows-nt)
		"JetBrains Mono NL 10"
	      "JetBrains Mono NL 13")))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :font font)))

(setup emacs
  ;; Hide scroll bar and tool bar.
  (:option scroll-bar-mode nil
           tool-bar-mode nil)

  ;; Menu bar looks ugly on Windows, since it doesn't inherit the dark
  ;; mode, so disable it there by default.
  (if (eq system-type 'windows-nt)
      (:option menu-bar-mode nil))

  ;; Show column numbers globally.
  (:option column-number-mode t))

;; Display line numbers in source and configuration files.
(setup display-line-numbers
  (:hook-into conf-mode prog-mode yaml-mode yaml-ts-mode))

;; Show breadcrumbs in projects.
(setup (:require breadcrumb)
  (breadcrumb-mode))

(provide 'init-ui)
;;; init-ui.el ends here
