;; -*- lexical-binding: t; -*-

(setq gnus-select-method nil)
(add-to-list 'gnus-secondary-select-methods '((nnhackernews "")
					      (nnreddit "")))

;; Render HTML emails with gnus-w3m.
(setq mm-text-html-renderer 'gnus-w3m)

;; Initialize BBDB for Gnus and Message mode.
(bbdb-initialize 'gnus 'message)
