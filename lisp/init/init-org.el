;;; init-org.el --- Org-related configuration        -*- lexical-binding: t; -*-
;;; Commentary:

;; Setup inspired by Steve Purcell's and Bernt Hansen's GTD Org
;; workflows.

;;; Code:

;;;; Org mode

(setopt org-directory (concat tsp/sync-directory "org")
        org-roam-directory (concat org-directory "/roam/"))

(use-package org
  :bind (:map org-mode-map
              ("C-M-<return>" . org-insert-subheading))
  :custom
  ;; Basic configuration.
  (org-auto-align-tags nil)
  (org-catch-invisible-edits 'show-and-error)
  (org-clock-sound "~/music/ding.wav")
  (org-ellipsis "↴")
  (org-hide-emphasis-markers t)
  (org-return-follows-link t)
  (org-startup-indented t)
  (org-tags-column 0)

  ;; Customization to make it easier to implement GTD.
  (org-columns-default-format
   "%TODO %45ITEM %TAGS %EFFORT %3ENERGY %3PRIORITY")
  (org-global-properties
   '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00")
     ("Energy_ALL" . "* ** ***")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)"
               "|" "DONE(d!/!)")
     (sequence "PROJECT(p)"
               "|" "DONE(d!|!)" "CANCELLED(c@/!)")
     (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)"
               "|" "CANCELLED(c@/!)")))
  :config
  (setopt org-modules (add-to-list 'org-modules 'org-habit)
          org-format-latex-options (plist-put org-format-latex-options :scale 1.7)))

(use-package org-habit
  :custom
  (org-habit-graph-column 50))

(use-package org-timer
  :custom
  (org-timer-default-timer "25:00"))

(use-package org-capture
  :custom
  (org-capture-templates `(("i" "Inbox" entry
                            (file ,(concat org-directory "/agenda/inbox.org"))
                            "* %?\n%U"))))

(use-package org-agenda
  :custom
  (org-agenda-files (list (concat org-directory "/agenda/")
                          (concat org-roam-directory "/projects/")))
  (org-agenda-compact-blocks t)
  (org-agenda-include-diary t)
  (org-agenda-start-on-weekday nil)
  ;; Define stuck projects to be any projects that don't have a next
  ;; action in their subtree.
  (org-stuck-projects '("-someday/PROJECT" ("NEXT") nil ""))

  ;; Custom agenda views.
  (org-agenda-custom-commands
   '(("p" "Agenda and personal tasks"
      ((agenda "" nil)
       (tags "INBOX"
	         ((org-agenda-overriding-header "Inbox")))
       (tags-todo "personal+TODO=\"NEXT\""
                  ((org-agenda-overriding-header "Next Actions")))
       (tags-todo "-someday+TODO=\"PROJECT\""
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function '+org--skip-subtree-if-stuck)))
       (tags-todo "-someday+TODO=\"PROJECT\""
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function '+org--skip-subtree-unless-stuck)))
       (tags-todo "personal+TODO=\"WAITING\""
                  ((org-agenda-overriding-header "Waiting")))))
     ("w" "Agenda and work tasks"
      ((agenda "" nil)
       (tags "work+review-TODO=\"NEXT\"-TODO=\"DONE\""
	         ((org-agenda-overriding-header "Reviews")))
       (tags-todo "work+TODO=\"NEXT\""
                  ((org-agenda-overriding-header "Next Actions")))
       (tags-todo "work+TODO=\"PROJECT\""
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function '+org--skip-subtree-if-stuck)))
       (tags-todo "work+TODO=\"PROJECT\""
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function '+org--skip-subtree-unless-stuck)))
       (tags-todo "work+TODO=\"WAITING\""
                  ((org-agenda-overriding-header "Waiting")))))
     ("y" "Agenda and someday/maybe tasks"
      ((agenda "" nil)
       (tags "INBOX"
	         ((org-agenda-overriding-header "Inbox")))
       (tags "someday"
	         ((org-agenda-overriding-header "Someday / Maybe")
	          (org-tags-match-list-sublevels nil)))))))
  :config
  (fullframe org-agenda org-agenda-quit))

(use-package ox
  :custom
  (org-export-global-macros '(("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))))

(use-package oc
  :defer t
  :custom
  ;; Set `org-cite-global-biblography' before load since we define
  ;; other variables in terms of it, for example `citar-bibliography'.
  (org-cite-global-bibliography (list (concat tsp/sync-directory "bib/references.bib")))
  (org-cite-activate-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-insert-processor 'citar)
  :config
  (require 'citar nil t))

(use-package ob
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  :config
  ;; Lazy load Org Babel libraries (ob-*.el). Taken from:
  ;;   https://gist.github.com/hlissner/14b42de71c65945f55a31b393af0391b
  (advice-add #'org-babel-confirm-evaluate
              :after-while #'+org--babel-lazy-load-library-a))

(defun +org--babel-lazy-load (lang)
  (cl-check-type lang symbol)
  (or (require (intern (format "ob-%s" lang)) nil t)
      (require lang nil t)))

(defun +org--babel-lazy-load-library-a (info)
  "Load Babel libraries lazily when Babel blocks are executed."
  (let* ((lang (nth 0 info))
         (lang (cond ((symbolp lang) lang)
                     ((stringp lang) (intern lang)))))
    (when (and lang
               (not (cdr (assq lang org-babel-load-languages)))
               (+org--babel-lazy-load lang))
      (add-to-list 'org-babel-load-languages (cons lang t)))
    t))

(defun +org--skip-subtree-if-stuck (&optional reverse)
  "Skip subtree if it has next actions.
If REVERSE, then skip subtree unless it has next actions."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (is-stuck (not (re-search-forward "^\\*+[ \t]+NEXT\\>" subtree-end t)))
         (should-skip (if reverse
                          (not is-stuck)
                        is-stuck)))
    (if should-skip
        subtree-end
      nil)))

(defun +org--skip-subtree-unless-stuck ()
  "Skip subtree unless it has next actions."
  (+org--skip-subtree-if-stuck t))

;;;; Citar

(use-package citar
  :defer t
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths (list (concat org-roam-directory "reference"))))

;;;; Org Roam --- For Zettelkasten notes.

(use-package org-roam
  :custom
  (org-roam-capture-templates
   '(("m" "main" plain "%?"
      :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %t\n")
      :immediate-finish t
      :unnarrowed t)
     ("p" "projects" plain "%?"
      :target (file+head "projects/${slug}.org" "#+title: ${title}\n#+date: %t\n")
      :immediate-finish t
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "reference/${slug}.org" "#+title: ${title}\n#+date: %t\n")
      :immediate-finish t
      :unnarrowed t)
     ("t" "troubleshoot" plain "%?"
      :target (file+head "troubleshoot/${slug}.org" "#+title: ${title}\n#+date: %t\n")
      :immediate-finish t
      :unnarrowed t)))

  ;; Show node type and tags in minibuffer completion.
  (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory
     (file-relative-name (org-roam-node-file node) org-roam-directory)))))

  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode))

;;;; Deft --- For general reference notes.

(use-package deft
  :defer t
  :custom
  (deft-directory (concat org-directory "/reference/"))
  (deft-use-filename-as-title t)
  (deft-recursive t))

(provide 'init-org)
;;; init-org.el ends here
