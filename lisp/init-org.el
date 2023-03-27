;;; init-org.el --- Org-related configuration        -*- lexical-binding: t; -*-
;;; Commentary:

;; Setup inspired by Steve Purcell's and Bernt Hansen's GTD Org
;; workflows.

;;; Code:

;;;; Org Mode

(setq org-directory "~/Dropbox/org")

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(defun tsp/org-roam-files ()
  "Return the list of Org Roam files.
For use as a target for Org refile."
  (directory-files-recursively org-roam-directory "\\.org\\'"))

(defun tsp/skip-subtree-if-stuck (&optional reverse)
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

(defun tsp/skip-subtree-unless-stuck ()
  "Skip subtree unless it has next actions."
  (tsp/skip-subtree-if-stuck t))

(with-eval-after-load 'org
  ;; Basic configuration.
  (setq
   org-catch-invisible-edits 'show-and-error
   org-clock-sound "~/Music/ding.wav"
   org-return-follows-link t)

  ;; GTD: Configure allowed values for effort and energy required.
  (setq org-global-properties
        '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00")
          ("Energy_ALL" . "* ** ***")))

  ;; Eye candy.
  (setq
   org-auto-align-tags nil
   org-ellipsis "↴"
   org-hide-emphasis-markers t
   org-startup-indented t
   org-tags-column 0)

  ;; Configure Org Columns mode to be more consistent with GTD.
  (setq org-columns-default-format
        "%TODO %45ITEM %TAGS %EFFORT %3ENERGY %3PRIORITY")

  ;; Improve org-cite faces in Tomorrow Night theme.
  (when (memq 'sanityinc-tomorrow-night custom-enabled-themes)
    (set-face-attribute 'org-cite nil :inherit 'org-footnote)
    (set-face-attribute 'org-cite-key nil :inherit 'org-footnote :underline t))  

  ;; Enable and configure `habits' module.
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 50)

  ;; Org refile configuration.
  (setq
   ;; Allow refiling to all agenda files.
   org-refile-targets '((org-agenda-files :tag . "")
			(tsp/org-roam-files :tag . ""))

   ;; Allow refiling to topmost level (instead of refiling as a
   ;; subheading of an existing heading).
   org-refile-use-outline-path 'file)

  ;; Org capture configuration.
  (setq org-default-notes-file (concat org-directory "/agenda/inbox.org")
        org-capture-templates '(("i" "Inbox" entry
                                 (file org-default-notes-file)
                                 "* %?")))

  ;; Define stuck projects to be any projects that don't have a next
  ;; action in their subtree.
  (setq org-stuck-projects '("-someday/PROJECT" ("NEXT") nil ""))

  ;; Org "TODO" keywords configuration.
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ; A task that needs doing
                                        ; eventually.
           "NEXT(n)"                    ; A next action (as per GTD).
           "|"
           "DONE(d!/!)")                ; Completed task.
          (sequence
           "PROJECT(p)"                 ; A project (as per GTD).
           "|"
           "DONE(d!|!)"                 ; Completed project.
           "CANCELLED(c@/!)")           ; Cancelled project.
          (sequence
           "WAITING(w@/!)"              ; Something external is
                                        ; holding up this task.
           "DELEGATED(e!)"              ; Task has been delegated to
                                        ; someone else.
           "HOLD(h)"                    ; Task paused/on hold because
                                        ; of me.
           "|"
           "CANCELLED(c@/!)")))         ; Task was cancelled, aborted,
                                        ; or is no longer applicable.
  
  ;; Org agenda configuration.
  (setq org-agenda-files '("~/Dropbox/org/agenda/")
        org-agenda-compact-blocks t
        org-agenda-start-on-weekday nil
	org-agenda-custom-commands
	'(("p" "Agenda and personal tasks"
	   ((agenda "" nil)
	    (tags "INBOX"
		  ((org-agenda-overriding-header "Inbox")))
	    (tags-todo "personal+TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Next Actions")))
	    (tags-todo "personal+TODO=\"PROJECT\""
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'tsp/skip-subtree-if-stuck)))
            (tags-todo "personal+TODO=\"PROJECT\""
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'tsp/skip-subtree-unless-stuck)))
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
                        (org-agenda-skip-function 'tsp/skip-subtree-if-stuck)))
            (tags-todo "work+TODO=\"PROJECT\""
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'tsp/skip-subtree-unless-stuck)))
	    (tags-todo "work+TODO=\"WAITING\""
                       ((org-agenda-overriding-header "Waiting")))))

	  ("y" "Agenda and someday/maybe tasks"
	   ((agenda "" nil)
	    (tags "INBOX"
		  ((org-agenda-overriding-header "Inbox")))
	    (tags "someday"
		  ((org-agenda-overriding-header "Someday / Maybe")
		   (org-tags-match-list-sublevels nil)))))))

  ;; Open Org agenda in a full frame window.
  (require 'fullframe)
  (fullframe org-agenda org-agenda-quit)

  ;; Org cite configuration, using citar.
  (setq org-cite-global-bibliography '("~/Dropbox/bib/references.bib")
        org-cite-activate-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-insert-processor 'citar
        citar-bibliography org-cite-global-bibliography)
  (define-key org-mode-map (kbd "C-c b") #'org-cite-insert))

;;;; Org Roam --- For Zettelkasten notes.

(setq org-roam-directory "~/Dropbox/org/roam/")

(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n g") #'org-roam-graph)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n j") #'org-roam-dailies-capture-today)

(with-eval-after-load 'org-roam
  ;; Org Roam capture templates, inspired by Jethro Kuan's setup.
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :target (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory
       (file-relative-name (org-roam-node-file node) org-roam-directory)))))

  ;; Show node type and tags in minibuffer completion.
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (setq citar-notes-paths (list (concat org-roam-directory "reference")))

  (org-roam-db-autosync-mode))

;;;; Deft --- For general reference notes.

(require 'deft)

(setq deft-directory "~/Dropbox/org/reference/"
      deft-use-filename-as-title t
      deft-recursive t)

(global-set-key (kbd "C-x C-g") 'deft)

(provide 'init-org)
;;; init-org.el ends here
