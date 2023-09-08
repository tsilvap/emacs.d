;;; init-org.el --- Org-related configuration        -*- lexical-binding: t; -*-
;;; Commentary:

;; Setup inspired by Steve Purcell's and Bernt Hansen's GTD Org
;; workflows.

;;; Code:

;;;; Org mode

(setup org
  (:bind "C-M-<return>" org-insert-subheading)

  ;; Set `org-directory' before load since we define other variables
  ;; in terms of it, for example `org-agenda-files'.
  (:option org-directory (concat tsp/sync-directory "org"))

  (:when-loaded
    ;; Basic configuration.
    (:option org-auto-align-tags nil
             org-catch-invisible-edits 'show-and-error
             org-clock-sound "~/Music/ding.wav"
             org-ellipsis "↴"
             org-format-latex-options (plist-put org-format-latex-options
                                                 :scale 1.7)
             org-hide-emphasis-markers t
             org-return-follows-link t
             org-startup-indented t
             org-tags-column 0
             (append org-modules) 'org-habit)

    ;; Customization to make it easier to implement GTD.
    (:option
     org-columns-default-format
     "%TODO %45ITEM %TAGS %EFFORT %3ENERGY %3PRIORITY"
     org-global-properties
     '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 6:00 8:00")
       ("Energy_ALL" . "* ** ***"))
     org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)"
                 "|" "DONE(d!/!)")
       (sequence "PROJECT(p)"
                 "|" "DONE(d!|!)" "CANCELLED(c@/!)")
       (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)"
                 "|" "CANCELLED(c@/!)")))))

(setup org-habit
  (:when-loaded
    (:option org-habit-graph-column 50)))

(setup org-capture
  (:when-loaded
    (:option org-capture-templates
             `(("i" "Inbox" entry
                (file ,(concat org-directory "/agenda/inbox.org"))
                "* %?\n%U")))))

(setup org-agenda
  (:fullframe org-agenda org-agenda-quit)
  (:when-loaded
    (:option org-agenda-files (list (concat org-directory "/agenda/"))
             org-agenda-compact-blocks t
             org-agenda-include-diary t
             org-agenda-start-on-weekday nil

             ;; Define stuck projects to be any projects that don't
             ;; have a next action in their subtree.
             org-stuck-projects '("-someday/PROJECT" ("NEXT") nil ""))

    ;; Custom agenda views.
    (:option
     org-agenda-custom-commands
     '(("p" "Agenda and personal tasks"
        ((agenda "" nil)
         (tags "INBOX"
	       ((org-agenda-overriding-header "Inbox")))
         (tags-todo "personal+TODO=\"NEXT\""
                    ((org-agenda-overriding-header "Next Actions")))
         (tags-todo "personal+TODO=\"PROJECT\""
                    ((org-agenda-overriding-header "Projects")
                     (org-agenda-skip-function '+org--skip-subtree-if-stuck)))
         (tags-todo "personal+TODO=\"PROJECT\""
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
	        (org-tags-match-list-sublevels nil)))))))))

(setup ox
  (:when-loaded
    (:option org-export-global-macros
             '(("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@")))))

(setup oc
  (:also-load citar)

  ;; Set `org-cite-global-biblography' before load since we define
  ;; other variables in terms of it, for example `citar-bibliography'.
  (:option org-cite-global-bibliography
           (list (concat tsp/sync-directory "bib/references.bib")))

  (:when-loaded
    (:option org-cite-activate-processor 'citar
             org-cite-follow-processor 'citar
             org-cite-insert-processor 'citar)))

(setup ob
  ;; Lazy load Org Babel libraries (ob-*.el). Taken from:
  ;;   https://gist.github.com/hlissner/14b42de71c65945f55a31b393af0391b
  (advice-add #'org-babel-confirm-evaluate
              :after-while #'+org--babel-lazy-load-library-a)

  (:when-loaded
    (:option org-confirm-babel-evaluate nil)))

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

(setup citar
  (:when-loaded
    (:option citar-bibliography org-cite-global-bibliography
             citar-notes-paths (list (concat org-roam-directory "reference")))))

;;;; Org Roam --- For Zettelkasten notes.

(setup org-roam
  ;; Set `org-roam-directory' before load since we define other
  ;; variables in terms of it, for example `citar-notes-paths'.
  (:option org-roam-directory (concat org-directory "/roam/"))

  (:when-loaded
    ;; Org Roam capture templates, inspired by Jethro Kuan's setup.
    (:option
     org-roam-capture-templates
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
    (:option org-roam-node-display-template
             (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

    (org-roam-db-autosync-mode)))

;;;; Deft --- For general reference notes.

(setup deft
  (:after-init 10
    (:option deft-directory (concat org-directory "/reference/")))
  (:option deft-use-filename-as-title t
           deft-recursive t))

;;;; org-journal --- Journaling in Emacs.

(setup org-journal
  (:after-init 10
    (:option org-journal-dir (concat org-directory "/journal")))
  (:when-loaded
    (:option org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\""
             org-journal-enable-agenda-integration t
             org-journal-file-format "%Y%m%d.org")))

(provide 'init-org)
;;; init-org.el ends here
