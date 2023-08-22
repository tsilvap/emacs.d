;;; init-buffers.el --- Configuration related to buffers  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar tsp--alnum
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "String containing all the ASCII alphanumeric characters.")

(defun tsp/switch-to-new-buffer ()
  "Display a new empty buffer and switch to it."
  (interactive)
  (let ((buffer-name (format "tmp.%s"
                             (mapconcat
                              (lambda (x) (tsp--random-alnum))
                              "xxxxxxxxxx" ""))))
    (switch-to-buffer buffer-name)))

(defun tsp--random-alnum ()
  "Return a random alphanumeric character."
  (let ((i (% (abs (random)) (length tsp--alnum))))
    (substring tsp--alnum i (1+ i))))

(setup emacs
  (:global "C-c b" tsp/switch-to-new-buffer))

;; Auto revert buffers globally.
(setup autorevert
  (global-auto-revert-mode))

(defvar tsp/ibuffer-whitelisted-remote-hosts '()
  "List of remote hosts to group in the VC groups in IBuffer.
Any remote files whose hosts are not in this list won't be
grouped.

Ideally you should only whitelist hosts that won't incur
performance issues for IBuffer: for example, the host of a VM
that is running locally.")

(defun tsp/ibuffer-switch-to-default-filter-groups ()
  "Switch to a default list of filter groups."
  (interactive)
  (setq ibuffer-filter-groups
        `(,@(ibuffer-vc-generate-filter-groups-by-vc-root)
          ("Org"
           (saved . "org"))
          ("ERC"
           (saved . "erc"))
          ("Gnus"
           (saved . "gnus"))))
  (ibuffer-update nil t))

;; IBuffer is an advanced replacement for the default buffer menu.
(setup ibuffer
  (:global [remap list-buffers] ibuffer)
  (:option ibuffer-saved-filters
           '(("erc"
              (mode . erc-mode))
             ("gnus"
              (or
               (mode . message-mode)
               (mode . mail-mode)
               (mode . gnus-group-mode)
               (mode . gnus-summary-mode)
               (mode . gnus-article-mode)
               (name . "^\\.newsrc-dribble")
               (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
               (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\|mm\\*\\)")
               (name . "\\(Original Article\\|canonical address\\|extract address\\)")))
             ("org"
              (or
               (mode . org-mode)
               (name . "^\\*Calendar\\*$")
               (name . "^\\*Org Agenda")
               (name . "^ \\*Agenda")
               (name . "^diary$"))))
           ibuffer-show-empty-filter-groups nil)
  (:hook #'tsp/ibuffer-switch-to-default-filter-groups)
  (:when-loaded
    ;; Redefine size column to display human readable size.
    (define-ibuffer-column size
      (:name "Size"
       :inline t
       :header-mouse-map ibuffer-size-header-map
       :summarizer
       (lambda (column-strings)
         (let ((total 0))
           (dolist (string column-strings)
             ;; It is what it is...
             (when-let ((size (get-text-property
                               0 'ibuffer-buffer-size (string-trim string))))
               (setq total (+ (float size) total))))
           (file-size-human-readable total))))
      (propertize (file-size-human-readable (buffer-size))
                  'ibuffer-buffer-size (buffer-size)))))

(setup ibuffer-vc
  (:load-after ibuffer)
  (:when-loaded
    ;; Group remote files for whitelisted hosts.
    (:option ibuffer-vc-skip-if-remote nil
             ibuffer-vc-include-function
             (lambda (file)
               (or (not (file-remote-p file))
                   (member (file-remote-p file 'host)
                           tsp/ibuffer-whitelisted-remote-hosts))))))

(provide 'init-buffers)
;;; init-buffers.el ends here
