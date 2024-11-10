;;; init-buffers.el --- Configuration related to buffers  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defun tsp/switch-to-new-buffer ()
  "Display a new empty buffer and switch to it."
  (interactive)
  (cl-flet
      ((random-alnum-string (len)
         (let* ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                (alnum-length (length alnum)))
           (apply #'string
                  (cl-loop repeat len
                           collect (aref alnum (random alnum-length)))))))
    (let ((buffer-name (format "tmp.%s"
                               (random-alnum-string 10))))
      (switch-to-buffer buffer-name))))

  (keymap-global-set "C-c b" #'tsp/switch-to-new-buffer)

;; Auto revert buffers globally.
(use-package autorevert
  :config
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
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setopt ibuffer-saved-filters
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
  (add-hook 'ibuffer-mode-hook #'tsp/ibuffer-switch-to-default-filter-groups)

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
                'ibuffer-buffer-size (buffer-size))))

(use-package ibuffer-vc
  :after (ibuffer)
  :config
  ;; Group remote files for whitelisted hosts.
  (setopt ibuffer-vc-skip-if-remote nil
          ibuffer-vc-include-function
          (lambda (file)
            (or (not (file-remote-p file))
                (member (file-remote-p file 'host)
                        tsp/ibuffer-whitelisted-remote-hosts)))))

(provide 'init-buffers)
;;; init-buffers.el ends here
