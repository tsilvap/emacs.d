;;; init-gnus.el --- Gnus configuration              -*- lexical-binding: t; -*-
;;; Commentary:

;; I'm currently using Gnus to read and send email (using ProtonMail
;; Bridge), and to follow news feeds, mailings lists, etc. via Gmane's
;; NNTP server.

;;; Code:

(setup gnus
  (:when-loaded
    (:option gnus-select-method '(nnimap "personal-mail"
                                         (nnimap-address "127.0.0.1")
                                         (nnimap-server-port 1144)
                                         (nnimap-stream plain))
             gnus-secondary-select-methods '((nntp "news.gmane.io")))

    ;; Sync Gnus directory and startup files.
    (:option gnus-directory "~/Dropbox/News"
             gnus-startup-file "~/Dropbox/.newsrc")

    ;; Render HTML emails with gnus-w3m.
    (:option mm-text-html-renderer 'gnus-w3m)

    ;; Send email via SMTP.
    (:option message-send-mail-function 'smtpmail-send-it
             smtpmail-smtp-server "127.0.0.1"
             smtpmail-smtp-service 1026)

    ;; Initialize BBDB for Gnus and Message mode.
    (bbdb-initialize 'gnus 'message)))

(provide 'init-gnus)
;;; init-gnus.el ends here
