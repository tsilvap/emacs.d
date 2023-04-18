;;; init-irc.el --- IRC client configuration         -*- lexical-binding: t; -*-
;;; Commentary:

;; You can configure the nickname and password for each server by
;; adding entries to .authinfo.gpg like so:
;;
;;   machine irc.libera.chat login mynickname password ****

;;; Code:

(with-eval-after-load 'erc
  ;; Make C-c RET (or C-c C-RET) send messages instead of RET.
  (define-key erc-mode-map (kbd "RET") nil)
  (dolist (keys '("C-c RET" "C-c C-RET"))
    (define-key erc-mode-map (kbd keys) 'erc-send-current-line))

  (setq erc-modules '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications pcomplete readonly ring stamp track))
  (erc-update-modules))

(provide 'init-irc)
;;; init-irc.el ends here
