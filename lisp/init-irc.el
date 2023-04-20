;;; init-irc.el --- IRC client configuration         -*- lexical-binding: t; -*-
;;; Commentary:

;; You can configure the nickname and password for each server by
;; adding entries to .authinfo.gpg like so:
;;
;;   machine irc.libera.chat login mynickname password ****

;;; Code:

(setup erc
  ;; Make C-c RET (or C-c C-RET) send messages instead of RET.
  (:rebind "RET" "C-c RET")
  (:bind "C-c C-RET" erc-send-current-line)

  (:option erc-modules
           '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications pcomplete readonly ring stamp track)))

(provide 'init-irc)
;;; init-irc.el ends here