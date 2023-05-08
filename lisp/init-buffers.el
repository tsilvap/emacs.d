;;; init-buffers.el --- Configuration related to buffers.  -*- lexical-binding: t; -*-
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
  (:global
   ;; IBuffer is an advanced replacement for the default buffer menu.
   [remap list-buffers] ibuffer

   "C-c b" tsp/switch-to-new-buffer))

;; Auto revert buffers globally.
(setup autorevert
  (global-auto-revert-mode))

(setup ibuffer-vc
  (:option ibuffer-vc-skip-if-remote t)
  (:with-mode ibuffer
    (:hook (lambda ()
             (ibuffer-vc-set-filter-groups-by-vc-root)))))

(provide 'init-buffers)
;;; init-buffers.el ends here
