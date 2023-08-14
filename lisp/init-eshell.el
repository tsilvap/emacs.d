;;; init-eshell.el --- Eshell-related configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thiago da Silva Pinto

;; Author: Thiago da Silva Pinto <thiagodasilva@protonmail.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'eshell)
(require 'iimage)
(require 'url)

(defvar tsp/wizard-zines-comics-dir
  (locate-user-emacs-file "etc/wizard-zines-comics"))

;; Borrowed and adapted from:
;; https://xenodium.com/wizard-zines-comics-eshell-util/
(defun eshell/ecomic (&rest args)
  "Display comic in ARGS.
Note: ensure comic images live in `tsp/wizard-zines-comics-dir',
named with the comic name and .png extension."
  (eshell-eval-using-options
   "ecomic" args
   '((?h "help" nil nil "show this usage screen")
     :external "ecomic"
     :show-usage
     :usage "COMIC

Show COMIC from Julia Evans's https://wizardzines.com/comics/")
   (let* ((comic-name (nth 0 (eshell-stringify-list (eshell-flatten-list args))))
          (image-fpath (expand-file-name (concat comic-name ".png")
                                         tsp/wizard-zines-comics-dir)))
     (unless (file-exists-p image-fpath)
       (error "ecomic: \"%s\" not found :-(" comic-name))
     (eshell-buffered-print "\n")
     (add-text-properties 0 (length image-fpath)
                          `(display ,(create-image image-fpath)
                                    modification-hooks
                                    (iimage-modification-hook))
                          image-fpath)
     (eshell-buffered-print image-fpath)
     (eshell-flush))))

(defun tsp/get-wizard-zines-comics ()
  "Download and save Julia Evans's Wizard Zines comics locally."
  (interactive)
  (url-retrieve
   "https://wizardzines.com/comics/"
   (lambda (status &rest args)
     (let* ((html (libxml-parse-html-region url-http-end-of-headers (point-max)))
            (image-urls
             (mapcar
              (lambda (anchor-node)
                (let* (;; `comic-page-path' is a string like "/comics/bash-tricks/".
                       (comic-page-path (dom-attr anchor-node 'href))
                       ;; `comic-name' would then be "bash-tricks".
                       (comic-name (caddr (split-string comic-page-path "/"))))
                  (format "https://wizardzines.com/images/uploads/%s.png"
                          comic-name)))
              (dom-by-tag (dom-by-class html "comic-pages") 'a))))
       (make-directory tsp/wizard-zines-comics-dir t)
       (cl-loop for i from 1
                for url in image-urls
                with total = (length image-urls)
                do (let* ((image-name (car (last (split-string url "/"))))
                          (dest-file-name (expand-file-name
                                           image-name
                                           tsp/wizard-zines-comics-dir)))
                     (message "[%d/%d] Saving comic %s..."
                              i total dest-file-name)
                     (url-copy-file url dest-file-name)))
       (message "Successfully saved comics.")))))

(provide 'init-eshell)
;;; init-eshell.el ends here
