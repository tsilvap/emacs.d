;;; init-web.el --- Support for web languages such as HTML, CSS, web templates  -*- lexical-binding: t; -*-

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

;;;; web-mode.el: major mode for web template files

;; Used to edit HTML with embed parts (CSS/JavaScript) or template
;; blocks (like Go's `html/template', Django templates, Ruby's ERB).
;;
;; If needed, you can customize `web-mode-engines-alist' per-project
;; in a .dir-locals.el file.
(setup web-mode
  (:file-match "\\.html?\\'"
               "\\.phtml\\'"
               "\\.tpl\\.php\\'"
               "\\.[agj]sp\\'"
               "\\.as[cp]x\\'"
               "\\.erb\\'"
               "\\.mustache\\'"
               "\\.djhtml\\'"
               "\\.j2\\'"
               "\\.tmpl\\'")
  (:hook
   (lambda ()
     (electric-pair-local-mode -1))

   ;; HACK: needed to make setting web-mode engine in .dir-locals.el
   ;; work correctly.
   ;;
   ;; Based on:
   ;;   https://emacs.stackexchange.com/a/59709
   ;;   https://www.emacswiki.org/emacs/LocalVariables#h5o-2
   (lambda ()
     (add-hook 'hack-local-variables-hook
               (lambda ()
                 (web-mode-guess-engine-and-content-type)
                 (web-mode-buffer-fontify))
               nil t)))
  (:when-loaded
    (:option web-mode-markup-indent-offset 2
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 2)))

(setup emmet-mode
  (:hook-into web-mode)
  (:when-loaded
    (:option emmet-move-cursor-between-quotes t
             emmet-self-closing-tag-style "")))

(provide 'init-web)
;;; init-web.el ends here
