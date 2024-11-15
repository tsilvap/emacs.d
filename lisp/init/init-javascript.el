;;; init-javascript.el --- Support for JavaScript and TypeScript programming languages  -*- lexical-binding: t; -*-

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

;;; TODO: Rewrite this using use-package, and using Doom Emacs's
;;; config for TypeScript/React/etc. as a reference.

;; (setup typescript-ts-mode
;;   (:file-match "\\.ts\\'")
;;   (:with-mode tsx-ts-mode
;;     (:file-match "\\.tsx\\'"))
;;   (:with-mode (typescript-ts-mode tsx-ts-mode)
;;     (:hook apheleia-mode eglot-ensure)))

;; (setup flymake-eslint
;;   ;; https://github.com/orzechowskid/flymake-eslint/issues/23#issuecomment-1675481378
;;   (:with-mode (eglot-managed-mode)
;;     (:hook (lambda ()
;;              (when (derived-mode-p 'typescript-ts-mode 'web-mode 'js-mode)
;;                (flymake-eslint-enable)))))

;;   (:option flymake-eslint-defer-binary-check t
;;            flymake-eslint-prefer-json-diagnostics t))

(provide 'init-javascript)
;;; init-javascript.el ends here
