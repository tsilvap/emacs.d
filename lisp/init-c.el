;;; init-c.el --- Support for the C language and related languages  -*- lexical-binding: t; -*-

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

(setup c-ts-mode
  (:hook indent-tabs-mode)
  (:when-loaded
    (:option c-ts-mode-indent-offset 4
             c-ts-mode-indent-style 'k&r
             tab-width 4)))

(provide 'init-c)
;;; init-c.el ends here
