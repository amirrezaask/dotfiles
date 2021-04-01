;;; json.el --- JSON module                          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
;; Keywords: 

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

;; 

;;; Code:

(pkg! json-mode :straight t :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(pkg! json-snatcher :straight t :hook json-mode :bind
      (:map json-mode ("C-c C-p". jsons-print-path)))

(provide 'modules/langs/json)
;;; json.el ends here
