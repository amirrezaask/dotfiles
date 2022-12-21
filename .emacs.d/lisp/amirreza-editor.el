;;; amirreza-editor.el --- Editor experience         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Amirreza Askarpour

;; Author: Amirreza Askarpour <amirreza@amirrezas-MacBook-Pro.local>
;; Keywords: lisp

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

;; (elpa-package smartparens
;; 	      (add-hook 'prog-mode-hook #'smartparens-mode))

(elpa-package rainbow-delimiters
	      (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(setq display-line-numbers-type 'relative) ;; relative line numbers

(global-display-line-numbers-mode 1) ;; enable line numbers globaly

(elpa-package expand-region
	      (define-key global-map (kbd "C-=") 'er/expand-region)
	      (define-key global-map (kbd "C--") 'er/contract-region))

(global-hl-line-mode)

(provide 'amirreza-editor)
;;; amirreza-editor.el ends here
