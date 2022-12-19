;;; amirreza-minibuffer.el --- Minibuffer stuff      -*- lexical-binding: t; -*-

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

(elpa-package vertico
	      (setq completion-cycle-threshold 3)
	      (setq tab-always-indent 'complete)
	      (vertico-mode)
	      (setq vertico-count 15)
	      (setq vertico-cycle t))


(elpa-package savehist
	      (savehist-mode))

(elpa-package consult
	      (setq consult-async-min-input 1)
	      (define-key global-map (kbd "C-c g") 'consult-ripgrep))

(elpa-package marginalia
	      (marginalia-mode))

(elpa-package orderless
	      (setq completion-styles '(orderless basic)
		    completion-category-defaults nil
		    completion-category-overrides '((file (styles partial-completion)))))


(provide 'amirreza-minibuffer)
;;; amirreza-minibuffer.el ends here
