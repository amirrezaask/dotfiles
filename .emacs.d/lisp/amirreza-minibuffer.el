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

(use-package vertico
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))


(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind
  (("C-c g" . consult-ripgrep)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))


(provide 'amirreza-minibuffer)
;;; amirreza-minibuffer.el ends here
