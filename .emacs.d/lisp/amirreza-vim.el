;;; amirreza-vim.el --- Evil Mode + Extra goodies    -*- lexical-binding: t; -*-

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
(if-evil
 
 
 (elpa-package evil
	       (setq evil-want-C-u-scroll t)
	       (setq evil-split-window-below t)
	       (setq evil-vsplit-window-right t)
	       (setq evil-want-integration t)
	       (setq evil-want-keybinding nil)
	       (evil-mode 1)
	       (evil-global-set-key 'normal (kbd "<C-d>") (lambda
							    ()
							    (interactive)
							    (evil-scroll-down)
							    (evil-scroll-line-to-center)
							    ))
	       (evil-global-set-key 'normal (kbd "<C-u>") (lambda
							    ()
							    (interactive)
							    (evil-scroll-up)
							    (evil-scroll-line-to-center))))


 (elpa-package evil-escape
	       (setq-default evil-escape-key-sequence "jk")
	       (setq evil-escape-unordered-key-sequence t)
	       (evil-escape-mode))

 (elpa-package general
	       (general-create-definer nmap-leader :prefix "SPC" :keymaps 'normal)
	       (general-create-definer nmap :keymaps 'normal)
	       (general-create-definer vmap :keymaps 'visual)
	       (general-create-definer imap :keymaps 'insert))


 (elpa-package evil-collection
	       (evil-collection-init))

 (elpa-package evil-nerd-commenter
	       (nmap "gc" 'evilnc-comment-or-uncomment-lines)
	       (vmap "gc" 'evilnc-comment-or-uncomment-lines))

 (elpa-package evil-surround
	       (global-evil-surround-mode 1))

 )
(provide 'amirreza-vim)
;;; amirreza-vim.el ends here
