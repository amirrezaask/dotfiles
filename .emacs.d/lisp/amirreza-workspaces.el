;;; amirreza-workspaces.el --- Workspaces            -*- lexical-binding: t; -*-

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


(elpa-package perspective
	      (setq persp-mode-prefix-key (kbd "C-c w"))
	      (persp-mode 1)
	      (define-key global-map (kbd "C-c w s") 'persp-switch)
	      (if-evil
	       (nmap-leader
		 "w s" 'persp-switch
		 )
	      )

(provide 'amirreza-workspaces)
;;; amirreza-workspaces.el ends here
