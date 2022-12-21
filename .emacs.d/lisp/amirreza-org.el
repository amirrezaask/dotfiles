;;; amirreza-org.el --- Org mode                     -*- lexical-binding: t; -*-

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

(emacs-package org
	       (defun amirreza/org-code-block ()
		 (interactive)
		 (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC"
				 (completing-read "Language: "
						  '("emacs-lisp"
						    "go"
						    "rust"
						    "python"
						    "lua"
						    "bash"
						    "sh"
						    "fish"
						    "java"
						    )))))
	       (defun amirreza/org-hook ()
		 (interactive)
		 (c-c-leader :map org-mode-map "c b" 'amirreza/org-code-block)
		 )
		 
	       (add-hook 'org-mode-hook #'amirreza/org-hook)
	       (setq org-src-window-setup 'current-window))

;; (elpa-package org-bullets
;; 	       (add-hook 'org-mode-hook #'org-bullets-mode))

(provide 'amirreza-org)
;;; amirreza-org.el ends here
