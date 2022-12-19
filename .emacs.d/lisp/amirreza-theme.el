;;; amirreza-theme.el --- Themes                     -*- lexical-binding: t; -*-

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

(use-package ef-themes)
(use-package doom-themes)

(setq amirreza/--current-theme nil)

(defun amirreza/switch-theme ()
  (interactive)
  (let ((theme (intern (completing-read "Theme: " (mapcar #'symbol-name
							  (custom-available-themes))))))
    (amirreza/load-theme theme)))

(defun amirreza/load-theme (theme)
  (when (not (eq amirreza/--current-theme nil))
    (disable-theme amirreza/--current-theme))
  (setq amirreza/--current-theme theme)
  (load-theme amirreza/--current-theme t))

(amirreza/load-theme amirreza/theme)


(provide 'amirreza-theme)
;;; amirreza-theme.el ends here
