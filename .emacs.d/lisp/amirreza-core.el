;;; amirreza-core.el --- Package manager + core macros + helpers  -*- lexical-binding: t; -*-

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


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(defmacro elpa-package
    (package &rest body)
  "Check if PACKAGE is installed and evaluate BODY."
  `(progn
     (straight-use-package (quote,package))
     (progn ,@body)
     ))

(defmacro emacs-package
    (package &rest body)
  `(progn
     ,@body))



(defmacro if-evil 
    (&rest body)
  `(if (boundp 'amirreza/darkside)
       (progn
	 ,@body)))

(defmacro if-not-evil 
    (&rest body)
  `(unless (boundp 'amirreza/darkside)
       (progn
	 ,@body)))


(provide 'amirreza-core)
;;; amirreza-core.el ends here
