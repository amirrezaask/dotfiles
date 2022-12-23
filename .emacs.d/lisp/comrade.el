;;; comrade.el --- My simple framework to be used for Emacs configuration  -*- lexical-binding: t; -*-

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

;;;###autoload
(defmacro elpa-package
    (package &rest body)
  "Check if PACKAGE is installed and evaluate BODY."
  `(progn
     (straight-use-package (quote ,package))
     (progn ,@body)
     ))

;;;###autoload
(defmacro emacs-package
    (package &rest body)
  "Nothing fancy, just evaluate BODY, mostly for organization"
  `(progn
     ,@body))

;;;###autoload
(defmacro if-evil
    (&rest body)
  "If user has enabled evil mode, evaluate BODY"
  `(if (boundp 'amirreza/darkside)
       (progn
	 ,@body)))

;;;###autoload
(defmacro if-not-evil
    (&rest body)
  "If user has not enabled evil mode, evaluate BODY"
  `(unless (boundp 'amirreza/darkside)
     (progn
       ,@body)))


(provide 'comrade)
;;; comrade.el ends here
