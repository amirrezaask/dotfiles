;;; jblow-theme.el --- Theme copied from jonathan blow emacs theme  -*- lexical-binding: t; -*-

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



(deftheme jblow "Theme from Great Jonathan Blow")

(let ((background "#072626")
      (selection "#0000ff")
      (keyword "#d4d4d4")
      (comment "#118a1a")
      (string "#2ec09c")
      (variable "#c8d4ec")
      (warning "#504038")
      (constant "#7ad0c6")
      (cursor "green")
      (mode-line "#d3b58d")
      (function "#ffffff")
      (macro "#8cde94")
      (punctuation "#8cde94")
      (hl-line "#084040")
      (builtin "#ffffff")

      )

  (custom-theme-set-faces
   'jblow
   
   `(default ((t (:foreground "#d3b58d" :background ,background))))
   `(cursor ((t (:background ,cursor))))

   `(font-lock-keyword-face           ((t (:foreground ,keyword))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constant))))
   `(font-lock-variable-name-face     ((t (:foreground ,variable))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,string))))
   `(font-lock-comment-face           ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-doc-face               ((t (:foreground ,comment))))
   `(font-lock-function-name-face     ((t (:foreground ,function))))
   `(font-lock-doc-string-face        ((t (:foreground ,string))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macro))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   `(mode-line ((t (:foreground "black" :background ,mode-line))))
   `(region ((t (:background ,selection))))
   `(hl-line ((t :background ,hl-line)))
   `(highlight ((t :foreground nil :background ,selection)))
   `(persp-selected-face ((t :foreground "#ffffff")))
   )
  )


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jblow)
;;; jblow-theme.el ends here
