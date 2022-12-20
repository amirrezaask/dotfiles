;;; amirreza-modeline.el --- Modeline                -*- lexical-binding: t; -*-

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

;; (elpa-package mini-modeline
;;   (setq mini-modeline-right-padding (/ (frame-width) 5))
;;   (setq mini-modeline-echo-duration 0.8)
;;   (setq mini-modeline-face-attr '(:background "#000000"))
;;   (setq-default mini-modeline-l-format
;; 		'("%e"
;; 		  mode-line-front-space
;; 		  mode-line-mule-info
;; 		  mode-line-client
;; 		  mode-line-modified
;; 		  mode-line-remote
;; 		  mode-line-frame-identification
;; 		  mode-line-buffer-identification
;; 		  " "
;; 		  mode-line-position
;; 		  )
;; 		)

;;   (setq-default mini-modeline-r-format
;; 		'("%e"
;; 		  mode-line-modes
;; 		  ))
;;   (mini-modeline-mode t))

;; (elpa-package minions
;; 	      (setq minions-mode-line-lighter ";")
;; 	      (setq minions-prominent-modes
;; 		    (list 'defining-kbd-macro
;; 			  'flymake-mode))

;; 	      (minions-mode 1))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))


(provide 'amirreza-modeline)
;;; amirreza-modeline.el ends here
