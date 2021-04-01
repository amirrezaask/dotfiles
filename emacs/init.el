;;; init.el --- init file                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <amirreza@soviet>
;; Keywords: 

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

;; (setq debug-on-error t)

(setq modules! '(
                 ivy
		         buffers
                 ui
		         env
                 dashboard
		         dev
		         dired
		         dotfiles
		         editor
                 evil
		         git
		         org
		         pdf
		         projectile
		         search
		         term
		         windows
                 ;; Languages to configure
                 (langs
                  go
                  elisp
                  rust
                  python
                  ;; clojure
                  configs
                  md
                  )
                 ))

(boot!)

(provide 'init)
;;; init.el ends here
