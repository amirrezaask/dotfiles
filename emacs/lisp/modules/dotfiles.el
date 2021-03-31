;;; dotfiles.el --- dotfiles module                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
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

(defvar amirreza/dotfiles-location (exec-path-from-shell-copy-env "DOTFILES") "Location of my dotfiles.")

(defun amirreza/edit-dot-config ()
  (interactive)
  (find-file (completing-read "Edit: " (directory-files-recursively amirreza/dotfiles-location ".*" nil (lambda (name)
                                                                                                          (not (string-match "\\.git" name)
                                                                                                               t))))))
(global-set-key (kbd "<f9>") 'amirreza/edit-dot-config)

(global-set-key (kbd "C-c e c") 'amirreza/edit-dot-config)

(provide 'modules/dotfiles)
;;; dotfiles.el ends here
