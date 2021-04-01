;;; projectile.el --- projectile module              -*- lexical-binding: t; -*-

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


(pkg! projectile
      :straight t
      :commands (projectile-find-file projectile-project-root)
      :init
      (if-enabled? evil
        (define-key evil-normal-state-map (kbd "SPC f p") 'amirreza/find-project)
        (define-key evil-normal-state-map (kbd "SPC SPC") 'amirreza/context-find-file)
        (define-key evil-normal-state-map (kbd "SPC f s") 'amirreza/find-symbol-at-point)
        )
      :bind
      (("C-c p" . amirreza/find-project)
       ("C-c f" . amirreza/context-find-file)
       ("C-c g" . projectile-grep)
       ("C-M-s" . amirreza/find-symbol-at-point)
       ("<f1>" . amirreza/find-file-at-point)
       ("<f2>" . amirreza/find-symbol-at-point)
       ("C-M-f" . amirreza/find-file-at-point)
       ("C-M-g" . amirreza/find-symbol-at-point))

      :config
      (defun amirreza/find-project ()
        "List of projects in pre defined project locations."
        (interactive)
        (dired (completing-read "Project: "
                                (directory-files-recursively "~/src"
                                                             ".*"
                                                             t
                                                             (lambda (path) (not (projectile-project-p path)))
                                                             t))))

      (defun amirreza/recursive-search-path (initial path)
        (completing-read "Find File: " (directory-files-recursively path directory-files-no-dot-files-regexp nil (lambda (name)
                                                                                                                   (not (string-match "\\.git" name)))
                                                                    t) nil nil initial))

      (defun amirreza/find-symbol-at-point ()
        (interactive)
        (let* ((symbol (thing-at-point 'word)))
          (counsel-rg symbol (projectile-project-root))))

      (defun amirreza/context-find-file ()
        (interactive)
        (cond
         ((not (null (projectile-project-p))) (projectile-find-file))
         (t (counsel-find-file))
         )
        ))


(pkg! project :defer t)

(provide 'modules/projectile)
;;; projectile.el ends here
