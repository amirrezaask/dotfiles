;;; git.el --- Git module                            -*- lexical-binding: t; -*-

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

(pkg! magit
      :straight t
      :commands (magit-status magit-get-current-branch)
      :init
      (if-enabled? evil
                   (define-key evil-normal-state-map (kbd "SPC g s") 'magit-status)
                   )
      :bind
      (("C-x g" . 'magit-status)
       ("C-c v s" . 'magit-status)))
     
    

(pkg! diff-hl
      :straight t
      :config (global-diff-hl-mode 1))

(pkg! gitconfig-mode
      :straight t
      :mode "/\\.gitconfig\\'")

(pkg! gitignore-mode
      :straight t
      :mode "/\\.gitignore\\'")

(pkg! gitattributes-mode
      :straight t
      :mode "/\\.gitattributes\\'")

(pkg! git-messenger
        :straight t
        :commands
        (git-messenger:popup-message)
        :bind
        (("C-c v b" . git-messenger:popup-message))

        :config
        (setq git-messenger:show-detail t)
        (setq git-messenger:use-magit-popup t))

(provide 'git)
;;; git.el ends here
