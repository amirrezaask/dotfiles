;;; org.el --- Org module                            -*- lexical-binding: t; -*-

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
(pkg! org
      :init
      (if-enabled? evil
                   (evil-define-key 'normal org-mode-map "SPC m n" 'amirreza/--org-insert-no-tangle)
                   (evil-define-key 'normal org-mode-map "SPC m b" 'amirreza/--org-insert-elisp-code-block))
      
      :config
      (defun amirreza/--org-insert-elisp-code-block ()
        (interactive)
        (insert (format "#+begin_src emacs-lisp\n\n#+end_src"))
        (previous-line)
        (beginning-of-line))

      (defun amirreza/--org-insert-no-tangle ()
        ""
        (interactive)
        (insert (format ":PROPERTIES:\n:header-args: :tangle no\n:END:\n"))
        (previous-line)
        (beginning-of-line))

      (setq org-ellipsis "⤵")
      (setq org-src-fontify-natively t)
      (setq org-src-tab-acts-natively t)
      (setq org-support-shift-select t)
      (setq org-src-window-setup 'current-window)
      (setq org-startup-folded t)
      :bind (:map org-mode-map
                  ("C-c m n" . amirreza/--org-insert-no-tangle)
                  ("C-c m b" . amirreza/--org-insert-elisp-code-block)))

(pkg! org-bullets
      :disabled t
      :straight t
      :hook (org-mode . (lambda () (org-bullets-mode 1))))


(provide 'modules/org)
;;; org.el ends here
