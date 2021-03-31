;;; ivy.el --- Ivy module                            -*- lexical-binding: t; -*-

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

(pkg! flx :straight t)
(pkg! ivy
      :straight t
      :demand
      :bind
      (:map ivy-switch-buffer-map
            ("C-k" . 'ivy-previous-line)
            :map ivy-minibuffer-map
            ("C-j" . 'ivy-next-line)
            ("C-k" . 'ivy-previous-line)
            ("RET" . 'ivy-alt-done))
      :config
      (setq ivy-height 15)
      ;; loopish cycling through list
      (setq ivy-wrap t)
      ;; don't show recents in minibuffer
      (setq ivy-use-virtual-buffers nil)
      ;; ...but if that ever changes, show their full path
      (setq ivy-virtual-abbreviate 'full)
      ;; ;; don't quit minibuffer on delete-error
      (setq ivy-on-del-error-function #'ignore)
      (setf (alist-get 't ivy-format-functions-alist)
            #'ivy-format-function-line)
      (setq ivy-initial-inputs-alist nil)
      (setq ivy-re-builders-alist
            '((t . ivy--regex-ignore-order)))
      (ivy-mode +1))

(pkg! counsel
      :straight t
      :bind
      (("M-x" . 'counsel-M-x)
       ("C-x C-f" . 'counsel-find-file)
       ("C-h b" . 'counsel-descbinds)
       ("C-h f" . 'counsel-describe-function)
       ("C-h v" . 'counsel-describe-variable)
       ("C-h a" . 'counsel-apropos)
       ("M-i" . 'counsel-imenu) ;; code semantics
       ("M-y" . 'counsel-yank-pop)))
(pkg! ivy-rich :straight t :after ivy :config (ivy-rich-mode 1))
(pkg! ivy-posframe :straight t
      :disabled t
      :config
      (setq ivy-posframe-parameters '((parent-frame nil)))
      (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
      (ivy-posframe-mode 1))

(pkg! all-the-icons-ivy :straight t :config (all-the-icons-ivy-setup))
            


(provide 'ivy)
;;; ivy.el ends here
