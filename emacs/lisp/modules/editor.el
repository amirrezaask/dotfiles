;;; editor.el --- Editor module                      -*- lexical-binding: t; -*-

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
(setq custom-file "~/.emacs.d/custom.el")
(setq scroll-step 5)
(setq scroll-margin 5)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.11)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(5
                                  ((shift) . 10)))
(setq mouse-wheel-progressive-speed t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
(setq-default fill-column 80) ;; column number which emacs start to line wrap.
(set-terminal-coding-system 'utf-8) ;; default emacs encodings
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen 0)
(setq use-dialog-box nil)
(setq echo-keystrokes 0.1)
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)
(define-key global-map (kbd "C--") (lambda () (interactive) (text-scale-adjust -1)))
(define-key global-map (kbd "C-=") (lambda () (interactive) (text-scale-adjust +1)))
(global-set-key (kbd "M-n") (lambda ()
                              (interactive)
                              (forward-line 10)))
(global-set-key (kbd "M-p") (lambda ()
                              (interactive)
                              (forward-line -10)))
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))
(defalias 'yes-or-no-p 'y-or-n-p)
(pkg! delsel ;; delete region when start typing
    :hook (after-init . delete-selection-mode))
(column-number-mode +1)
(setq kill-ring-max 15)
(pkg! battery :config (display-battery-mode 1))
(pkg! time :config (display-time-mode 1))
(global-display-line-numbers-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(display-battery-mode 1)
(pkg! which-key
      :straight t
      :defer 1
      :init
      (setq which-key-sort-order #'which-key-prefix-then-key-order
            which-key-sort-uppercase-first nil
            which-key-add-column-padding 1
            which-key-max-display-columns nil
            which-key-min-display-lines 6
            which-key-side-window-slot -10)
      :config
      (setq which-key-idle-delay 0.3)
      (defalias 'which-key! 'which-key-add-key-based-replacements)
      (which-key-mode 1)
      (which-key-setup-minibuffer))

(pkg! highlight-indent-guides
      :straight t
      :hook ((yaml-mode) . highlight-indent-guides-mode)
      :init
      (setq highlight-indent-guides-method 'character)
      :config
      (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces))

(pkg! sudo-edit
        :straight t
        :commands (sudo-edit))

(pkg! expand-region
      :straight t
      :bind (("C-=" . 'er/expand-region))
      ("C--" . 'er/contract-region))

(pkg! hl-todo
      :straight t
      :hook ((prog-mode) . hl-todo-mode)
      :config
      (setq hl-todo-highlight-punctuation ":")
      hl-todo-keyword-faces
      `(("TODO"       warning bold)
        ("FIXME"      error bold)
        ("HACK"       font-lock-constant-face bold)
        ("REVIEW"     font-lock-keyword-face bold)
        ("NOTE"       success bold)
        ("DEPRECATED" font-lock-doc-face bold)))
(pkg! multiple-cursors
      :straight t
      :commands (mc/edit-lines
      mc/mark-all-like-this
      mc/mark-next-like-this
      mc/skip-to-next-like-this
      mc/unmark-next-like-this
      mc/mark-previous-like-this
      mc/skip-to-previous-like-this
      mc/unmark-previous-like-this
      mc/mark-all-in-region-regexp
      mc/insert-numbers
      mc/insert-letters)
      :bind (("C-M-n" .  mc/mark-next-like-this)
             ("C-M-p" . mc/mark-previous-like-this)
             ("C-M-a" . mc/mark-all-like-this)))


(pkg! so-long 
      :config (global-so-long-mode 1))

(pkg! vlf :straight t :commands (vlf))

(pkg! tramp
      :commands (tramp)
      :config
      (setq tramp-default-method "ssh"))
(pkg! rainbow-delimiters :straight t :hook (prog-mode . rainbow-delimiters-mode))
(provide 'modules/editor)
;;; editor.el ends here
