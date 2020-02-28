;; Author Amirrezaask <raskarpour@gmail.com>
;; Always remember effient over fancy

;; (setq debug-on-error 1)
(setq start (float-time)) ;; to measure emacs startup time
;; initial setup
(require 'package)
(setq custom-file "~/.__custom.el")
(setq inhibit-splash-screen 0)
(setq make-backup-files 0)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-linum-mode t)
(package-initialize)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package) 
;; (setq use-package-verbose t)
;; (setq use-package-minimum-reported-time 0.000001)
;; initial setup done ;)


;; UI stuff

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(global-prettify-symbols-mode 1)


;; Font Setup
(setq font "Jetbrains Mono")
(setq font-size 11)
(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" font font-size)))
(set-face-attribute 'default nil
		    :family font
		    :height (* 10 font-size))
;; Font ends here
;; Themes
(use-package dracula-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
;; Themes end here
(defun light-it-up ()
  (interactive)
  (load-theme 'solarized-light t))

(load-theme 'solarized-dark t)
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-banner-logo-title "Welcome To GNU Emacs")
;;   (setq dashboard-items '(
;; 			  (projects . 5)
;; 			  (bookmarks . 5)
;; 			  (agenda . 5)
;; 			  (registers . 5))))
(use-package emojify :ensure t :config (emojify-mode 1))
;; (use-package spaceline :ensure t :config (spaceline-spacemacs-theme))
;; (use-package doom-modeline :ensure t :config (doom-modeline-mode 1))
;; UI stuff ends here


;; Completion framework
(use-package ido-vertical-mode :ensure t :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (ido-vertical-mode t))
(use-package swiper :ensure t :defer t :commands swiper)
(use-package counsel :ensure t :commands counsel-M-x)
;; Completion framework ends here

;; Org setup
(use-package org :ensure t :defer t)
(use-package org-bullets :ensure t :hook org-mode :config (lambda () (org-bullets-mode 1)))
;; Org ends here

;; editor
(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")
(use-package json-mode :ensure t :mode "\\.json\\'"
  :config
  (add-hook 'before-save-hook 'json-mode-beautify))
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'swiper)
  :config
  (evil-mode t))

(use-package which-key :ensure t :config (which-key-mode 1))
(use-package markdown-mode :ensure t :mode "\\.md\\'")
;; editor ends here

;; IDE
;; (use-package dap-mode :ensure t :hook ((go-mode python-mode php-mode) . dap-mode))
(use-package flycheck :ensure t :hook ((python-mode go-mode php-mode emacs-lisp-mode) . flycheck-mode))
;; (use-package neotree :ensure t)
(use-package magit :ensure t :defer t)
(use-package forge :ensure t :defer t :after magit)
;; (use-package projectile :ensure t)
(use-package lsp-mode :ensure t :defer t)
(use-package lsp-ui :ensure t :defer t)
(use-package company-lsp :ensure t :defer t)
(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 30)
  (setq company-idle-delay .1)
  (setq company-echo-delay 0)
  (global-company-mode))

;; IDE end
;; python setup
(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (lsp))
;; (use-package elpy :ensure t :init (elpy-enable)) ;; a little too much i think

(use-package py-autopep8 :ensure t :defer t :hook python-mode)
;; python setup ends here


;; Lisp setup
(use-package paredit :ensure t :hook (emacs-lisp-mode . paredit-mode))
(use-package parinfer :ensure t :hook (emacs-lisp-mode . parinfer-mode))
(use-package rainbow-delimiters :ensure :hook ((emacs-lisp-mode python-mode go-mode php-mode) . rainbow-delimiters-mode))
;; Lisp ends here


;; not important :) languages 
(use-package php-mode :ensure t :defer :init (add-hook 'php-mode-hook #'lsp))
(use-package js2-mode :ensure t :hook js-mode)
;; not important languages :) ends here




;; Devops
(use-package docker :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package ansible :ensure t :defer t :init (add-hook 'yaml-mode-hook (lambda () (ansible))))
(use-package kubernetes :ensure t :defer t)
;; Devops ends here


;; Golang Setup
(defun go-path () (concat (getenv "HOME") "/go"))

(use-package go-mode
  :mode "\\.go\\'"
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda ()
			    (lsp)
			    (add-hook 'before-save-hook #'lsp-format-buffer t t)
			    (add-hook 'before-save-hook #'lsp-organize-imports t t)
			    (add-hook 'go-mode-hook 'go-eldoc-setup)
			    (local-set-key (kbd "M-.") #'godef-jump)
			    (local-set-key (kbd "M-*") 'pop-tag-mark)
			    ))
  :config
  (add-to-list 'exec-path (concat (go-path) "/bin")))
(use-package go-add-tags :ensure t)
(use-package go-stacktracer :ensure t)
(use-package go-eldoc :ensure t)
(use-package gotest :ensure t) 
;; Golang Setup ends here


;; keybindings
(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ".." 'xref-find-definitions
   "/" 'undo-tree-undo
   "xx" 'counsel-M-x
   "SPC" 'find-file
   "ff" 'find-file
   ;; line actions 
   "ls" #'move-beginning-of-line
   "le" #'move-end-of-line
   "lc" 'comment-line
   ;; line actions ends here
   ;; buffer actions
   "bl" 'switch-to-buffer
   "bs" 'save-buffer
   "bk" 'kill-buffer
   "bn" #'buffer-next
   "bp" #'buffer-previous
   "be" 'eval-buffer
   ;; buffer actions ends here
   ;; window actions
   "wk" 'evil-window-top
   "wj" 'evil-window-down
   "wh" 'evil-window-left
   "wl" 'evil-window-right
   "wo" 'other-window
   "wc" 'delete-window
   "sh" 'split-window-horizontally
   "sv" 'split-window-vertically
   ;; windows actions ends here
   ;; Project actions
   "pf" 'counsel-git
   ;; Project ends here
   "ss" 'swiper
   "ee" 'eval-last-sexp
   "dk" 'describe-key
   "df" 'describe-function))
;; keybindings ends here

;; IRC setup
;; (erc-autojoin-mode)
;; (defun irc ()
;;   (interactive)
;;   (erc :server "irc.freenode.net" :nick "amirrezaask" :password nil))
;; IRC ends here


;; Twitter setup
;; (use-package twittering-mode :ensure t :defer t :config (setq twittering-icon-mode t) :commands twittering-mode)
;; Twitter ends here


;; init.el ends here
(message "Startup Time %f" (- (float-time) start))
(find-file "~/.TODO.org")
