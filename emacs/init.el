;;; init --- initialize my emacs bundle
;;; Commentary:
;; Author Amirrezaask <raskarpour@gmail.com>
;; Always remember effient over fancy
;;; Code:
;; (add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/elisp"))
;; (setq debug-on-error 1)
(setq start (float-time)) ;; to measure emacs startup time

;; Basic Setup
(require 'package)
(setq custom-file "~/.__custom.el")
(setq inhibit-splash-screen 0) ;; turn off emacs annoying startup page.
(setq make-backup-files nil) ;; turn off emacs annoying ~ files
(setq create-lockfiles nil) ;; turn off emacs annoying # files
(setq ring-bell-function 'ignore) ;; turn off emacs beeping
(tool-bar-mode 0) ;; turn off emacs GUI toolbar
(scroll-bar-mode 0) ;; turn off emacs GUI scrollbar
(menu-bar-mode 0) ;; turn emacs GUI menubar
(defalias 'yes-or-no-p 'y-or-n-p) ;; instead of yes-or-no ask y-or-no, only for convinience
(global-linum-mode t) ;; enable line numbers
(package-initialize) ;; initialize emacs built-in package manager
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


(package-install 'use-package)
(require 'use-package) ;; use-package helps us declaretively define our packages and lazy load them only when we need them.
;; Basic Setup ends here

;; UI Enhancements
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ))) ;; shows lambda word as the symbol
(global-prettify-symbols-mode 1)


(defvar font-family "Jetbrains Mono" "Font to use for Emacs.")
(defvar font-size 11 "Font size for Emacs.")
(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" font-family font-size)))
(set-face-attribute 'default nil
		    :family font-family
		    :height (* 10 font-size))
(use-package dracula-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)

(defun light-it-up ()
  "Light the IDE up."
  (interactive)
  (load-theme 'spacemacs-light t))

(load-theme 'spacemacs-dark t)
(use-package emojify :ensure t :config (emojify-mode 1))
(use-package doom-modeline :ensure t :config (doom-modeline-mode 1))
;; UI Enhancements ends here

;; Enhance Search, find-file, M-x
(use-package ido-vertical-mode :ensure t :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (ido-vertical-mode t))

(use-package swiper :ensure t :defer t :init (global-set-key (kbd "C-s") 'swiper) :commands swiper)
(use-package counsel :ensure t :defer t :init (global-set-key (kbd "M-x") 'counsel-M-x) :commands counsel-M-x)
;; Enhance Search, find-file, M-x ends here

;; markup
(use-package org :ensure t :defer t)
(use-package org-bullets :ensure t :hook org-mode :config (lambda () (org-bullets-mode 1)))
(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")
(use-package json-mode :ensure t :mode "\\.json\\'"
  :config
  (add-hook 'before-save-hook 'json-mode-beautify))
(use-package markdown-mode :ensure t :mode "\\.md\\'")
;; markup ends here


;; keybindings
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x '") 'split-window-horizontally)
(use-package evil :ensure t :config (evil-mode t)) ;; Only for editing.
(use-package which-key :ensure t :config (which-key-mode 1))
;; keybindings ends here



;; IDE stuff (syntax checker, git wrapper, language server protocol, autocomplete framework)
;; (use-package dap-mode :ensure t :hook ((go-mode python-mode php-mode) . dap-mode))
(use-package flycheck :ensure t :hook ((python-mode go-mode php-mode emacs-lisp-mode) . flycheck-mode))
(use-package magit :ensure t :defer t)
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
;; IDE stuff ends here


;; Python
(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (lsp))

(use-package py-autopep8 :ensure t :defer t :hook python-mode)
;; Python ends here


;; Lisp
(use-package paredit :ensure t :hook (emacs-lisp-mode . paredit-mode))
(use-package parinfer :ensure t :hook (emacs-lisp-mode . parinfer-mode))
(use-package rainbow-delimiters :ensure :hook ((emacs-lisp-mode python-mode go-mode php-mode) . rainbow-delimiters-mode))
;; Lisp ends here

;; PHP
(use-package php-mode :ensure t :defer :init (add-hook 'php-mode-hook #'lsp))
;; PHP ends here

;; Javascript/Typescript
(use-package js2-mode :ensure t :defer t :hook js-mode)
(use-package tide :ensure t :defer t :mode "\\.ts\\'")
;; Javascript/Typescript ends here


;; Devops
(use-package multi-term :ensure t :defer t)
(use-package docker :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package ansible :ensure t :defer t :init (add-hook 'yaml-mode-hook (lambda () (ansible))))
(use-package kubernetes :ensure t :defer t)
;; Devops ends here


;; Go

(use-package go-mode
  :mode "\\.go\\'"
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda ()
			    (lsp)
			    (add-hook 'before-save-hook #'lsp-format-buffer t t)
			    (add-hook 'before-save-hook #'lsp-organize-imports t t)
			    (add-hook 'go-mode-hook 'go-eldoc-setup)))
  :config
  (add-to-list 'exec-path (concat (concat (getenv "HOME") "/go") "/bin")))

(use-package go-add-tags :ensure t :hook go-mode :defer t :config (global-set-key "C-c C-s" 'go-add-tags))
(use-package gotest :ensure t :defer t :hook go-mode :config (global-set-key (kbd "C-c C-t C-t") 'go-test-current-test) (global-set-key (kbd "C-c C-t C-f") 'go-test-current-file))
;; Go ends here


(message "Startup Time %f" (- (float-time) start))

(provide 'init)
;;; init ends here.
