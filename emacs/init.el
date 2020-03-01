;;; init --- initialize my emacs bundle
;;; Commentary:
;; Author Amirrezaask <raskarpour@gmail.com>
;; Always remember effient over fancy
;;; Code:
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/elisp"))
;; (setq debug-on-error 1)
(setq start (float-time)) ;; to measure emacs startup time
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

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
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
;; Themes end here
(defun light-it-up ()
  "Light the IDE up."
  (interactive)
  (load-theme 'spacemacs-light t))

(load-theme 'dracula t)
(use-package emojify :ensure t :config (emojify-mode 1))
(use-package doom-modeline :ensure t :config (doom-modeline-mode 1))
(use-package ido-vertical-mode :ensure t :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (ido-vertical-mode t))

(use-package swiper :ensure t :defer t :init (global-set-key (kbd "C-s") 'swiper) :commands swiper)
(use-package counsel :ensure t :defer t :init (global-set-key (kbd "M-x") 'counsel-M-x) :commands counsel-M-x)

(use-package org :ensure t :defer t)
(use-package org-bullets :ensure t :hook org-mode :config (lambda () (org-bullets-mode 1)))
(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")
(use-package json-mode :ensure t :mode "\\.json\\'"
  :config
  (add-hook 'before-save-hook 'json-mode-beautify))


(use-package which-key :ensure t :config (which-key-mode 1))
(use-package markdown-mode :ensure t :mode "\\.md\\'")
;; (use-package dap-mode :ensure t :hook ((go-mode python-mode php-mode) . dap-mode))
(use-package flycheck :ensure t :hook ((python-mode go-mode php-mode emacs-lisp-mode) . flycheck-mode))
(use-package magit :ensure t :defer t)
(use-package forge :ensure t :defer t :after magit)
(use-package projectile :ensure t)
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

(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (lsp))

(use-package py-autopep8 :ensure t :defer t :hook python-mode)


(use-package paredit :ensure t :hook (emacs-lisp-mode . paredit-mode))
(use-package parinfer :ensure t :hook (emacs-lisp-mode . parinfer-mode))
(use-package rainbow-delimiters :ensure :hook ((emacs-lisp-mode python-mode go-mode php-mode) . rainbow-delimiters-mode))


(use-package php-mode :ensure t :defer :init (add-hook 'php-mode-hook #'lsp))

(use-package js2-mode :ensure t :defer t :hook js-mode)
(use-package tide :ensure t :defer t :mode "\\.ts\\'")




(use-package docker :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package ansible :ensure t :defer t :init (add-hook 'yaml-mode-hook (lambda () (ansible))))
(use-package kubernetes :ensure t :defer t)


(defun go-path () (concat (getenv "HOME") "/go"))

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
  (add-to-list 'exec-path (concat (go-path) "/bin")))

(use-package go-add-tags :ensure t :hook go-mode :defer t :config (global-set-key "C-c C-s" 'go-add-tags))
(use-package gotest :ensure t :defer t :hook go-mode :config (global-set-key (kbd "C-c C-t C-t") 'go-test-current-test) (global-set-key (kbd "C-c C-t C-f") 'go-test-current-file))

(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x '") 'split-window-horizontally)

(message "Startup Time %f" (- (float-time) start))

(provide 'init)
;;; init ends here.
