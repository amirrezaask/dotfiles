;; (setq debug-on-error 1)

(require 'package)
(setq custom-file "~/.__custom.el")
(cua-mode t)
(setq make-backup-files 0)
(setq create-lockfiles nil)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1) 
(setq cua-keep-region-after-copy t)
(setq warning-minimum-level :emergency)


(package-initialize)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package) 
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.000001)


(defun go-path () (concat (getenv "HOME") "/go"))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode t)
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 120
		    :weight 'normal
		    :width 'normal)


(use-package dracula-theme :ensure t :defer t)

(use-package spacemacs-theme :ensure t :defer t)


(load-theme 'spacemacs-dark t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 2))

(use-package doom-modeline :ensure t :config (doom-modeline-mode 1))

(use-package ido-vertical-mode :ensure t :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (ido-vertical-mode t))



(use-package docker :ensure t)

(use-package docker-compose-mode :ensure t :hook yaml-mode)

(use-package ansible :ensure t :hook yaml-mode :config #'ansible)

(use-package kubernetes :ensure t)

(use-package gitlab-ci-mode :ensure t :hook yaml-mode)

(use-package yaml-mode :ensure t :config :mode "\\.ya?ml\\'")



(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 30)
  (setq company-idle-delay .1)
  (setq company-echo-delay 0)
  (global-company-mode))
(use-package flycheck :ensure t :hook ((python-mode go-mode php-mode) . flycheck-mode))
(use-package yasnippet  :ensure t)
(use-package yasnippet-snippets :ensure t)

(use-package lsp-mode :ensure t)
  
(use-package lsp-ui :ensure t )
(use-package company-lsp :ensure t)

(use-package magit :ensure t)

(use-package evil :ensure t :config (evil-mode t))

(use-package which-key :ensure t :config (which-key-mode 1))

(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "bl" 'switch-to-buffer
   "ff" 'find-file
   "sv" 'split-window-vertically
   "sh" 'split-window-horizontally))   
(use-package paredit :ensure t :hook (emacs-lisp-mode . paredit-mode))

(use-package parinfer :ensure t :hook (emacs-lisp-mode . parinfer-mode))
(use-package rainbow-delimiters :ensure :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(defun my-go-hook ()
   (lsp)
  (flymake-mode-on)
  (yas-minor-mode-on)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (local-set-key (kbd "M-.") #'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(use-package go-mode
  :mode "\\.go\\'"
  :ensure t
  :init
  (add-hook 'go-mode-hook 'my-go-hook)
  :config
  (add-to-list 'exec-path (concat (go-path) "/bin"))
  )

(use-package go-add-tags :ensure t)
(use-package go-stacktracer :ensure t)
(use-package go-eldoc :ensure t)
(use-package gotest :ensure t) 


(use-package json-mode :ensure t :mode "\\.json\\'"
  :config
  (message "json mode loaded")
  (add-hook 'before-save-hook 'json-mode-beautify))

(use-package python-mode
  :ensure t
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (lsp))

(use-package elpy :ensure t :init (elpy-enable))
(use-package py-autopep8 :ensure t :hook python-mode)

(use-package markdown-mode :ensure t :mode "\\.md\\'")
(use-package js2-mode :ensure t)
(use-package php-mode :ensure t :config (add-hook 'php-mode-hook #'lsp))




