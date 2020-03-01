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

(load-theme 'dracula t)
(use-package emojify :ensure t :config (emojify-mode 1))
;; (require 'powerline)
;; (powerline-evil-vim-color-theme)
;; (use-package spaceline :ensure t :config (spaceline-spacemacs-theme))
;; (use-package doom-modeline :ensure t :config (doom-modeline-mode 1))
;; UI stuff ends here

(global-set-key (kbd "C-c /") 'comment-line)

;; Completion framework
(use-package helm :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x -") 'split-window-vertically)
  (global-set-key (kbd "C-x '") 'split-window-horizontally)
  )
;; ;; Completion framework ends here

;; Org setup
(use-package org :ensure t :defer t)
(use-package org-bullets :ensure t :hook org-mode :config (lambda () (org-bullets-mode 1)))
;; Org ends here

;; editor
(use-package yaml-mode :ensure t :mode "\\.ya?ml\\'")
(use-package json-mode :ensure t :mode "\\.json\\'"
  :config
  (add-hook 'before-save-hook 'json-mode-beautify))


(use-package which-key :ensure t :config (which-key-mode 1))
(use-package markdown-mode :ensure t :mode "\\.md\\'")
;; editor ends here

;; IDE
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

;; IDE end
;; python setup
(use-package python-mode
  :ensure t
  :defer t
  :mode "\\.py\\'"
  :config
  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
  (lsp))

(use-package py-autopep8 :ensure t :defer t :hook python-mode)
;; python setup ends here


;; Lisp setup
(use-package paredit :ensure t :hook (emacs-lisp-mode . paredit-mode))
(use-package parinfer :ensure t :hook (emacs-lisp-mode . parinfer-mode))
(use-package rainbow-delimiters :ensure :hook ((emacs-lisp-mode python-mode go-mode php-mode) . rainbow-delimiters-mode))
;; Lisp ends here


;; not important :) languages 
(use-package php-mode :ensure t :defer :init (add-hook 'php-mode-hook #'lsp))
;; not important :) languages

;; Javascript/Typescript
(use-package js2-mode :ensure t :defer t :hook js-mode)
(use-package tide :ensure t :defer t :mode "\\.ts\\'")
;; Javscript/Typescript ends here




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
			    (add-hook 'go-mode-hook 'go-eldoc-setup)))
  :config
  (add-to-list 'exec-path (concat (go-path) "/bin")))

(use-package go-add-tags :ensure t :hook go-mode :defer t :config (global-define-key "C-c s" 'go-add-tags))
(use-package gotest :ensure t :defer t :hook go-mode :config (global-define-key "C-c t" 'go-test-current-test) ("C-c C-t" 'go-test-current-file)) 
;; Golang Setup ends here

;; init.el ends here
(message "Startup Time %f" (- (float-time) start))
(find-file "~/.TODO.org")
