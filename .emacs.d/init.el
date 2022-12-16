(setq package-enable-at-startup nil)

(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar

(setq amirreza/font "FiraCode Nerd Font Mono")
(setq amirreza/font "JetBrainsMono Nerd Font Mono")

(setq amirreza/font-size "21")

(setq amirreza/dark-theme 'gruber-darker)
(setq amirreza/light-theme 'ef-day)


;; Setup package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)



(use-package emacs
  :config
  (setq backup-by-copying t) ;; Always copy files for backup.
  (setq version-control t) ;; Use version numbers for backup.
  (setq delete-old-versions t) ;; Delete old backup of files.
  (setq kept-new-versions 6) ;; Number of newest versions to keep.
  (setq kept-old-versions 2) ;; Number of old versions to keep.
  (setq create-lockfiles nil) ;; Don't create .# files as lock.
  (setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
	'(("." . "~/.emacs.d/backup")))
  (delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
  (show-paren-mode 1) ;; Highlight matching parens
  (setq show-paren-delay 0) ;; highlight matching parens instantly.
  (global-display-line-numbers-mode 1)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
)

;; Orderless completion matching algorithm
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Buffer management UI
(use-package bufler
  :bind
  ("C-x b" . bufler))

;; Better window management facilities
(use-package ace-window
  :bind
  ("C-x o" . ace-window))

;; My font setup for my home monitor
(defun amirreza/home-monitor ()
  (interactive)
  (setq amirreza/font-size "23")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; My font setup for my laptop setup
(defun amirreza/laptop ()
  (interactive)
  (setq amirreza/font-size "19")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Themes
(use-package ef-themes)
(use-package doom-themes)
(use-package gruber-darker-theme)

;; Toggle between light and dark mode
(setq amirreza/--color-mode 'dark)

(defun amirreza/load-theme ()
  (if (eq amirreza/--color-mode 'dark)
      (load-theme amirreza/dark-theme t)
    (load-theme amirreza/light-theme t)))

(defun amirreza/toggle-color ()
  (interactive)
  (if (eq amirreza/--color-mode 'dark)
      (setq amirreza/--color-mode 'light)
    (setq amirreza/--color-mode 'dark)
    )
  (amirreza/load-theme))

(global-set-key (kbd "<f12>") 'amirreza/toggle-color)

;; Load theme
(amirreza/load-theme)

;; Reload font settings
(defun amirreza/reload-font ()
  (interactive)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(amirreza/reload-font)

;; Minibuffer completion
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind
  (
   ("C-s" . consult-line)
   ("C-c g" . consult-ripgrep)))

(use-package marginalia
  :init
  (marginalia-mode))

;; In buffer auto complete menu
(use-package company
  :init
  (setq company-backends '(company-capf))
  (global-company-mode))

;; Dired, Emacs file manager
(use-package dired
  :straight nil
  :hook (dired-mode . (lambda ()
                             (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
                             )))

;; best movement ever ?
(defun amirreza/up-center ()
  (interactive)
  (previous-line 20)
  (recenter-top-bottom))

(defun amirreza/down-center ()
  (interactive)
  (next-line 20)
  (recenter-top-bottom))

;; Best movement ever ?????
(setq recenter-positions '(middle))
(global-set-key (kbd "M-p") (lambda () (interactive) (amirreza/up-center)))
(global-set-key (kbd "M-n") (lambda () (interactive) (amirreza/down-center)))

(use-package magit
  :bind
  (("C-x g" . magit)))

(use-package apache-mode)

(use-package vterm)

(use-package systemd)

(use-package nginx-mode)

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path (expand-file-name "bin" user-emacs-directory))



(global-set-key (kbd "C-9") #'compile) ;; Run compile command and show it's output
(global-set-key (kbd "C-8") #'async-shell-command)

(when (file-executable-p "rg")
			 (grep-apply-setting
			  'grep-command
			  "rg --no-heading --vimgrep "))

(global-set-key (kbd "C-0") #'grep)

(global-set-key (kbd "C-1") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))

(add-hook 'grep-mode-hook (lambda ()
                            (define-key grep-mode-map (kbd "M-.") 'find-file-at-point)))

(use-package go-mode)
(use-package rust-mode)
(use-package zig-mode)

(use-package eglot
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer nil)
  (global-eldoc-mode)
  (defun amirreza/eglot-hook ()
    (eglot-ensure)
    (put 'eglot-note 'flymake-overlay-control nil)
    (put 'eglot-warning 'flymake-overlay-control nil)
    (put 'eglot-error 'flymake-overlay-control nil)

    (define-key eglot-mode-map (kbd "C-c d") 'eldoc)
    (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "M-r") 'xref-find-references)
    (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c c") 'eglot-code-actions))

  :hook
  (
   (go-mode
    rust-mode
    python-mode
    php-mode
    ). amirreza/eglot-hook))

(use-package org-present)

(use-package smartparens :hook prog-mode)

(use-package rainbow-delimiters :hook prog-mode)

(use-package org-bullets :hook org-mode)

(use-package git-gutter
  :init
  (global-git-gutter-mode))

(use-package prescient)

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))


(use-package company-prescient
  :init
  (company-prescient-mode))


(use-package helpful
  :bind
  (("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)))


