;; Basic
(setq gc-cons-threshold 100000000) ;; 100 MB
(setq vc-follow-symlinks t)
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
;; Basic END

;; Package manager START
(setq package-enable-at-startup nil)
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
;; Package manager END

;; MacOS
(setq use-short-answers t)
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again
;; MacOS END

;; FONT START
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))

(setq font-family "Jetbrains Mono %s")
(set-face-attribute 'default nil :font (format font-family 12))
(set-frame-font (format font-family 12) nil t)

(defun amirreza/default ()
  (interactive)
  (set-face-attribute 'default nil :font (format font-family 12))
  (set-frame-font (format font-family 12) nil t))

(defun amirreza/benq ()
  (interactive)
  (set-face-attribute 'default nil :font (format font-family 19))
  (set-frame-font (format font-family 19) nil t))
;; FONT END

;; PATH
(defun home (path)
  (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(add-to-list 'exec-path (home ".opam/5.0.0/bin")) ;; ocaml my caml
(add-to-list 'exec-path (home ".opam/default/bin"))
(setenv "PATH" (string-join exec-path ":")) ;; set emacs process PATH
;; PATH END

;; Navigation
(setq recenter-positions '(middle))
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)
;; Navigation END

;; Modeline
(setq-default mode-line-format '("%e" mode-line-front-space
				 mode-line-modified
				 " "
				 default-directory "%b"
				 mode-line-end-spaces
				 ))
;; Modeline END

;; Frame
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default frame-title-format '("%e" default-directory))
;; Frame END

;; GUI
(global-hl-line-mode)
(global-display-line-numbers-mode)
(setq-default cursor-type 'box)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; GUI END

;; Themes
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package ef-themes)
(use-package amirreza-themes :straight (amirreza-themes :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(setq custom-safe-themes t)
(global-set-key (kbd "<f1>") 'ef-themes-load-random)
(load-theme 'naysayer)
;; Themes END

;; minibuffer
(use-package vertico
  :init
  (setq vertico-cycle t)
  (setq vertico-count 25)
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
;; minibuffer END

;; Autocomplete
(global-unset-key (kbd "C-SPC"))
(use-package corfu
  :bind
  ("C-SPC" . 'completion-at-point)
  :config
  (setq corfu-auto t)
  (global-corfu-mode))
;; Autocomplete END

;; text editing
(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
(use-package multiple-cursors
  :bind
  (("C-S-n" . 'mc/mark-next-like-this)
   ("C-S-p" . 'mc/mark-previous-like-this)))
;; text editing END

;; languages
(use-package go-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29)
  (use-package csharp-mode))
(use-package typescript-mode)
(use-package lua-mode)
(use-package tuareg) ;; ocaml
;; languages END

;; sidebar
(use-package dired-sidebar
  :bind ("C-1" . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar))
;; sidebar END

;; Compile
(use-package compile
  :bind
  (("<f5>" . compile)
   :map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))
;; Compile END

;; Magit
(use-package magit)
;; Magit END

;; formatter
(use-package format-all)
;; formatter END

(global-set-key (kbd "C-x n") 'find-file-other-frame)

;; indent guides
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character))
;; indent guides END

;; Eglot 
(unless (>= emacs-major-version 29)
  (straight-use-package 'eglot))

(defun eglot-save-with-imports () (interactive)
       (eglot-format-buffer)
       (eglot-code-actions nil nil "source.organizeImports" t))

(add-hook 'go-mode-hook (lambda ()
			  (add-hook 'before-save-hook 'eglot-save-with-imports nil t)))

(use-package eglot :straight nil
  :hook
  ((go-mode rust-mode tuareg-mode) . eglot-ensure) ;; Go + Rust + Ocaml
  :bind
  (:map eglot-mode-map
	("C-x C-l" . eglot-save-with-imports)
	("M-i" . eglot-find-implementations)
	("C-c C-c" . eglot-code-actions)))
;; Eglot END

;; XRef
(use-package xref :straight nil
  :bind
  (("M-." . xref-find-definitions)
   ("M-r" . xref-find-references)))
;; XRef END

;; Grep
(use-package wgrep)
(when (executable-find "rg")
  (grep-apply-setting 'grep-command "rg --vimgrep ")
  (grep-apply-setting 'grep-use-null-device nil))
(global-set-key (kbd "C-S-g") 'grep)
;; Grep END

;; Emacs daemon server
(unless (server-running-p)
  (server-start))
;; Emacs daemon server END

