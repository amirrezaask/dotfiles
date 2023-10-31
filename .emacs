;; Basic
(setq gc-cons-threshold 100000000) ;; 100 MB
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
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

;; FONT START
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(add-to-list 'default-frame-alist '(font . "Jetbrains Mono 12"))
(defun amirreza/default () (interactive) (set-face-attribute 'default nil :font (format font-family 12)) (set-frame-font (format font-family 12) nil t))
(defun amirreza/benq () (interactive) (set-face-attribute 'default nil :font (format font-family 19)) (set-frame-font (format font-family 19) nil t))
;; FONT END

;; themes
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package ef-themes)
(use-package amirreza-themes :straight (amirreza-themes :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(setq custom-safe-themes t)
(load-theme 'naysayer)
;; themes END

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
(setq-default mode-line-format '("%e" mode-line-front-space mode-line-modified " %l:%c " default-directory "%b " mode-line-modes))
;; Modeline END

;; Frame
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized) ;; open emacs in maximized mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default frame-title-format '("%e" default-directory))
;; Frame END

;; GUI
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; GUI END

;; Autocomplete
(global-unset-key (kbd "C-SPC"))
(use-package corfu
  :bind
  ("C-SPC" . 'completion-at-point)
  :config
  (setq corfu-auto t)
  (global-corfu-mode))
;; Autocomplete END

;; languages
(use-package go-mode)
(use-package rust-mode)
(when (< emacs-major-version 29)
  (use-package csharp-mode))
;; languages END

;; Compile
(use-package compile
  :bind
  (("<f5>" . compile)
   :map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))
;; Compile END

(global-set-key (kbd "C-x n") 'find-file-other-frame)

;; Eglot 
(unless (>= emacs-major-version 29)
  (straight-use-package 'eglot))

(defun eglot-save-with-imports () (interactive)
       (eglot-format-buffer)
       (eglot-code-actions nil nil "source.organizeImports" t))

(add-hook 'go-mode-hook (lambda ()  (add-hook 'before-save-hook 'eglot-save-with-imports nil t)))

(use-package eglot :straight nil
  :hook
  ((go-mode rust-mode) . eglot-ensure) ;; Go + Rust
  :bind
  (:map eglot-mode-map
	("C-x C-l" . eglot-save-with-imports)
	("M-<f12>" . eglot-find-implementation)
	("C-c C-c" . eglot-code-actions)))
;; Eglot END

;; xref
(use-package xref :straight nil
  :bind
  (("M-." . xref-find-definitions)
   ("<f12>" . xref-find-definitions)
   ("S-<f12>" . xref-find-references)
   ("M-r" . xref-find-references)))
;; xref END

;; Grep
(use-package wgrep)
(grep-apply-setting 'grep-command "grep --exclude-dir='.git' --color=auto -nH --null -r -e ")
(when (executable-find "rg")
  (grep-apply-setting 'grep-command "rg --vimgrep ")
  (grep-apply-setting 'grep-use-null-device nil))
(global-set-key (kbd "C-S-g") 'grep)
;; Grep END

;; Emacs daemon server
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
   (server-start))
;; Emacs daemon server END
