;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 200000000) ;; 200 MB
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package Manager

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MacOS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-short-answers t)
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(setq font-families '("Jetbrains Mono" "Fira Code" "Liberation Mono"))

(defun set-font (font fontsize)
  (interactive (list (completing-read "Font Family: " font-families) (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(set-font "Fira Code" 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enviroment Variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun home (path)
  (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(setenv "PATH" (string-join exec-path ":")) ;; set emacs process PATH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile: Project Based Commands

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :commands (projectile-project-p)
  :bind
  (("C-x p p" . projectile-switch-project)
   ("C-x p a" . projectile-add-known-project)
   ("C-x p f" . projectile-find-file)
   ("C-x p g" . projectile-grep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Navigation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-dwim ()
  (interactive)
  (if (projectile-project-p) (projectile-find-file) (call-interactively 'find-file)))

(global-set-key (kbd "M-o") 'find-file-dwim)

(setq recenter-positions '(middle))
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Navigation And Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-;") 'previous-buffer)
(global-set-key (kbd "C-'") 'next-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Window Management

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<down>") 'shrink-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun amirreza/modeline-vc () (interactive) (propertize (if vc-mode vc-mode "")))
(defun amirreza/modeline-file () (interactive) (propertize (format "%s%s%s" (if (buffer-modified-p (current-buffer)) " [+] " "") default-directory (buffer-name (current-buffer)))))
(defun amirreza/modeline-linecol () (interactive) (propertize "%l:%c"))
(defun amirreza/modeline-major-mode () (interactive) (propertize (substring (capitalize (symbol-name major-mode)) 0 -5)))
(defun amirreza/modeline-left () (interactive) (concat (amirreza/modeline-vc)))
(defun amirreza/modeline-center () (interactive) (concat (amirreza/modeline-file)))
(defun amirreza/modeline-right () (interactive) (concat (amirreza/modeline-major-mode)))
(defun amirreza/modeline-format ()
  (let* ((left (amirreza/modeline-left))
	 (center (amirreza/modeline-center))
	 (right (amirreza/modeline-right))
	 (win-len (window-width (get-buffer-window (current-buffer))))
	 (center-right-spaces (make-string (- (/ win-len 2) (+ (/ (length center) 2) (length right))  ) ?\s))
	 (left-center-spaces (make-string (- (/ win-len 2) (+ (length left) (/ (length center) 2))) ?\s))
	 )

    (concat left left-center-spaces center center-right-spaces right)))

(setq-default mode-line-format '("%e" (:eval (amirreza/modeline-format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Frame Settings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-x\C-n" 'find-file-other-frame)
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default frame-title-format '("%e" (:eval default-directory)))
(global-set-key "\C-x\C-c" 'delete-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Graphical User Interface Settings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Themes & Colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package sweet-theme)
(use-package spacemacs-theme)
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)
(use-package amirreza-themes :straight (amirreza-themes :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(setq custom-safe-themes t)
(global-set-key (kbd "M-1") 'ef-themes-load-random)
(load-theme 'handmadehero)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minibuffer Enhancements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico :init (setq vertico-cycle t) (setq vertico-count 25) (vertico-mode))
(use-package consult)
(global-set-key "\C-xb" 'consult-buffer)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Autocomplete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :config
  (setq corfu-auto t)
  (global-corfu-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Text Editing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode)
(use-package multiple-cursors
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))

;; Keyboard Macro
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Golang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode)
(defun go-add-tags (FILE STRUCT)
  (interactive (list (read-file-name "Go File: " nil nil nil (buffer-name (current-buffer)) nil)
		     (read-string "Struct: " (word-at-point))))
  (unless (executable-find "gomodifytags") (error "Install gomodifytags first. https://github.com/fatih/gomodifytags"))
  (shell-command-to-string (format "gomodifytags -file %s -struct %s -add-tags json -transform snakecase -w" FILE STRUCT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs Lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun docblock (comment)
  (interactive (list (read-string "comment: ")))
  (insert (format ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; %s\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
" comment)))

(use-package elisp-mode :straight nil
  :bind
  (:map emacs-lisp-mode-map
	("C-c c" . 'docblock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other Languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29) (use-package csharp-mode))
(use-package typescript-mode)
(use-package lua-mode)
(use-package tuareg) ;; ocaml


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-dwim ()
  ""
  (interactive)
  (cond
   ((projectile-project-p) (call-interactively 'projectile-compile-project)
    t (call-interactively 'compile))
   )
  )

(use-package compile
  :bind
   (:map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))

(global-set-key (kbd "<f5>") 'compile-dwim)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: Emacs Git Client

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code Formatting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun amirreza/format-dwim () (interactive) (if (use-region-p) (format-all-region) (format-all-buffer)))
(use-package format-all
  :bind
  ("<f9>" . 'amirreza/format-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Language Server Protocol (LSP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :hook (((go-mode rust-mode) . #'lsp))
  :init
  (setq read-process-output-max (* 2 1024 1024) ;; 2mb
	lsp-log-io nil ;; disable logging IO requests/responses
	lsp-use-plists t)  ;; Performance tweaks
  (setq lsp-auto-guess-root t) ;; don't ask for project root detection
  (setq lsp-headerline-breadcrumb-enable nil) ;; Disable UI elements
  :bind
  (:map lsp-mode-map
	("<f12>" . lsp-find-definition)
	("M-<f12>" . lsp-find-references)
	("C-<f12>" . lsp-find-implementation))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eldoc: Emacs Documentation Presentation Frontend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc :straight nil
  :bind
  (("C-h ." . eldoc)
   ("M-h" . eldoc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; xref: Emacs Goto Facilities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package xref :straight nil
  :bind
  (("<f12>" . xref-find-definitions)
   ("C-<down-mouse-1>" . xref-find-definitions)
   ("M-<f12>" . xref-find-references)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flymake: Emacs Diagnostics Facilities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flymake :straight nil
  :bind
  (:map flymake-mode-map
	("M--" . flymake-goto-prev-error)
	("M-=" . flymake-goto-next-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Searching and Grep

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rg) ;; Ripgrep

(use-package wgrep) ;; Writeable Grep Buffers

(use-package isearch :straight nil
  :bind
  (("C-." . 'isearch-forward-thing-at-point)
   :map
   isearch-mode-map
   ("C-." . 'isearch-repeat-forward)))

(defun grep-dwim ()
  "dwim variation of grep command using combination of projectile and emacs grep"
  (interactive)
  (cond
   ((and (projectile-project-p) (executable-find "rg")) (call-interactively 'projectile-ripgrep))
   ((and (projectile-project-p)) (call-interactively 'projectile-grep))
   ((executable-find "rg") (call-interactively 'rg))
   (t (call-interactively 'grep))))

(global-set-key "\C-xpg" 'grep-dwim)
(global-set-key (kbd "C-S-f") 'grep-dwim) ;; old habbits, ctrl+shift+f
