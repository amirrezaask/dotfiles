(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(global-unset-key (kbd "C-z"))

;; package manager setup
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
  (load bootstrap-file nil 'nomessage)) ;; package manager setup
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq native-comp-async-report-warnings-errors 'silent) ;; silent native compilation warns
(setq use-short-answers t) ;; 
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again
;; Font stuff
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(setq --font-family "")
(defun load-font (font fontsize)
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq --font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun set-font-size (fontsize)
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" --font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(load-font "Fira Code" 11)
;; environment variables env
(defun home (path)
  (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(setenv "PATH" (string-join exec-path ":")) ;; set emacs process PATH
;; git integration
(defun shell-execute (COMMAND)
  (interactive (read-string "Command: "))
  (shell-command-to-string (format "sh -c 'printf \"$(%s)\"'" COMMAND)))

(defun git-repo-root (&optional DIR)
  (interactive (list (read-directory-name "Directory: ")))
  (let* ((default-directory (or DIR default-directory))
	 (root (shell-execute  "git rev-parse --show-toplevel 2>/dev/null")))
    (if (not (string= root "")) root nil)))

(defun git-ls-files (&optional DIR)
  (interactive)
  (let* ((default-directory (or (git-repo-root) (read-directory-name "Directory: ")))
	 (files (shell-execute "git ls-files"))
	 (files (string-split files "\n"))
	 (chosen (completing-read (format "[%s] Git Files: " (git-repo-root)) files)))
    (find-file chosen)))

(global-set-key (kbd "C-x p f") 'git-ls-files)

(use-package magit
  :bind
  ("C-x g" . 'magit-status))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
;; better navigation tools
(defun find-file-dwim ()
  (interactive)
  (if (git-repo-root) (git-ls-files) (call-interactively 'find-file)))	

(global-set-key (kbd "C-c o") 'find-file-dwim)

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(setq recenter-positions '(middle))
(global-set-key (kbd "C-v") 'jump-down) ;; better than default scroll up
(global-set-key (kbd "M-v") 'jump-up)
;; window keys
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
;; colors and themes : prettiness of emacs
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package sweet-theme)
(use-package spacemacs-theme)
(use-package ef-themes)
(use-package gruvbox-theme)
(use-package gruber-darker-theme)
(use-package dracula-theme)
(use-package solarized-theme)
(use-package amirreza-themes :no-require :straight (:host codeberg :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(setq custom-safe-themes t)
(load-theme 'jonathan-blow)
;; custom modeline
(defun amirreza/modeline-vc () (interactive) (propertize (if vc-mode vc-mode "")))
(defun amirreza/modeline-file () (interactive) (propertize (if (buffer-file-name) (buffer-file-name) default-directory)))
(defun amirreza/modeline-modified () (interactive) (propertize (if (buffer-modified-p (current-buffer)) "[+]" "")))
(defun amirreza/modeline-linecol () (interactive) (propertize "%l:%c"))
(defun amirreza/modeline-major-mode () (interactive) (propertize (substring (capitalize (symbol-name major-mode)) 0 -5)))
(defun amirreza/modeline-left () (interactive) (concat (amirreza/modeline-vc)))
(defun amirreza/modeline-center () (interactive) (concat (amirreza/modeline-modified) (amirreza/modeline-file) " " (amirreza/modeline-linecol)))
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

(custom-set-faces
 `(mode-line ((t (:underline nil :box (:color ,(face-foreground 'default))))))
 '(mode-line-inactive ((t (:underline nil))))) ;; make sure our active window is identifiable in a multi window situation

(setq-default mode-line-format '("%e" (:eval (amirreza/modeline-format))))
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval default-directory)))
(global-set-key "\C-x\C-c" 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-display-line-numbers-mode t) ;; line numbers
(setq display-line-numbers 'relative) ;; relative line numbers
(global-hl-line-mode) ;; highlight my current line
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(use-package corfu :config (global-corfu-mode)) ;; autocomplete
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; auto refresh buffers from disk
(delete-selection-mode) ;; when selected a text and user types delete text
(use-package multiple-cursors :bind (("C->" . 'mc/mark-next-like-this) ("C-<" . 'mc/mark-previous-like-this))) ;; multi cursors
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter) ;; start recording keyboard macro
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat) ;; end recording keyboard macro
(global-set-key (kbd "C-q") 'set-mark-command) ;; better selection key

(use-package go-mode) ;; Golang

(defun go-add-tags ()
  "Add Go struct tags using gomodifytags"
  (interactive)
  (unless (executable-find "gomodifytags") (error "Install gomodifytags first. https://github.com/fatih/gomodifytags"))
  (shell-command-to-string (read-string "Command: " (format "gomodifytags -file %s -struct %s -add-tags json -transform snakecase -w" (buffer-name (current-buffer)) (word-at-point)) nil nil nil)))

(defun go-doc (THING)
  "Go docs THING at point"
  (interactive (list (read-string "Symbol: " nil nil (word-at-point) nil)))
  (unless (executable-find "go") (error "Install go toolchain. https://go.dev/downloads"))
  (compile (read-string "Command: " (format "go doc %s" THING) nil nil nil)))

;; no need to explain these
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29) (use-package csharp-mode))
(use-package typescript-mode)
(use-package lua-mode)
(use-package tuareg) ;; ocaml

(defun compile-dwim ()
  "DWIM version of compile"
  (interactive)
  (cond
   ((git-repo-root) (let ((default-directory (git-repo-root))) (call-interactively 'compile)))
   (t (call-interactively 'compile))))

(defun compile-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(use-package compile
  :bind
   (:map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))

(global-set-key (kbd "<f5>") 'compile-dwim)
(global-set-key (kbd "C-:") 'compile-directory)
(global-set-key (kbd "M-c") 'compile-dwim)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; I'm trying to move away from using LSPs, they are a lock-in dependencies and lock you in certain environments
(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :init
  (setq read-process-output-max (* 2 1024 1024) ;; 2mb
	lsp-log-io nil ;; disable logging IO requests/responses
	lsp-use-plists t)  ;; Performance tweaks
  (setq lsp-auto-guess-root t) ;; don't ask for project root detection
  (setq lsp-headerline-breadcrumb-enable nil) ;; Disable UI elements
  (setq lsp-keymap-prefix "C-c m")
  :bind
  (:map lsp-mode-map
	("<f12>" . lsp-find-definition)
	("<f2>" . lsp-rename)
	("M-<f12>" . lsp-find-references)
	("C-c C-c" . lsp-execute-code-action)
	("C-<f12>" . lsp-find-implementation)))

(use-package eglot)

(use-package eldoc :straight nil
  :bind
  (("C-h ." . eldoc)
   ("M-h" . eldoc)))

(use-package xref :straight nil
  :bind
  (("<f12>" . xref-find-definitions)
   ("M-<f12>" . xref-find-references)))

(use-package flymake :straight nil
  :bind
  (:map flymake-mode-map
	("M-p" . flymake-goto-prev-error)
	("M-n" . flymake-goto-next-error)))

(use-package wgrep) ;; Writeable Grep Buffers
(use-package isearch :straight nil
  :bind
  (("C-." . 'isearch-forward-thing-at-point)
   :map
   isearch-mode-map
   ("C-." . 'isearch-repeat-forward)))


(use-package grep :straight nil
  :bind
  (:map grep-mode-map
	("k" . kill-grep))
  :config
  (grep-apply-setting 'grep-command "grep --exclude-dir='.git' --color=auto -nH --null -r -e ")
  (when (executable-find "rg") ;; use rg if available
    (grep-apply-setting 'grep-command "rg --vimgrep ")
    (grep-apply-setting 'grep-use-null-device nil))
  (when (executable-find "ug")
    (grep-apply-setting 'grep-command "ug --exclude-dir='.git' --color=auto -nH --null -r -e ")
    ))

(defun grep-directory (DIR)
  (interactive (list (read-directory-name "Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'grep)))

(defun grep-dwim ()
  "if inside a git repo do grep with repo root as cwd otherwise ask for cwd"
  (interactive)
  (let ((default-directory (or (when (git-repo-root) (git-repo-root)) (read-directory-name "Directory: "))))
    (call-interactively 'grep)))

(global-set-key (kbd "M-s") 'grep-dwim)
(global-set-key (kbd "C-S-s") 'occur)
