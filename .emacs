(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

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
  "Loads a font."
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq --font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun set-font-size (fontsize)
  "Set a font size"
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" --font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(load-font "Consolas" 11)
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

(use-package ace-window :bind ("C-x C-o" . 'ace-window))

(use-package vertico :init (setq vertico-cycle t) (setq vertico-count 25) (vertico-mode))
(use-package marginalia :config (marginalia-mode))
(use-package consult)
(global-set-key "\C-xb" 'consult-buffer)

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
(global-set-key (kbd "<prior>") 'jump-up)
(global-set-key (kbd "<next>") 'jump-down)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-0") 'delete-window)
(setq custom-safe-themes t) ;; all themes are safe, don't ask
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval default-directory)))
(global-set-key "\C-x\C-c" 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-display-line-numbers-mode t) ;; line numbers
(setq display-line-numbers 'relative) ;; relative line numbers
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(use-package corfu :config (global-corfu-mode)) ;; autocomplete
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; auto refresh buffers from disk
(delete-selection-mode) ;; when selected a text and user types delete text
(use-package multiple-cursors :bind (("C->" . 'mc/mark-next-like-this) ("C-<" . 'mc/mark-previous-like-this))) ;; multi cursors
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter) ;; start recording keyboard macro.
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat) ;; end recording keyboard macro.
(global-set-key (kbd "C-q") 'dabbrev-expand) ;; expand current word with suggestions from all buffers.
(custom-set-faces
 `(default ((t (:foreground "#a9a9a9" :background "gray3"))))
 `(cursor ((t (:background "green"))))
 `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "white"))))
 `(font-lock-string-face            ((t (:foreground "#2ec09c"))))
 `(font-lock-comment-face           ((t (:foreground "#118a1a"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#118a1a"))))
 `(font-lock-doc-face               ((t (:foreground "#118a1a"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#2ec09c"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(mode-line ((t (:foreground "black" :background "#a9a9a9"))))
 `(mode-line-inactive ((t (:foreground "black" :background "white"))))
 `(vertico-current ((t (:background "blue3"))))
 `(error ((t (:background "black" :foreground "red"))))
 `(flymake-error ((t (:background "black" :foreground "red"))))
 `(flymake-warning ((t (:foreground "DarkOrange"))))
 `(flymake-note ((t (:foreground "DarkOrange"))))
 )
(use-package go-mode
  :bind
  ("C-c C-c" . (lambda ()
		 (interactive)
		 (compile "go run *.go")
		 ))
  ) ;; Golang

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


(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :hook (go-mode . lsp)
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
