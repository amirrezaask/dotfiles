(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
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
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(add-to-list 'exec-path "c:/programs/bin")

(if (eq system-type 'windows-nt)
	(setenv "PATH" (string-join exec-path ";"))
	(setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

;; git integration
(defun shell-execute (COMMAND)
  (interactive (read-string "Command: "))
  (if (eq system-type 'windows-nt)
      (shell-command-to-string (format "cmd /c %s" COMMAND))
      (shell-command-to-string (format "sh -c 'printf \"$(%s)\"'" COMMAND))))

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

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(setq recenter-positions '(middle))
(setq custom-safe-themes t) ;; all themes are safe, don't ask
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval default-directory)))
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(use-package corfu :config (global-corfu-mode)) ;; autocomplete
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; auto refresh buffers from disk
(delete-selection-mode) ;; when selected a text and user types delete text

(custom-set-faces
 `(default ((t (:foreground "#d3b58d" :background "#072626"))))
 `(cursor ((t (:background "lightgreen"))))
 `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "white"))))
 `(font-lock-string-face            ((t (:foreground "#0fdfaf"))))
 `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
 `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(mode-line ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive ((t (:foreground "black" :background "white"))))
 `(vertico-current ((t (:background "blue3"))))
 `(error ((t (:background "black" :foreground "red"))))
 `(flymake-error ((t (:background "black" :foreground "red"))))
 `(flymake-warning ((t (:foreground "DarkOrange"))))
 `(flymake-note ((t (:foreground "DarkOrange")))))


;; Language modes
(use-package go-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29) (use-package csharp-mode))
(use-package typescript-mode)
(use-package lua-mode)

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


(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package wgrep) ;; Writeable Grep Buffers
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


(global-set-key (kbd "C-S-f") 'grep-dwim) ;; Smart grep
(global-set-key (kbd "C-z") 'undo) ;; sane undo key
(global-set-key (kbd "C-<return>") 'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "<f5>") 'compile-dwim) ;; |> little green button of my IDE
(global-set-key (kbd "C-:") 'compile-directory) ;; another type of green |> button
(global-set-key (kbd "C-;") 'compile-dwim)
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter) ;; start recording keyboard macro.
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat) ;; end recording keyboard macro.
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-=") 'split-window-vertically)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-q") 'dabbrev-expand) ;; expand current word with suggestions from all buffers.
(global-set-key (kbd "C-v") 'jump-down) ;; better than default scroll up
(global-set-key (kbd "M-v") 'jump-up)   
(global-set-key (kbd "<prior>") 'jump-up)
(global-set-key (kbd "<next>") 'jump-down)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key "\C-x\C-c" 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
