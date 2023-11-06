;; Basic
(setq gc-cons-threshold 200000000) ;; 200 MB
(setq debug-on-error t)
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
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
(setq amirreza/font-family "Fira Mono")

(defun amirreza/set-font (font fontsize)
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun amirreza/laptop ()
  (interactive)
  (amirreza/set-font amirreza/font-family 11))

(defun amirreza/benq ()
  (interactive)
  (amirreza/set-font amirreza/font-family 13))

(amirreza/laptop)
;; FONT END

;; PATH
(defun home (path)
  (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(setenv "PATH" (string-join exec-path ":")) ;; set emacs process PATH
;; PATH END

;; Navigation
(defun find-file-dwim ()
  (interactive)
  (if (git-repo-root) (git-find-files) (call-interactively 'find-file)))

(global-set-key (kbd "C-x p f") 'find-file-dwim)
(global-set-key (kbd "M-o") 'find-file-dwim)

(setq recenter-positions '(middle))
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)
;; Navigation END

;; Window management
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
;; Window management END
(defun amirreza/shell-execute (COMMAND)
  (interactive (read-string "Command: "))
  (shell-command-to-string (format "printf \"$(%s)\"" COMMAND)))

;; Git Based Commands
(defun git-repo-root (&optional DIR)
  (interactive (list (read-directory-name "Directory: ")))
  (let* ((default-directory (or DIR default-directory))
	 (root (amirreza/shell-execute  "git rev-parse --show-toplevel 2>/dev/null")))
    (if (not (string= root "")) root nil)))

(defun git-compile (&optional DIR)
  (interactive (list (read-directory-name "Directory: ")))
  (let* ((default-directory (or DIR default-directory))
	 (root (amirreza/shell-execute  "git rev-parse --show-toplevel 2>/dev/null"))
	 (default-directory root))
    (call-interactively 'compile)))

(defun git-find-files (&optional DIR)
  (interactive (list (read-directory-name "Directory: ")))
  (let* ((default-directory (or DIR default-directory))
	 (files (amirreza/shell-execute "git ls-files"))
	 (files (string-split files "\n"))
	 (chosen (completing-read (format "[%s] Git Files: " (git-repo-root)) files)))
    (find-file chosen)
    ))
;; Git Based Commands END

;; Modeline
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
;; Modeline END

;; Frame
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default frame-title-format '("%e" "%f"))
;; Frame END

;; GUI
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; GUI END

;; Themes
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package sweet-theme)
(use-package spacemacs-theme)
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)
(setq custom-safe-themes t)
(global-set-key (kbd "M-1") 'ef-themes-load-random)
(load-theme 'ef-maris-dark)
;; Themes END

;; minibuffer
(use-package vertico :init (setq vertico-cycle t) (setq vertico-count 25) (vertico-mode))
(use-package consult)
(global-set-key (kbd "C-x b") 'consult-buffer)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
;; minibuffer END

;; Autocomplete
(use-package corfu
  :config
  (setq corfu-auto t)
  (global-corfu-mode))
;; Autocomplete END

;; Text Editing
(delete-selection-mode)
(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
(use-package multiple-cursors
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))
;; Text Editing END

;; languages
(use-package go-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29) (use-package csharp-mode))
(use-package typescript-mode)
(use-package lua-mode)
(use-package tuareg) ;; ocaml
;; languages END

;; Compile
(use-package compile
  :bind
   (:map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))

(global-set-key (kbd "<F5>") 'git-compile)
;; Compile END

;; Magit
(use-package magit)
;; Magit END

;; formatter
(defun amirreza/format-dwim () (interactive) (if (use-region-p) (format-all-region) (format-all-buffer)))
(use-package format-all
  :bind
  ("<f9>" . 'amirreza/format-dwim))
;; formatter END

(global-set-key (kbd "C-x C-n") 'find-file-other-frame)

;; indent guides
(use-package highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
;; indent guides END

;; LSPMode
(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :hook ((go-mode rust-mode) . #'lsp)
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
;; LSPMode END

;; Eldoc
(use-package eldoc :straight nil
  :bind
  (("C-h ." . eldoc)
   ("M-h" . eldoc)))
;; Eldoc END

;; XRef
(use-package xref :straight nil
  :bind
  (("<f12>" . xref-find-definitions)
   ("M-<f12>" . xref-find-references)))
;; XRef END

;; Diagnostics
(use-package flymake :straight nil
  :bind
  (:map flymake-mode-map
	("M-[" . flymake-goto-prev-error)
	("M-]" . flymake-goto-next-error))
  )
;; Diagnostics END

;; Search and Grep
(use-package isearch :straight nil
  :bind
  (("C-." . 'isearch-forward-thing-at-point)
   :map
   isearch-mode-map
   ("C-." . 'isearch-repeat-forward)))

(defun grep-dwim ()
  "run grep command in either your project root or current directory"
  (interactive)
  (if (git-repo-root)
      (let ((default-directory (git-repo-root)))
	(call-interactively 'grep))
    (let ((default-directory (read-file-name "Directory: ")))
      (call-interactively 'grep))))

(use-package wgrep)
(grep-apply-setting 'grep-command "grep --exclude-dir='.git' --color=auto -nH --null -r -e ")
(when (executable-find "rg")
  (grep-apply-setting 'grep-command "rg --vimgrep ")
  (grep-apply-setting 'grep-use-null-device nil))
(global-set-key (kbd "C-x p g") 'grep-dwim)
(global-set-key (kbd "C-S-f") 'grep-dwim)
;; Search and Grep END
