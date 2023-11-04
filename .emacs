;; Basic
(setq gc-cons-threshold 200000000) ;; 200 MB
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

(defun amirreza/set-font (font fontsize)
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun amirreza/laptop ()
  (interactive)
  (amirreza/set-font "Jetbrains Mono" 11))

(defun amirreza/benq ()
  (interactive)
  (amirreza/set-font "Jetbrains Mono" 13))

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
(defun amirreza/find-file ()
  (interactive)
  (if (and (fboundp 'project-current) (fboundp 'project-root) (project-root (project-current)))
      (project-find-file)
    (find-file)))

(global-set-key (kbd "M-o") 'amirreza/find-file)
(setq recenter-positions '(middle))
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)
;; Navigation END

;; Modeline
(defun amirreza/modeline-vc () (interactive) (propertize (if vc-mode vc-mode "") 'face '(:weight light)))
(defun amirreza/modeline-file () (interactive) (propertize (format "%s%s%s" (if (buffer-modified-p (current-buffer)) " [*] " "") default-directory (buffer-name (current-buffer))) 'face '(:weight light)))
(defun amirreza/modeline-linecol () (interactive) (propertize "%l:%c"))
(defun amirreza/modeline-major-mode () (interactive) (propertize (substring (capitalize (symbol-name major-mode)) 0 -5) 'face '(:weight light)))
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
(global-hl-line-mode)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; GUI END

;; Themes
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i)))
(use-package ef-themes)
(setq custom-safe-themes t)
(setq amirreza/themes-list '(
			     ;; Modus Light Themes
			     modus-operandi
			     modus-operandi-tinted
			     modus-operandi-deuteranopia
			     modus-operandi-tritanopia

			     ;; Modus Dark Themes
			     modus-vivendi
			     modus-vivendi-tinted
			     modus-vivendi-deuteranopia
			     modus-vivendi-tritanopia

			     ;; EF Dark Themes
			     ef-autumn
			     ef-bio
			     ef-cherie
			     ef-dark
			     ef-deuteranopia-dark
			     ef-duo-dark
			     ef-elea-dark
			     ef-maris-dark
			     ef-melissa-dark
			     ef-night
			     ef-symbiosis
			     ef-trio-dark
			     ef-tritanopia-dark
			     ef-winter

			     ;; EF Light Themes
			     ef-cyprus
			     ef-day
			     ef-deuteranopia-light
			     ef-duo-light
			     ef-elea-light
			     ef-frost
			     ef-kassio
			     ef-light
			     ef-maris-light
			     ef-melissa-light
			     ef-spring
			     ef-summer
			     ef-trio-light
			     ef-tritanopia-light
			     ))
(defun amirreza/load-random-theme ()
  (interactive)
  (let ((theme (nth (random (length amirreza/themes-list)) amirreza/themes-list)))
    (load-theme theme)
    (message "Loaded %s" (symbol-name theme))))

(global-set-key (kbd "<f1>") 'amirreza/load-random-theme)
(amirreza/load-random-theme)
;; Themes END

;; minibuffer
(use-package vertico :init (setq vertico-cycle t) (setq vertico-count 25) (vertico-mode))
(use-package consult)
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

;; Text Editing
(delete-selection-mode)
(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
(use-package multiple-cursors
  :bind
  (("C-S-n" . 'mc/mark-next-like-this)
   ("C-S-p" . 'mc/mark-previous-like-this)))
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

;; dired
(use-package dired-sidebar
  :bind ("C-1" . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar))
;; dired END

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
(defun amirreza/format-dwim () (interactive) (if (use-region-p) (format-all-region) (format-all-buffer)))
(use-package format-all
  :bind
  ("C-c m f" . 'amirreza/format-dwim))
;; formatter END

(global-set-key (kbd "C-C n") 'find-file-other-frame)

;; indent guides
(use-package highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
;; indent guides END

;; Eglot
(unless (>= emacs-major-version 29)
  (straight-use-package 'eglot))

(defun eglot-save-with-imports () (interactive)
       (eglot-format-buffer)
       (eglot-code-actions nil nil "source.organizeImports" t))

(add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-save-with-imports nil t)))

(use-package eglot :straight nil
  :init
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider)
        eglot-autoshutdown t
        eldoc-idle-delay 0.75
	eldoc-documentation-strategy 'eldoc-documentation-compose
        flymake-no-changes-timeout 0.5) ;; eglot performance improvement by doing less work
  :hook
  ((go-mode rust-mode tuareg-mode) . eglot-ensure) ;; Go + Rust + Ocaml
  :bind
  (:map eglot-mode-map
	("M-<f12>" . eglot-find-implementation)
	("C-c m c" . eglot-code-actions)))
;; Eglot END

;; XRef
(use-package xref :straight nil
  :bind
  (("M-." . xref-find-definitions)
   ("<f12>" . xref-find-definitions)
   ("S-<f12>" . xref-find-references)
   ("M-r" . xref-find-references)))
;; XRef END

;; Search and Grep
(use-package isearch :straight nil
  :bind
  (("C-." . 'isearch-forward-thing-at-point)
   :map
   isearch-mode-map
   ("C-." . 'isearch-repeat-forward)))

(use-package wgrep)
(grep-apply-setting 'grep-command "grep --exclude-dir='.git' --color=auto -nH --null -r -e ")
(when (executable-find "rg")
  (grep-apply-setting 'grep-command "rg --vimgrep ")
  (grep-apply-setting 'grep-use-null-device nil))
(global-set-key (kbd "C-c s") 'grep)
;; Search and Grep END
