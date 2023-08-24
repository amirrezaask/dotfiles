(setq gc-cons-threshold 100000000) ;; 100 MB
(setq vc-follow-symlinks t)
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

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(setq ring-bell-function (lambda ())) ;; no stupid sounds

(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(setq image-types (cons 'svg image-types)) ;; macos bug

(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
(global-unset-key (kbd "C-SPC"))

(setq use-short-answers t)

(setq mac-command-modifier 'meta) ;; macos again

(use-package exec-path-from-shell ;; use $PATH from default shell
  :init
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x i") (lambda ()
				(interactive)
				(find-file (expand-file-name "init.el" user-emacs-directory))))

(setq inhibit-startup-screen t) ;; disable default start screen

(when (< (frame-width) 162)
  (setq split-width-threshold nil))

(setq recenter-positions '(middle))

(defun jump-up ()
  (interactive)
  (next-line (* -1 (/ (window-height) 2)))
  (recenter-top-bottom))

(defun jump-down ()
  (interactive)
  (next-line (/ (window-height) 2))
  (recenter-top-bottom))

(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)

(global-hl-line-mode +1)

(set-frame-font "Jetbrains Mono 18")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package doom-themes)
(use-package ef-themes)
(use-package amirreza-themes :straight (amirreza-themes :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(use-package gruber-darker-theme)

(load-theme 'naysayer t) ;;

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


(use-package corfu
  :bind
  ("C-SPC" . 'completion-at-point)
  :config
  (setq corfu-auto t)
  (global-corfu-mode))


;; text editing
(use-package multiple-cursors
  :bind
  (("C-S-n" . 'mc/mark-next-like-this)
   ("C-S-p" . 'mc/mark-previous-like-this)))

;; Git
(use-package magit
  :bind
  (:map global-map
	("C-0" . magit)
   :map magit-mode-map
   ("C-0" . delete-window)))


;; Dired, file manager
(use-package dired
  :straight nil
  :bind
  (:map global-map
   ("C-1" . (lambda () (interactive) (dired default-directory)))
  :map dired-mode-map
  ("C-1" . 'previous-buffer)))

;; languages
(use-package go-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package rust-mode)
(when (< emacs-major-version 29)
  (use-package csharp-mode))
(use-package typescript-mode)
(use-package tuareg) ;; ocaml


;; Project
(defun projects-refresh ()
  (interactive)
  (dolist (loc '("~/dev" "~/w"))
    (project-remember-projects-under loc)))

(use-package project
  :commands (project-remember-projects-under)
  :init
  (projects-refresh) ;; refresh projects on start
  (setq project-switch-commands 'project-dired)
  :bind
  ("C-x p R" . projects-refresh))


;; Compile
(use-package compile
  :bind
  (("<f5>" . compile)
   ("C-x C-x" . compile)
   :map compilation-mode-map
   ("<f5>" . recompile)
   ("C-x C-x" . recompile)
   ("k" . kill-compilation)))


(use-package wgrep)

;; Grep
(defun my-grep ()
  "Best Grep command of all time"
  (interactive)
  (let* ((rg-command "rg -n -H --no-heading -e '%s' %s")
	 (gnu-grep-command "grep -rn '%s' %s")
	 (base-command gnu-grep-command)
	 (pattern (read-string "Pattern: "))
	 (dir (read-file-name "Dir: " (if (project-root (project-current)) (project-root (project-current)) default-directory))))
    
    (when (executable-find "rg") (setq base-command rg-command))
    (compilation-start (format base-command pattern dir) #'grep-mode)))

(global-set-key (kbd "C-x C-g") 'my-grep)


(defun eglot-save-with-imports () (interactive)
       (eglot-format-buffer)
       (eglot-code-actions nil nil "source.organizeImports" t))

(add-hook 'go-mode-hook (lambda ()
			  (add-hook 'before-save-hook 'eglot-save-with-imports nil t)))

(use-package eglot
  :hook
  ((go-mode rust-mode tuareg-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
	("C-x C-l" . eglot-save-with-imports)
	("C-c C-c" . eglot-code-actions)))
