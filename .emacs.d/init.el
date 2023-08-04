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

(setq straight-use-package-by-default t)

(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el

(setq make-backup-files nil) ;; no emacs ~ backup files
(setq image-types (cons 'svg image-types)) ;; macos bug

(global-set-key (kbd "C-q") 'set-mark-command) ;; better key to start a selection
(global-unset-key (kbd "C-SPC"))

(defalias 'y-or-n-p 'yes-or-no-p)

(setq mac-command-modifier 'meta) ;; macos again

(use-package exec-path-from-shell ;; use $PATH from default shell
  :init
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x i") (lambda ()
				(interactive)
				(find-file (expand-file-name "init.el" user-emacs-directory))))

(setq inhibit-startup-screen t) ;; disable default start screen

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

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))

(setq ring-bell-function (lambda ()))

(global-hl-line-mode +1)

(setq theme 'gruber-darker)

(set-frame-font "Menlo 18")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'maximized)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package doom-themes)
(use-package ef-themes)
(use-package amirreza-themes :straight (amirreza-themes :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes"))
(use-package gruber-darker-theme)

(load-theme theme t)

(use-package vertico
  :init
  (setq vertico-cycle t)
  (setq vertico-count 25)
  (vertico-mode))

(use-package consult)

(use-package embark
  :bind
  (:map minibuffer-mode-map
  ("C-." . embark-act)))

(use-package wgrep)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package embark-consult)

(use-package corfu
  :bind
  ("C-SPC" . 'completion-at-point)
  :config
  (setq corfu-auto t)
  (global-corfu-mode))

(use-package multiple-cursors
  :bind
  (("C-S-n" . 'mc/mark-next-like-this)
   ("C-S-p" . 'mc/mark-previous-like-this)))

(use-package iedit
  :init
  (setq iedit-toggle-key-default nil)
  :bind
  ("C-S-d" . 'iedit-mode))

(use-package magit
  :bind
  (:map global-map
	("C-0" . magit)
   :map magit-mode-map
   ("C-0" . delete-window)))

(use-package dired
  :straight nil
  :bind
  (:map global-map
   ("C-1" . (lambda () (interactive) (dired default-directory)))
  :map dired-mode-map
	("C-1" . 'previous-buffer)))

(use-package go-mode)

(use-package yaml-mode)

(use-package json-mode)

(use-package rust-mode)

(use-package csharp-mode)

(use-package typescript-mode)

(setq my-projects-location '("~/dev" "~/w"))
(setq mabna-projects-root "~/w")

(defun projects-refresh ()
  (interactive)
  (dolist (loc my-projects-location)
    (project-remember-projects-under loc)))

(use-package project
  :commands (project-remember-projects-under)
  :init
  (projects-refresh) ;; refresh projects on start
  (setq project-switch-commands 'project-dired)
  :bind
  ("C-x p R" . projects-refresh))


(use-package compile
  :bind
  (("<f5>" . compile)
   ("C-x C-x" . compile)
   :map compilation-mode-map
   ("<f5>" . recompile)
   ("C-x C-x" . recompile)
   ("k" . kill-compilation)))

;; Grep
(require 'grep)
(grep-apply-setting 'grep-use-null-device nil)
(grep-apply-setting 'grep-command "grep -rn ")

(when (executable-find "rg")
  (grep-apply-setting
   'grep-command
   "rg -n -H --no-heading -e "))

(global-set-key (kbd "C-x C-g") 'grep)
