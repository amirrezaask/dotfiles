;; Elpaca Async package manager setup
(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)


;; Configurations

(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el

(setq make-backup-files nil)
(setq image-types (cons 'svg image-types))

(global-set-key (kbd "C-q") 'set-mark-command)
(global-unset-key (kbd "C-SPC"))

(setq mac-command-modifier 'meta)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x i") (lambda ()
				(interactive)
				(find-file (expand-file-name "README.org" user-emacs-directory))
				))
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

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package doom-themes)
(use-package ef-themes)
(use-package amirreza-themes :elpaca (amirreza-themes :host github :repo "amirrezaask/themes"))
(use-package gruber-darker-theme)

(add-hook 'elpaca-after-init-hook (lambda () (load-theme theme t)))

(use-package vertico
  :init
  (setq vertico-cycle t)
  (setq vertico-count 25)
  (vertico-mode))


(use-package consult
  :bind
  ("C-S-s" . consult-ripgrep))

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

(use-package marginalia :config (marginalia-mode +1))

(use-package embark-consult)

(use-package corfu
  :bind
  ("C-SPC" . 'completion-at-point)
  :config
  (setq corfu-auto t)
  (global-corfu-mode))

(use-package org
  :elpaca nil ;; Use version that was bundled with Emacs
  :hook (org-mode . (lambda () (electric-indent-mode -1)))
  :bind
  (:map org-src-mode
	("C-c C-c" . 'org-edit-src-exit))
  :init
  (setq org-startup-folded t)
  (setq org-src-window-setup 'current-window))

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
	("C-x g" . magit)
	("C-0" . magit)
   :map magit-mode-map
   ("C-0" . delete-window)))

(use-package dired :elpaca nil
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

(setq my-projects-location '("~/dev" "~/w"))
(setq mabna-projects-root "~/w")

(defun projects-refresh ()
  (interactive)
  (dolist (loc my-projects-location)
    (project-remember-projects-under loc)))

(defun project-vterm ()
  (interactive)
  (let* ((name (project-root (project-current)))
	 (buf-name (format "*vterm %s" name))
	 )
    (if (get-buffer buf-name)
	(switch-to-buffer buf-name)
      (vterm buf-name))))

(use-package project :elpaca nil
  :commands (project-remember-projects-under)
  :init
  (projects-refresh) ;; refresh projects on start  
  :bind
  ("C-x p R" . projects-refresh)
  ("C-x p ;" . project-vterm))

(use-package compile :elpaca nil
  :bind
  (("<f5>" . compile)
   :map compilation-mode-map
   ("<f5>" . recompile)
   ("k" . kill-compilation)))
