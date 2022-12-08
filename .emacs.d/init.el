(setq amirreza/font "JetBrainsMono Nerd Font Mono")

(defun amirreza/home-monitor ()
  (interactive)
  (set-frame-font (concat amirreza/font " 20") nil t))

(defun amirreza/laptop ()
  (interactive)
  (set-frame-font (concat amirreza/font " 19") nil t))

(amirreza/laptop)

;; Package manager setup
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


(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  )


(use-package savehist
  :init
  (savehist-mode))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package consult
  :bind
  (
   ("C-s" . consult-line)
   ))


(use-package marginalia
  :init
  (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package eglot
  :hook ((go-mode-hook) . eglot-ensure)
  )


(use-package ef-themes)
(use-package doom-themes)
(use-package gruber-darker-theme)

(load-theme 'doom-one t)

(setq backup-by-copying t) ;; Always copy files for backup.
(setq version-control t) ;; Use version numbers for backup.
(setq delete-old-versions t) ;; Delete old backup of files.
(setq kept-new-versions 6) ;; Number of newest versions to keep.
(setq kept-old-versions 2) ;; Number of old versions to keep.
(setq create-lockfiles nil) ;; Don't create .# files as lock.

(setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
      '(("." . "~/.emacs.d/backup")))

(delete-selection-mode 1) ;; 

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(global-display-line-numbers-mode 1)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; add my scripts to load path

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(use-package projectile)


(use-package magit
  :bind
  (("C-x g" . magit)))


(use-package go-mode
  :hook (go-mode . eglot-ensure))


(use-package rust-mode
  :hook (rust-mode . eglot-ensure))


;; Modeline
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-end-spaces))



(use-package dired
  :straight nil
  :hook (dired-mode . (lambda ()
                             (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
                             )))
(use-package apache-mode)

(use-package systemd)

(use-package nginx-mode)

(use-package docker-compose-mode)

(use-package dockerfile-mode)
