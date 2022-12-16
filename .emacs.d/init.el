(setq amirreza/font "FiraCode Nerd Font Mono")

(setq amirreza/font-size "21")

(setq amirreza/dark-theme 'ef-night)
(setq amirreza/light-theme 'ef-day)


;; Setup package manager.
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



(use-package emacs
  :config
  (setq backup-by-copying t) ;; Always copy files for backup.
  (setq version-control t) ;; Use version numbers for backup.
  (setq delete-old-versions t) ;; Delete old backup of files.
  (setq kept-new-versions 6) ;; Number of newest versions to keep.
  (setq kept-old-versions 2) ;; Number of old versions to keep.
  (setq create-lockfiles nil) ;; Don't create .# files as lock.
  (setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
	'(("." . "~/.emacs.d/backup")))
  (delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
  (show-paren-mode 1) ;; Highlight matching parens
  (setq show-paren-delay 0) ;; highlight matching parens instantly.
  (global-display-line-numbers-mode 1)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
)

;; Orderless completion matching algorithm
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Buffer management UI
(use-package bufler
  :bind
  ("C-x b" . bufler))

;; Better window management facilities
(use-package ace-window
  :bind
  ("C-x o" . ace-window))

;; Load all configurations
(load-file (expand-file-name "loader.el" (expand-file-name "lisp" user-emacs-directory)))


