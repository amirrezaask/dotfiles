;; (setq debug-on-error t)


;; Feed early-init.el in case of it does not exists
(unless (file-exists-p (expand-file-name "early-init.el" user-emacs-directory))
  (append-to-file " ;;; Auto generated from init.el
(setq package-enable-at-startup nil)
(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
" nil (expand-file-name "early-init.el" user-emacs-directory))
  )


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; If we want to join DARK SIDE ( aka enable evil mode and evil integration in packages )
;; (setq amirreza/darkside t)

(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")
;; (setq amirreza/font "FiraCode Nerd Font Mono")
(setq amirreza/font "JetBrainsMono Nerd Font Mono")
(setq amirreza/font-size "21")
(setq amirreza/theme 'jblow)

(require 'amirreza-core)
(require 'amirreza-vim)
(require 'amirreza-emacs)
(require 'amirreza-buffer)
(require 'amirreza-company)
(require 'amirreza-dired)
(require 'amirreza-editor)
(require 'amirreza-eglot)
(require 'amirreza-font)
(require 'amirreza-git)
(require 'amirreza-help)
(require 'amirreza-languages)
(require 'amirreza-minibuffer)
(require 'amirreza-modeline)
(require 'amirreza-org)
(require 'amirreza-prescient)
(require 'amirreza-project)
(require 'amirreza-theme)
(require 'amirreza-treesitter)
(require 'amirreza-windows)
(require 'amirreza-workspaces)
(require 'amirreza-macos)
