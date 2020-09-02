(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 6)
(defvar amirreza/emacs-init-timestamp (float-time))

(setq gc-cons-threshold (* 1024 1024 100)) ;; 100MB for Emacs initialization process

(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 1024 1024 20)))) ;; reseting the gc cons to 20MB
(defvar file-name-handler-alist-bak file-name-handler-alist "file name handler backup.")

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook (lambda () (setq file-name-handler-alist file-name-handler-alist-bak)))

(setq package-enable-at-startup nil)

(tool-bar-mode 0) ;; disable tool-bar

(scroll-bar-mode 0) ;; disable scroll-bar

(menu-bar-mode 0) ;; disable menu-bar

(setq vc-follow-symlinks t)
; (delete-file "~/.emacs.d/README.el")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
