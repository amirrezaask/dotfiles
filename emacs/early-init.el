(defvar amirreza-emacs-init-timestamp (float-time) "Holds Emacs initialization time.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun amirreza-faster-start ()
  ;; Defer Garbage collection
  (setq gc-cons-threshold most-positive-fixnum)

  ;; Restore garbage collection after initialization
  (add-hook 'after-init-hook (lambda ()
                               (require 'gcmh)
                               (gcmh-mode 1)))
  ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)


  (defvar file-name-handler-alist-bak file-name-handler-alist "file name handler backup.")
  
  (setq file-name-handler-alist nil)

  (add-hook 'after-init-hook (lambda () (setq file-name-handler-alist file-name-handler-alist-bak)))

  (setq package-enable-at-startup nil)

  (tool-bar-mode 0) ;; disable tool-bar

  (scroll-bar-mode 0) ;; disable scroll-bar

  (menu-bar-mode 0) ;; disable menu-bar

  (setq vc-follow-symlinks t)

  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil))

;; I have no time for nonsense
(amirreza-faster-start)
