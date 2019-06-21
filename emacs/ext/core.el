(require 'package)
(require 'cl)
(package-initialize)


;; install packages if not installed
(defun install (required-pkgs)
  (setq pkgs-to-install
      (let ((uninstalled-pkgs (remove-if 'package-installed-p required-pkgs)))
        (remove-if-not '(lambda (pkg) (y-or-n-p (format "Package %s is missing. Install it? " pkg))) uninstalled-pkgs)))

  (when (> (length pkgs-to-install) 0)
    (package-refresh-contents)
   (dolist (pkg pkgs-to-install)
(package-install pkg))))






;; main function to initialize emacs
(defun initialize ()
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
  ;; enables the line numbers globally
  (global-linum-mode t)
  ;;highlight parens in emacs
  (show-paren-mode 1)
  ;; hide emacs start screen
  (setq inhibit-startup-screen t)
  ;; install missing pkgs
  (install pkgs)
  ;; Setting default font to Consolas
  (set-face-attribute 'default nil
		      :family "Fira Code"
		      :height 110
		      :weight 'normal
		      :width 'normal)
  (load-theme 'spacemacs-dark t) 
  (cua-mode t))



(provide 'core)
