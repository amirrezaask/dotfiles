(package-initialize)


(require 'package)
(require 'cl)

;; install packages if not installed
(defun install (required-pkgs)
  (setq pkgs-to-install
      (let ((uninstalled-pkgs (remove-if 'package-installed-p required-pkgs)))
        (remove-if-not '(lambda (pkg) (y-or-n-p (format "Package %s is missing. Install it? " pkg))) uninstalled-pkgs)))

  (when (> (length pkgs-to-install) 0)
    (package-refresh-contents)
   (dolist (pkg pkgs-to-install)
(package-install pkg))))



;; list of needed packages
(setq pkgs '(
	     spacemacs-theme
	     php-mode
	     ))


;; main function to initialize emacs
(defun initialize ()
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;; enables the line numbers globally
  (global-linum-mode t) 
 
  ;; Setting default font to Consolas
  (set-face-attribute 'default nil
		      :family "Fira Code"
		      :height 110
		      :weight 'normal
		      :width 'normal)
  (load-theme 'spacemacs-dark t) 
  (cua-mode t)
  (install pkgs))

(initialize)
