(package-initialize)

;; get user init file path
(setq user-init-file (or load-file-name (buffer-file-name)))
;; get user emacs directory
(setq user-emacs-directory (file-name-directory user-init-file))
;;add extentions to emacs load path
(add-to-list 'load-path (concat user-emacs-directory "/ext"))

(require 'core)

;; list of needed packages
(setq pkgs '(
	     dracula-theme
	     spacemacs-theme
	     php-mode
	     web-mode
	     go-mode
	     elpy
	     flycheck
	     jedi
	     ))

(initialize)

(require 'ext-python)
