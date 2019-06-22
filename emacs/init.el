(package-initialize)

;; get user init file path
(setq user-init-file (or load-file-name (buffer-file-name)))
;; get user emacs directory
(setq user-emacs-directory (file-name-directory user-init-file))
;;add extentions to emacs load path
(add-to-list 'load-path (concat user-emacs-directory "/ext"))
(add-to-list 'load-path (concat user-emacs-directory "/ext/go"))

(require 'core)

;; list of needed packages
(setq pkgs '(
	     dracula-theme
	     exec-path-from-shell
	     spacemacs-theme
	     php-mode
	     web-mode
	     auto-complete
	     go-mode
	     elpy
	     flycheck
	     jedi
	     ))

(initialize)

(require 'ext-python)
(require 'ext-go)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell web-mode spacemacs-theme php-mode jedi go-mode flycheck elpy dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
