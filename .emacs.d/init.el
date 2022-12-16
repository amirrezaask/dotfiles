(setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "OperatorMono Nerd Font")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")

(setq amirreza/font-size "21")

(setq amirreza/dark-theme 'ef-night)
(setq amirreza/light-theme 'ef-day)


;; Load all configurations
(load-file (expand-file-name "loader.el" (expand-file-name "lisp" user-emacs-directory)))
