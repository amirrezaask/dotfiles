(setq amirreza-emacs-starting-time (float-time)) ;; Store current time for further analysis.
(setq BASE_PATH (file-name-directory load-file-name)) ;; $CWD where this file is.
(setq INIT_FILE load-file-name)
(setq CONFIG_FILE (expand-file-name "config.org" BASE_PATH))
(add-to-list 'load-path (expand-file-name "lisp" BASE_PATH))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" BASE_PATH))
(org-babel-load-file CONFIG_FILE)

(setq amirreza-emacs-init-took (* (float-time (time-subtract (float-time) amirreza-emacs-starting-time)) 1000))
(setq emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000))
(setq amirreza-emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza-emacs-init-took emacs-init-time-took))
(message amirreza-emacs-init-log-message)
