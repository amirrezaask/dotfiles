(when load-file-name
  (setq BASE_PATH (file-name-directory load-file-name)) ;; Store this file location.
  (setq INIT_FILE load-file-name)
  (setq CONFIG_FILE (expand-file-name "emacs.org" BASE_PATH)))

(org-babel-load-file CONFIG_FILE)
