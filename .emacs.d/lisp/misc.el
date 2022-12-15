(use-package apache-mode)

(use-package vterm)

(use-package systemd)

(use-package nginx-mode)

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path (expand-file-name "bin" user-emacs-directory))

(global-set-key (kbd "C-9") #'compile)
(global-set-key (kbd "C-8") #'async-shell-command)

(when (file-executable-p "rg")
			 (grep-apply-setting
			  'grep-command
			  "rg --no-heading --vimgrep "))

(global-set-key (kbd "C-0") #'grep)

(global-set-key (kbd "C-1") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))

(add-hook 'grep-mode-hook (lambda ()
                            (define-key grep-mode-map (kbd "M-.") 'find-file-at-point)
                            ))




(provide 'misc)
