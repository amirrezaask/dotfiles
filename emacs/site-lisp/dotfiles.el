(defvar amirreza/dotfiles-location (exec-path-from-shell-copy-env "DOTFILES") "Location of my dotfiles.")
(defun amirreza/edit-dot-emacs ()
  (interactive)
  (find-file (completing-read "Edit: " (directory-files-recursively "~/.emacs.d/" ".*" nil (lambda (name)
                                                                                             (not (string-match "\\.git" name)))
                                                                    t))))

(defun amirreza/edit-dot-config ()
  (interactive)
  (find-file (completing-read "Edit: " (directory-files-recursively amirreza/dotfiles-location ".*" nil (lambda (name)
                                                                                                          (not (string-match "\\.git" name)))
                                                                    t))))
(define-key global-map (kbd "C-c e c") 'amirreza/edit-dot-config)
(define-key global-map (kbd "C-c e e") 'amirreza/edit-dot-emacs)
(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "SPC e e") 'amirreza/edit-dot-emacs))
(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "SPC e c") 'amirreza/edit-dot-config))


(provide 'dotfiles)
