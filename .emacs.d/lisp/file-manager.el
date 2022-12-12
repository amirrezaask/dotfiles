(use-package dired
  :straight nil
  :hook (dired-mode . (lambda ()
                             (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
                             )))


(provide 'file-manager)
