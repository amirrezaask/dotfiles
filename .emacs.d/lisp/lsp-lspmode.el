(straight-use-package 'lsp-mode)

(defun amirreza/lspmode-hook ()
  (lsp)
  (define-key lsp-mode-map (kbd "C-c r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c f") 'lsp-format)
  (define-key lsp-mode-map (kbd "C-c c") 'lsp-code-actions)
  )

(add-hook 'go-mode-hook 'amirreza/lspmode-hook)
(add-hook 'rust-mode-hook 'amirreza/lspmode-hook)
(add-hook 'python-mode-hook 'amirreza/lspmode-hook)
(add-hook 'php-mode-hook 'amirreza/lspmode-hook)

(provide 'lsp-lspmode)
