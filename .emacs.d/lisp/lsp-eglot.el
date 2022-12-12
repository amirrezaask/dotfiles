(straight-use-package 'eglot )

(global-eldoc-mode 1)

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer nil)

(defun amirreza/eglot-hook ()
  (eglot-ensure)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (put 'eglot-error 'flymake-overlay-control nil)

  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c c") 'eglot-code-actions)
  )

(add-hook 'go-mode-hook 'amirreza/eglot-hook)
(add-hook 'rust-mode-hook 'amirreza/eglot-hook)
(add-hook 'python-mode-hook 'amirreza/eglot-hook)
(add-hook 'php-mode-hook 'amirreza/eglot-hook)

(provide 'lsp-eglot)
