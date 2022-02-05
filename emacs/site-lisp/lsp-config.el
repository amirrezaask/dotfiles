(use-package eglot
  :ensure t
  :hook
  (((go-mode php-mode python-mode lua-mode c-mode elixir-mode erlang-mode) . #'amirreza-eglot-lsp-hook))

  :init
  (defun amirreza-eglot-lsp-hook ()
    (eglot-ensure))
  
  :bind
  (:map global-map
        ("C-c l r" . xref-find-references)
        ("M-." . xref-find-definitions)
        ("C-c l i" . eglot-find-implementation)))

(provide 'lsp-config)
