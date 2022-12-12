(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )


(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  :config
  (evil-escape-mode)
  )


(use-package evil-collection
  :config
  (evil-collection-init))



(provide 'vim)
