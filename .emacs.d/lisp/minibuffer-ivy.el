(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

(straight-use-package 'ivy-rich)
(straight-use-package 'all-the-icons-ivy)

(setq ivy-height 17
      ivy-wrap t
      ivy-fixed-height-minibuffer t
      ivy-read-action-format-function #'ivy-read-action-format-columns
      ivy-use-virtual-buffers nil
      ivy-virtual-abbreviate 'full
      ivy-on-del-error-function #'ignore
      ivy-use-selectable-prompt t)


(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c g") 'counsel-rg)


(ivy-mode 1)
(ivy-rich-mode 1)

(provide 'minibuffer-ivy)
