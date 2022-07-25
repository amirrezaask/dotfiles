(use-package vertico
    :straight t
    :init
    (vertico-mode 1)
    :config
    (setq vertico-resize nil
          vertico-count 17
          vertico-cycle t
          completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args))))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  :bind
  (
   ("C-s" . consult-line)
   )
  )

(provide 'finder)
