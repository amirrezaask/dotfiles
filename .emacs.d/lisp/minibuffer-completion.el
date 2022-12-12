(message "loading minibuffer")
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  )

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))


(use-package savehist
  :init
  (savehist-mode))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package consult
  :bind
  (
   ("C-s" . consult-line)
   ("C-c g" . consult-ripgrep)
   ))


(use-package marginalia
  :init
  (marginalia-mode))


(provide 'minibuffer-completion)
