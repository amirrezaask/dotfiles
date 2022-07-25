(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; add my scripts to load path

(require 'pkgmgr)
(require 'basics)
(require 'finder)

(load-theme 'tango-dark t) ;; set theme

(set-frame-font "JetBrainsMono Nerd Font Mono 16" nil t) ;; Set font

(use-package exec-path-from-shell :straight t
  :config
    (setq exec-path-from-shell-shell-name "zsh")
    (exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
    (exec-path-from-shell-initialize))

(use-package highlight-indent-guides
  :hook ((yaml-mode-hook . #'highlight-indent-guides)
         (focus-in-hook . #'highlight-indent-guides-auto-set-faces))
    :straight t
    :config
    (setq highlight-indent-guides-method 'character))

(use-package expand-region :straight t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package hl-todo
  :straight t
  :config
    (global-hl-todo-mode 1)
    (setq hl-todo-highlight-punctuation ":"
      hl-todo-keyword-faces
      `(("TODO"       warning bold)
        ("FIXME"      error bold)
        ("HACK"       font-lock-constant-face bold)
        ("REVIEW"     font-lock-keyword-face bold)
        ("NOTE"       success bold)
        ("DEPRECATED" font-lock-doc-face bold)))
  )

(use-package vlf :straight t)
(global-so-long-mode 1)



(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . #'company-select-next)
        ("C-p" . #'company-select-previous)
        ("C-o" . #'company-other-backend)
        ("<tab>" . #'company-complete-common-or-cycle)
        ("RET" . #'company-complete-selection)
        )
  :config
    (setq company-minimum-prefix-lenght 1)
    (setq company-tooltip-limit 30)
    (setq company-idle-delay 0.0)
    (setq company-echo-delay 0.1)
    (setq company-show-numbers t)
    (setq company-backends '(company-capf company-dabbrev company-files company-dabbrev-code))
  )

(use-package magit :straight t)

(use-package git-messenger :straight t
    :config
      (setq git-messenger:show-detail t)
      (setq git-messenger:use-magit-popup t))

(setq tramp-default-method "ssh")

(add-hook 'pdf-tools-ensured-hook #'menu-bar-mode)

(use-package org
  :config
    (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit) ;; consitent with magit commit

    (defun amirreza/--org-insert-elisp-code-block ()
        (interactive)
        (insert (format "#+begin_src emacs-lisp\n\n#+end_src"))
        (previous-line)
        (beginning-of-line))

    (defun amirreza/--org-insert-no-tangle ()
        ""
        (interactive)
        (insert (format ":PROPERTIES:\n:header-args: :tangle no\n:END:\n"))
        (previous-line)
        (beginning-of-line))

    (setq org-ellipsis "⤵")
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-support-shift-select t)
    (setq org-src-window-setup 'current-window)
    (setq org-startup-folded t)
  )


(use-package perspective :straight t
  :config
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode 1)
  :bind
  (
   ("C-x w s" . persp-switch)
   ("C-x w n" . persp-next)
   ("C-x w k" . persp-kill)
   )
  )

(use-package go-mode
    :straight t
    :mode "\\.go\\'"
    :hook
    (go-mode . (lambda () (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin")))))

(use-package rust-mode :straight t :mode "\\.rs\\'")

(use-package zig-mode
  :mode "\\.zig\\'"
  :straight t)

(use-package haskell-mode :straight t)

(use-package apache-mode :straight t
    :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'" "access\\.conf\\'"))

(use-package systemd :straight t
  :mode ("\\.service\\'" "\\.timer\\'"))

(use-package nginx-mode :straight 
  :mode ("/etc/nginx/conf.d/.*" "/etc/nginx/.*\\.conf\\'"))

(use-package docker-compose-mode
    :straight t
    :mode "docker-compose\\.yml")

(use-package dockerfile-mode :straight t :mode "\\Dockerfile\\'")

(use-package lsp-mode 
  :straight t
  :hook
  ((go-mode php-mode rust-mode python-mode zig-mode c-mode c++-mode) . lsp)

  )

(use-package yasnippet :straight t
  :bind
  (("C-x C-x" . yas-expand)
   ("C-x C-l" . yas-insert-snippet))
  :config
    (yas-global-mode 1)
  )
