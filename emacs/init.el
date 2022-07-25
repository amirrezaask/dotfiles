(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq backup-by-copying t) ;; Always copy files for backup.
(setq version-control t) ;; Use version numbers for backup.
(setq delete-old-versions t) ;; Delete old backup of files.
(setq kept-new-versions 6) ;; Number of newest versions to keep.
(setq kept-old-versions 2) ;; Number of old versions to keep.
(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
      '(("." . "~/.config/emacs/backup/")))

(setq-default indent-tabs-mode nil ;; Don't insert tabs for indentation.
                tab-width 4) ;; Width of the TAB character in display.


(defalias 'yes-or-no-p 'y-or-n-p) ;; Show y or n instead of yes or no for question prompts.

(setq echo-keystrokes 0.1) ;; Show keystrokes in minibuffer faster than default.

(setq use-dialog-box nil) ;; Don't use any kind of GUI dialog box.

(setq inhibit-splash-screen 0) ;; Disable Emacs start screen.

(setq ring-bell-function 'ignore) ;; No bell ringing.

(set-terminal-coding-system 'utf-8) ;; default emacs encodings
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default fill-column 80) ;; column number which emacs start to line wrap.

(setq scroll-step 5) ;; When point moves out of screen, number of lines to scroll
(setq scroll-margin 5) ;; Scroll margin lines, when point arrives at these margins scroll the display.
(setq scroll-conservatively 101) ;; Number of lines to scroll to bring point back into view.
(setq scroll-up-aggressively 0.11) ;; When scrolling how much to move the view.
(setq scroll-down-aggressively 0.01) ;; Same as above.
(setq auto-window-vscroll nil) ;; Disable changing window-vscroll automatically.
(setq fast-but-imprecise-scrolling nil) ;; Disable fast scroll since it does not feel good.
(setq mouse-wheel-scroll-amount '(5
                                  ((shift) . 10)))
(setq mouse-wheel-progressive-speed t)

;; Horizontal Scroll
(setq hscroll-step 1) ;; Number of columns to scroll when point is to close to edge.
(setq hscroll-margin 1) ;; How many columns away from edge to start scrolling.

(setq custom-file "~/.config/emacs/custom.el") ;; Don't tamper with init.el for custom variables and use given file.

(column-number-mode +1) ;; Show column number in modeline.
(display-battery-mode 1) ;; Show battery in modeline.
(display-time-mode 1) ;; Show time in modeline.
(global-display-line-numbers-mode 1) ;; Ensure line numbers globally.

(use-package exec-path-from-shell :straight t
  :config
    (setq exec-path-from-shell-shell-name "zsh")
    (exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
    (exec-path-from-shell-initialize))

(load-theme 'tango-dark t) ;; set theme

(set-frame-font "JetBrainsMono Nerd Font Mono 16" nil t) ;; Set font

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(when (> emacs-major-version 26) (global-tab-line-mode -1)) ;; Disable tab line in Emacs 27+.

(blink-cursor-mode 1) ;; Cursor blinks.


(use-package helpful
  :straight t
  :bind
  (("C-h s" . helpful-symbol)
        ("C-h k" . helpful-key)
        ("C-h v" . helpful-variable)
        ("C-h c" . helpful-command)
        ("C-h f" . helpful-function)
        )
)

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
