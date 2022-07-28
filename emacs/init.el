(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; add my scripts to load path

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
      '(("." . "~/.emacs.d/backup")))

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

(column-number-mode +1) ;; Show column number in modeline.

(global-display-line-numbers-mode 1) ;; Ensure line numbers globally.

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(when (> emacs-major-version 26) (global-tab-line-mode -1)) ;; Disable tab line in Emacs 27+.

(set-frame-font "JetBrainsMono Nerd Font Mono 16" nil t) ;; Set font

(straight-use-package 'vertico)
(vertico-mode 1)
(setq vertico-resize nil
      vertico-count 17
      vertico-cycle t
      completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
              args)))

(straight-use-package 'orderless)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'orderless)

(straight-use-package 'gruber-darker-theme)
;;(load-theme 'gruber-darker t) ;; set theme

(straight-use-package 'consult)

(global-set-key (kbd "C-s") 'consult-line)

(global-set-key (kbd "C-c C-f") 'project-find-file)

(straight-use-package 'exec-path-from-shell)

(setq exec-path-from-shell-shell-name "zsh")
(exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
(exec-path-from-shell-initialize)

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
        ("DEPRECATED" font-lock-doc-face bold))))

(straight-use-package 'vlf)

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
        ("RET" . #'company-complete-selection))
  :config
    (setq company-minimum-prefix-lenght 1)
    (setq company-tooltip-limit 30)
    (setq company-idle-delay 0.0)
    (setq company-echo-delay 0.1)
    (setq company-show-numbers t)
    (setq company-backends '(company-capf company-dabbrev company-files company-dabbrev-code))
  )


(straight-use-package 'magit)

(use-package org
  :config
    (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit) ;; consitent with magit commit
    (setq org-ellipsis "⤵")
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-support-shift-select t)
    (setq org-src-window-setup 'current-window)
    (setq org-startup-folded t))


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

(straight-use-package 'go-mode)
(add-hook 'go-mode-hook (lambda () (add-to-list 'exec-path (concat (getenv "HOME") "/bin"))))

(require 'jai-mode)

(straight-use-package 'php-mode)

(straight-use-package 'rust-mode)
(setq rust-format-on-save t)

(straight-use-package 'zig-mode)

(straight-use-package 'haskell-mode)

(use-package apache-mode :straight t :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'" "access\\.conf\\'"))

(use-package systemd :straight t :mode ("\\.service\\'" "\\.timer\\'"))

(use-package nginx-mode :straight :mode ("/etc/nginx/conf.d/.*" "/etc/nginx/.*\\.conf\\'"))

(use-package docker-compose-mode :straight t :mode "docker-compose\\.yml")

(use-package dockerfile-mode :straight t :mode "\\Dockerfile\\'")

(use-package lsp-mode :straight t :init (setq lsp-headerline-breadcrumb-enable nil) :hook ((go-mode php-mode rust-mode python-mode zig-mode c-mode c++-mode) . lsp) :config (setq lsp-auto-guess-root t))

(use-package yasnippet :straight t :bind (("C-x C-x" . yas-expand) ("C-x C-l" . yas-insert-snippet)) :config (yas-global-mode 1))

(custom-set-faces
 '(default ((t (:foreground "#d3b58d" :background "#072626"))))
 '(cursor-color ((t (:foreground "lightgreen"))))
 '(custom-group-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(custom-variable-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(font-lock-builtin-face ((t nil)))

 '(font-lock-comment-face ((t (:foreground "#616b04"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "white")))) 
 '(font-lock-keyword-face ((t (:foreground "white" ))))

 '(font-lock-string-face ((t (:foreground "#0fdfaf"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "#c8d4ec"))))  

 '(font-lock-warning-face ((t (:foreground "#504038"))))
 '(highlight ((t (:foreground "navyblue" :background "darkseagreen2"))))
 '(mode-line ((t (:inverse-video t))))
 '(mode-line-inactive ((t (:inverse-video t))))
 '(region ((t (:background "blue"))))
 '(widget-field-face ((t (:foreground "white"))) t)
 '(widget-single-line-field-face ((t (:background "darkgray"))) t))

