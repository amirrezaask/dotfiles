(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; add my scripts to load path

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq backup-by-copying t) ;; Always copy files for backup.
(setq version-control t) ;; Use version numbers for backup.
(setq delete-old-versions t) ;; Delete old backup of files.
(setq kept-new-versions 6) ;; Number of newest versions to keep.
(setq kept-old-versions 2) ;; Number of old versions to keep.
(setq create-lockfiles nil) ;; Don't create .# files as lock.

(delete-selection-mode 1)

(setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
      '(("." . "~/.emacs.d/backup")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq-default indent-tabs-mode nil ;; Don't insert tabs for indentation.
              tab-width 4) ;; Width of the TAB character in display.

(setq-default cursor-type 'box)


(defalias 'yes-or-no-p 'y-or-n-p) ;; Show y or n instead of yes or no for question prompts.

(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 10)))
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 10)))

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

(global-display-line-numbers-mode 1) ;; Ensure line numbers globally.

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(when (> emacs-major-version 26) (global-tab-line-mode -1)) ;; Disable tab line in Emacs 27+.

;; Font
(package-install 'gruber-darker-theme)

(package-install 'exec-path-from-shell)
(setq exec-path-from-shell-shell-name "zsh")
(exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
(exec-path-from-shell-initialize)


;; Highlight indents for yaml
(package-install 'highlight-indent-guides)
(add-hook 'yaml-mode-hook-hook #'highlight-indent-guides)
(add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
(setq highlight-indent-guides-method 'character)


;; Expand region
(package-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(package-install 'vlf)
(global-so-long-mode 1)


;; Company ( Auto complete )
(package-install 'company)
(global-company-mode)
(add-hook 'company-mode-hook (lambda ()
                               (define-key company-active-map (kbd "C-n") #'company-select-next)
                               (define-key company-active-map (kbd "C-p") #'company-select-previous)
                               (define-key company-active-map (kbd "C-o") #'company-other-backend)
                               (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
                               (define-key company-active-map (kbd "RET") #'company-complete-selection)
                               (setq company-minimum-prefix-lenght 1)
                               (setq company-tooltip-limit 30)
                               (setq company-idle-delay 0.0)
                               (setq company-echo-delay 0.1)
                               (setq company-show-numbers t)
                               (setq company-backends '(company-capf company-dabbrev company-files company-dabbrev-code))
                               ))



;; Magit
(package-install 'magit)

;; Org mode
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit) ;; consitent with magit commit
                           (setq org-ellipsis "⤵")
                           (setq org-src-fontify-natively t)
                           (setq org-src-tab-acts-natively t)
                           (setq org-support-shift-select t)
                           (setq org-src-window-setup 'current-window)
                           (setq org-startup-folded t)
                           ))

;; Golang
(package-install 'go-mode)
(add-hook 'go-mode-hook (lambda () (add-to-list 'exec-path (concat (getenv "HOME") "/bin"))))

;; Jai
(require 'jai-mode)

;; Loki
(require 'loki-mode)
;; PHP
(package-install 'php-mode)

;; Rust
(package-install 'rust-mode)

;; Ziglang
(package-install 'zig-mode)
(setq zig-format-on-save nil)

;; Haskell
(package-install 'haskell-mode)

;; Configuration formats
(package-install 'apache-mode)
(package-install 'systemd)
;;(package-install 'nginx-mode)
(package-install 'docker-compose-mode)
(package-install 'dockerfile-mode)

;; LSP
(package-install 'eglot)
(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'php-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)


(package-install 'cmake-mode)

;; Modeline
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-end-spaces))




(defun GREP (text)
  (interactive "sPattern: ")

  (cond
   ((not (null (executable-find "rg"))) (compilation-start
                                          (concat
                                           "rg "
                                           "--line-number "
                                           "--no-heading "
                                           "--ignore-case "
                                           text
                                           )
                                          'grep-mode
                                          ))
    ((vc-backend (buffer-file-name)) (compilation-start
                                      (concat
                                       "git "
                                       "grep -n "
                                       text
                                       )
                                      'grep-mode
                                      ))
    (t (compilation-start (concat "grep -n -RI " text) 'grep-mode))
     
    )
  )

(defun FIND-FILE ()
  (interactive)
  (cond
   ((vc-backend (buffer-file-name)) (project-find-file))
   (t (call-interactively 'find-file))
   )
  )



(global-set-key (kbd "C-9") #'compile)
(global-set-key (kbd "C-8") #'async-shell-command)
(global-set-key (kbd "C-0") 'GREP)

(global-set-key (kbd "C-x C-f") 'FIND-FILE)

;; UI stuff
(set-face-attribute 'default nil :foreground "#d3b58d" :background "#072626")
(set-face-attribute 'cursor nil :background "green")

(set-face-attribute 'font-lock-comment-face nil :foreground "#118a1a")
(set-face-attribute 'font-lock-function-name-face nil :foreground "white" :bold nil)
(set-face-attribute 'font-lock-keyword-face nil :foreground "#d4d4d4")
(set-face-attribute 'font-lock-string-face nil :foreground "#2ec09c")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#c8d4ec")
(set-face-attribute 'font-lock-warning-face nil :foreground "$504038")
(set-face-attribute 'font-lock-constant-face nil :foreground "#7ad0c6")
(set-face-attribute 'highlight nil :foreground "white")
(set-face-attribute 'mode-line nil :foreground "grey" :background "black") ;; background is the color of text some how.
(set-face-attribute 'region nil :background "#3c02fa")

(set-face-attribute 'widget-single-line-field nil :background "darkgray")
(set-frame-font "JetBrainsMono Nerd Font Mono 16" nil t) ;; Set font

