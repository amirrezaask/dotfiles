
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 10)))
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 10)))

(setq echo-keystrokes 0.1) ;; Show keystrokes in minibuffer faster than default.

(setq use-dialog-box nil) ;; Don't use any kind of GUI dialog box.
(setq inhibit-splash-screen 0) ;; Disable Emacs start screen.
(setq ring-bell-function 'ignore) ;; No bell ringing.


(setq-default fill-column 80) ;; column number which emacs start to line wrap.

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(unless (eq system-type 'windows-nt)
  (package-install 'exec-path-from-shell)
  (setq exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
  (exec-path-from-shell-initialize)
)

;; Completion
(package-install 'vertico)
(vertico-mode)
(setq vertico-cycle t)

(package-install 'orderless)
;; (setq completion-styles '(substring orderless basic))

;; minibuffer annotations
(package-install 'marginalia)
(marginalia-mode)

;; Some useful functionalities
(package-install 'consult)

;; Expand region
(package-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; handle large files and long lines better
(package-install 'vlf)
(global-so-long-mode 1)

;; In Buffer Completion
(package-install 'corfu)
(setq corfu-auto t)


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
(add-hook 'go-mode-hook
       (lambda ()
	     (if (or (file-exists-p "makefile")
		             (file-exists-p "Makefile"))
             (setq-local compile-command "make")
           (setq-local compile-command "go build ./...")
           )
         )
       )

;; Jai
(require 'jai-mode)

;; OCaml
(package-install 'tuareg)

;; PHP
(package-install 'php-mode)

;; Rust
(package-install 'rust-mode)
(add-hook 'rust-mode-hook
       (lambda ()
	     (if (or (file-exists-p "makefile")
		             (file-exists-p "Makefile"))
             (setq-local compile-command "make")
           (setq-local compile-command "cargo build")
           )
         )
       )

;; Ziglang
(package-install 'zig-mode)
(add-hook 'zig-mode-hook
       (lambda ()
	     (if (or (file-exists-p "makefile")
		             (file-exists-p "Makefile"))
             (setq-local compile-command "make")
           (setq-local compile-command "zig build")
           )
         )
       )

(setq zig-format-on-save nil)

;; Haskell
(package-install 'haskell-mode)

;; Configuration formats
(package-install 'apache-mode)

(package-install 'systemd)

(package-install 'nginx-mode)

(package-install 'docker-compose-mode)

(package-install 'dockerfile-mode)

;; LSP
(package-install 'eglot)

;; disable eglot mouse things
(add-hook 'eglot-managed-mode-hook (lambda ()
          (put 'eglot-node 'flymake-overlay-control nil)
          (put 'eglot-warning 'flymake-overlay-control nil)
          (put 'eglot-error 'flymake-overlay-control nil)
          ))

(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.loki\\'" . jai-mode))

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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


(defun ASYNC-SHELL-COMMAND ()
  (interactive)
  (let ((default-directory (vc-root-dir)))
    (call-interactively 'async-shell-command)
    )
  )

(global-set-key (kbd "C-9") #'compile)
(global-set-key (kbd "C-8") #'ASYNC-SHELL-COMMAND)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-6") 'eglot-format-buffer)
(global-set-key (kbd "C-1") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(add-hook 'grep-mode-hook (lambda ()
                            (define-key grep-mode-map (kbd "M-.") 'find-file-at-point)
                            ))


(package-install 'ace-window)
(global-set-key (kbd "C-x o") 'ace-select-window)



;; UI stuff
(package-install 'ef-themes)
(package-install 'gruber-darker-theme)

(set-face-attribute 'default nil :foreground "#d3b58d" :background "#072626")
(set-face-attribute 'cursor nil :background "green")

(set-face-attribute 'font-lock-comment-face nil :foreground "#118a1a")
(set-face-attribute 'font-lock-function-name-face nil :foreground "white" :bold nil)
(set-face-attribute 'font-lock-keyword-face nil :foreground "#d4d4d4")
(set-face-attribute 'font-lock-string-face nil :foreground "#2ec09c")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#c8d4ec")
(set-face-attribute 'font-lock-warning-face nil :foreground "#504038")
(set-face-attribute 'font-lock-constant-face nil :foreground "#7ad0c6")
(set-face-attribute 'highlight nil :foreground "white")
(set-face-attribute 'mode-line nil :foreground "black" :background "#d3b58d")
(set-face-attribute 'region nil :background "#3c02fa")
      
;; (load-theme 'gruber-darker t)

(defun amirreza/home-monitor ()
  (interactive)
  (set-frame-font "Iosevka 20" nil t))

(defun amirreza/snapp-monitor ()
  (set-frame-font "Iosevka 18" nil t))


(set-frame-font "Iosevka 14" nil t)

