(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
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

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(display-battery-mode 1) ;; Show battery in modeline.

(display-time-mode 1) ;; Show time in modeline.

(global-display-line-numbers-mode 1) ;; Ensure line numbers globally.

(show-paren-mode 1) ;; Highlight matching parens
(setq show-paren-delay 0) ;; highlight matching parens instantly.


(global-set-key (kbd "M-n") (lambda ()
                              (interactive)
                              (forward-line 10)))

(global-set-key (kbd "M-p") (lambda ()
                              (interactive)
                              (forward-line -10)))

(when (> emacs-major-version 26) (global-tab-line-mode -1)) ;; Disable tab line in Emacs 27+.


(setq-default cursor-type 'bar) ;; Shape of the cursor.

(blink-cursor-mode 1) ;; Cursor blinks.

(global-hl-line-mode +1) ;; Highlight current line.

;; Add melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
;; Initialize emacs package manager

;; helper to install missing packages
(defun install-pkgs (packages)
  (mapc (lambda (pkg)
	  (unless (package-installed-p pkg)
	    (package-install pkg)))
  packages
  ))


;; Installing packages
(install-pkgs '(doom-themes ;; nice themes
		doom-modeline ;; nice modeline
		evil ;; Vim modal editing
		evil-collection ;; Vim integration for popular packages
        evil-escape ;; jk kj escape from insert
        evil-surround ;; clone of vim-sorround
		delsel ;; delete-selection-mode
        which-key ;; help with keymaps
        bufler ;; better buffer management
        ace-window ;; better window switch
        perspective ;; workspaces
        savehist ;; respect history when searching
        org-bullets ;; bullet points for org documents
        toc-org ;; generate table of contents for org files
        htmlize ;; create html from org file
        highlight-indent-guides ;; highlight indents
        projectile ;; better project detection
        sudo-edit ;; edit files with sudo access
        expand-region ;; expand selected region
        hl-todo ;; highlight todo, etc keywords in code
        so-long ;; minor mode to help emacs handle long lines
        vlf ;; minor mode to help emacs handle large files
        markdown-mode
        pdf-tools
        crontab-mode
        apache-mode
        systemd
        nginx-mode
        rainbow-delimiters
        exec-path-from-shell
        go-mode
        go-add-tags
        gotest
        php-mode
        python-mode
        py-autopep8
        lua-mode
        ccls
        elixir-mode
        alchemist
        erlang
        purescript-mode
        haskell-mode
        lsp-haskell
        lsp-mode
        company
        magit ;; best git client ever ?
        diff-hl
        git-messenger
        yasnippet
        yasnippet-snippets
        eldoc
        ivy
        counsel
        swiper
        all-the-icons-ivy
        ivy-rich
        flx
		))

;; Enable modeline
(doom-modeline-mode 1)

;; Set the theme
(load-theme 'doom-dracula t)

;; Set the font
(set-frame-font "JetBrainsMono Nerd Font Mono 16" nil t)


;; Which key: show available keymaps based on what you typed
(setq which-key-sort-order #'which-key-prefix-then-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10)

(setq which-key-idle-delay 0.3)

(which-key-mode 1)

(which-key-setup-minibuffer)

(defun amirreza/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
)

(defvar evil-want-Y-yank-to-eol t)
(add-hook 'evil-mode-hook 'amirreza/evil-hook)
(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-global-set-key 'normal ";" 'evil-ex)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(evil-collection-init)

(setq-default evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)
(evil-escape-mode 1)

(global-evil-surround-mode 1)

(defvar amirreza/project-locations '("~/src/gitlab.snapp.ir" "~/src/github.com/amirrezaask" "~/src/gitlab.snapp.ir"))
(defun amirreza/find-project ()
  "List of projects in pre defined project locations."
  (interactive)
  (let ((output '()))
    (dolist (path amirreza/project-locations)
      (dolist (p (directory-files path t)) (add-to-list 'output p))
      )
    (dired (completing-read "Project: " output))
    ))
 (global-set-key (kbd "C-c f p") 'amirreza/find-project)

(autoload 'projectile-project-p "projectile.el")
(defun amirreza/grep ()
  (interactive)
  (cond
   ((projectile-project-p) (consult-git-grep))
   ((executable-find "rg") (consult-ripgrep))))

(defun amirreza/find-file ()
  "If we are in project use projectile-find-file else use internal find-file"
  (interactive)
  (cond
   ((projectile-project-p) (projectile-find-file))
   (t (call-interactively 'find-file))))

(defun amirreza/find-symbol-at-point ()
  (interactive)
  (let* ((symbol (thing-at-point 'word)))
    (consult-ripgrep (projectile-project-root) symbol)))

(global-set-key (kbd "C-c f f") 'amirreza/find-file)

(global-set-key (kbd "C-c f s") 'amirreza/grep)

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "SPC f f") 'amirreza/find-file)
  (evil-global-set-key 'normal (kbd "SPC f p") 'amirreza/find-project)
  (evil-global-set-key 'normal (kbd "SPC f s") 'amirreza/grep)
  (evil-global-set-key 'normal (kbd "??") 'amirreza/grep))

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
(setq org-src-window-setup 'split-window-right)
(setq org-startup-folded t)


(with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c m n") 'amirreza/--org-insert-no-tangle)
    (define-key org-mode-map (kbd "C-c m b") 'amirreza/--org-insert-elisp-code-block))

(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'toc-org-mode)




(add-hook 'yaml-mode-hook #'highlight-indent-guide)

(setq highlight-indent-guides-method 'character)

(add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C--") 'er/contract-region)

(add-hook 'prog-mode-hook #'hl-todo-mode)
(setq hl-todo-highlight-punctuation ":"
      hl-todo-keyword-faces
      `(("TODO"       warning bold)
        ("FIXME"      error bold)
        ("HACK"       font-lock-constant-face bold)
        ("REVIEW"     font-lock-keyword-face bold)
        ("NOTE"       success bold)
        ("DEPRECATED" font-lock-doc-face bold)))

(global-so-long-mode 1)

(setq tramp-default-method "ssh")

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'pdf-tools-ensured-hook #'menu-bar-mode)

;; (use-package apache-mode :ensure t
;;     :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'" "access\\.conf\\'"))

;;   (use-package systemd :ensure t
;;     :mode ("\\.service\\'" "\\.timer\\'"))

;;   (use-package nginx-mode :ensure 
;;     :mode ("/etc/nginx/conf.d/.*" "/etc/nginx/.*\\.conf\\'"))

;; (use-package docker-compose-mode
;;     :ensure t
;;     :mode "docker-compose\\.yml")
;; (use-package dockerfile-mode :ensure t :mode "\\Dockerfile\\'")

(setq exec-path-from-shell-shell-name "zsh")
(exec-path-from-shell-copy-envs '("GOPROXY" "GOPRIVATE"))
(exec-path-from-shell-initialize)

(defun amirreza/go-hook ()
    (interactive)
    ;; add go binaries to exec-path
    (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin")))

(add-hook 'go-mode-hook 'amirreza/go-hook)

(add-hook 'python-mode-hook #'(py-autopep8-ensure-on-save))

(setq lsp-clients-lua-language-server-install-dir "/home/amirreza/.local/lua-language-server")

(setq lsp-clients-lua-language-server-bin (concat lsp-clients-lua-language-server-install-dir "/bin/lua-language-server"))

(setq lsp-clients-lua-language-server-main-location (concat lsp-clients-lua-language-server-install-dir "/main.lua"))

(add-hook 'purescript-mode-hook #'turn-on-purescript-indentation)
;; Adds hooks for languages
(add-hook 'go-mode-hook #'lsp)

(add-hook 'go-mode-hook #'lsp)

(add-hook 'php-mode-hook #'lsp)

(add-hook 'c-mode-hook #'lsp)

(add-hook 'lua-mode-hook #'lsp)

(add-hook 'python-mode-hook #'lsp)

(add-hook 'erlang-mode-hook #'lsp)

(add-hook 'purescript-mode-hook #'lsp)

(add-hook 'haskell-mode #'lsp)

(add-hook 'lsp-mode-hook (lambda ()
                           (local-set-key (kbd "C-c f i") 'lsp-find-implementation)
                           ))
(add-hook 'prog-mode-hook #'company-mode)
(with-eval-after-load 'company
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
)

(setq git-messenger:show-detail t)
(setq git-messenger:use-magit-popup t)

(yas-global-mode 1)
(global-set-key (kbd "C-x C-x") 'yas-expand)
(global-set-key (kbd "C-x C-l") 'yas-insert-snippet)

(global-eldoc-mode 1)

(setq ivy-sort-max-size 7500)
(setq ivy-height 13
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

(ivy-mode 1)
(require 'counsel nil t)
(all-the-icons-ivy-setup)
(ivy-rich-mode 1)

(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "??") 'counsel-rg)
(define-key evil-normal-state-map (kbd "SPC f p") 'amirreza/find-project)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC h f") 'describe-function)
(define-key evil-normal-state-map (kbd "SPC h k") 'describe-key)
(define-key evil-normal-state-map (kbd "SPC h v") 'describe-variable)
(define-key evil-normal-state-map (kbd "SPC h a") 'apropos)

