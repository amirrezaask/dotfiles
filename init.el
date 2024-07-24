(setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 20) ;; 20 Megabytes
		  gc-cons-percentage 0.2)))

(use-package emacs
  :config
  (setq frame-resize-pixelwise t
	frame-inhibit-implied-resize t
	ring-bell-function 'ignore
	use-dialog-box t ; only for mouse events, which I seldom use
	use-file-dialog nil
	use-short-answers t
	inhibit-splash-screen t
	inhibit-startup-screen t
	inhibit-x-resources t
	inhibit-startup-buffer-menu t)
  (setq image-types (cons 'svg image-types) mac-command-modifier 'meta) ;; Fix macos fucked up bugs.
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(setq smilies '("
                          oooo$$$$$$$$$$$$oooo
                      oo$$$$$$$$$$$$$$$$$$$$$$$$o
                   oo$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$o         o$   $$ o$
   o $ oo        o$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$o       $$ $$ $$o$
oo $ $ \"$      o$$$$$$$$$    $$$$$$$$$$$$$    $$$$$$$$$o       $$$o$$o$
\"$$$$$$o$     o$$$$$$$$$      $$$$$$$$$$$      $$$$$$$$$$o    $$$$$$$$
  $$$$$$$    $$$$$$$$$$$      $$$$$$$$$$$      $$$$$$$$$$$$$$$$$$$$$$$
  $$$$$$$$$$$$$$$$$$$$$$$    $$$$$$$$$$$$$    $$$$$$$$$$$$$$  \"\"\"$$$
   \"$$$\"\"\"\"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     \"$$$
    $$$   o$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     \"$$$o
   o$$\"   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$       $$$o
   $$$    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\" \"$$$$$$ooooo$$$$o
  o$$$oooo$$$$$  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   o$$$$$$$$$$$$$$$$$
  $$$$$$$$\"$$$$   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$     $$$$\"\"\"\"\"\"\"\"
 \"\"\"\"       $$$$    \"$$$$$$$$$$$$$$$$$$$$$$$$$$$$\"      o$$$
            \"$$$o     \"\"\"$$$$$$$$$$$$$$$$$$\"$$\"         $$$
              $$$o          \"$$\"\"$$$$$$\"\"\"\"           o$$$
               $$$$o                 oo             o$$$\"
                \"$$$$o      o$$$$$$o\"$$$$o        o$$$$
                  \"$$$$$oo     \"\"$$$$o$$$$$o   o$$$$\"\"  
                     \"\"$$$$$oooo  \"$$$o$$$$$$$$$\"\"\"
                        \"\"$$$$$$$oo $$$$$$$$$$       
                                \"\"\"\"$$$$$$$$$$$        
                                    $$$$$$$$$$$$       
                                     $$$$$$$$$$\"      
                                      \"$$$\"\"\"\"
"
      "
                   __ooooooooo__
              oOOOOOOOOOOOOOOOOOOOOOo
          oOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
       oOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
     oOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
   oOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
  oOOOOOOOOOOO*  *OOOOOOOOOOOOOO*  *OOOOOOOOOOOOo
 oOOOOOOOOOOO      OOOOOOOOOOOO      OOOOOOOOOOOOo
 oOOOOOOOOOOOOo  oOOOOOOOOOOOOOOo  oOOOOOOOOOOOOOo
oOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
oOOOO     OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO     OOOOo
oOOOOOO OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO OOOOOOo
 *OOOOO  OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO  OOOOO*
 *OOOOOO  *OOOOOOOOOOOOOOOOOOOOOOOOOOOOO*  OOOOOO*
  *OOOOOO  *OOOOOOOOOOOOOOOOOOOOOOOOOOO*  OOOOOO*
   *OOOOOOo  *OOOOOOOOOOOOOOOOOOOOOOO*  oOOOOOO*
     *OOOOOOOo  *OOOOOOOOOOOOOOOOO*  oOOOOOOO*
       *OOOOOOOOo  *OOOOOOOOOOO*  oOOOOOOOO*
          *OOOOOOOOo           oOOOOOOOO*
              *OOOOOOOOOOOOOOOOOOOOO*
                   \"\"ooooooooo\"\"
"))

(setq initial-scratch-message
      (nth (random 2) smilies))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name))

(setq native-comp-async-report-warnings-errors nil)
(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.

(setq is-windows (eq system-type 'windows-nt)                                      
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

(setq use-package-always-defer t)
(defun edit-init () "Edit this file." (interactive) (find-file INIT-FILE))

(setq package-quickstart t)
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

;; Install packages.
(when has-treesitter
  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))


(setq recenter-positions '(middle))       ;; When doing C-l always recenter.
(setq custom-file "~/.custom.el")         ;; set custom file to not meddle with init.el
(setq make-backup-files nil)              ;; no emacs ~ backup files
(setq vc-follow-symlinks t)               ;; Don't prompt if encounter a symlink file, just follow the link.
(set-default-coding-systems 'utf-8)       ;; always use UTF8
(setq kill-whole-line t)                  ;; kill line and newline char
(global-auto-revert-mode +1)              ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode)                   ;; when selected a text and user types delete text
(global-display-line-numbers-mode +1)     ;; Display line numbers.
(toggle-truncate-lines +1)

(use-package so-long
  :ensure t
  :init
  (global-so-long-mode +1))

(use-package vlf
  :ensure t
  :init
  (require 'vlf-setup))

(use-package multiple-cursors
  :bind
  (("C-M-n"   . 'mc/mark-next-like-this-word)
   ("C-M-p"   . 'mc/mark-previous-like-this-word)
   ("C-S-n"   . 'mc/mark-next-like-this)
   ("C-S-p"   . 'mc/mark-previous-like-this)
   ("C-c m a" . 'mc/mark-all-like-this-dwim)))

(defun copy () "Either copy region or the current line." (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
    (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun cut () "Either cut region or the current line." (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)) ;; copy active region contents
    (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line

(setq font-families (font-family-list))
(defun set-font (font size) "Set font interactively"
       (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (if (x-list-fonts font)
	   (set-face-attribute 'default nil :font (format "%s-%d" font size))))

(cond
 ((member "Liberation Mono" font-families) (set-font "Liberation Mono" 15))
 (is-macos (set-font "Menlo" 15))
 (is-windows (set-font "Consolas" 15)))

(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "w:/bin")
(add-to-list 'exec-path "c:/programs/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(setq completion-styles '(flex initials shorthand substring basic))
;; (with-eval-after-load 'minibuffer
;;   (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
;;   (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
;;   (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
;;   (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
;;   (define-key completion-in-region-mode-map (kbd "RET") 'minibuffer-choose-completion))


(use-package vertico
  :ensure t
  :init
  (vertico-mode +1)
  (setq vertico-cycle t
	vertico-resize nil))

(use-package consult
  :ensure t
  :commands (consult-ripgrep consult-grep)
  :after vertico)

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode +1))

(use-package embark :ensure t)
(use-package embark-consult :ensure t)

;; Making Emacs beautiful
(use-package ef-themes :ensure t
  :bind
  ("M-t" . ef-themes-load-random))

(deftheme braid)
(custom-theme-set-faces
   'braid
  `(default                          ((t (:foreground "#debe95" :background "#202020"))))
  `(hl-line                          ((t (:background "#353535"))))
  `(vertico-current                  ((t (:background "medium blue"))))
  `(region                           ((t (:background "medium blue"))))
  `(cursor                           ((t (:background "lightgreen"))))
  `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
  `(font-lock-type-face              ((t (:foreground "#8cde94"))))
  `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
  `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
  `(font-lock-builtin-face           ((t (:foreground "white"))))
  `(font-lock-string-face            ((t (:foreground "gray70"))))
  `(font-lock-comment-face           ((t (:foreground "yellow"))))
  `(font-lock-comment-delimiter-face ((t (:foreground "yellow"))))
  `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
  `(font-lock-function-name-face     ((t (:foreground "white"))))
  `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
  `(font-lock-warning-face           ((t (:foreground "yellow"))))
  `(font-lock-note-face              ((t (:foreground "khaki2" ))))
  `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
  `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
  `(show-paren-match                 ((t (:background "mediumseagreen")))))

(deftheme witness)
(custom-theme-set-faces
  'witness
 `(default                          ((t (:foreground "#d3b58d" :background "#072626"))))
 `(hl-line                          ((t (:background "#0c4141"))))
 `(region                           ((t (:background  "medium blue"))))
 `(cursor                           ((t (:background "lightgreen"))))
 `(font-lock-keyword-face           ((t (:foreground "white"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "lightgreen"))))
 `(font-lock-string-face            ((t (:foreground "#0fdfaf"))))
 `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
 `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
 `(hightlight                       ((t (:foreground "navyblue" :background "darkseegreen2"))))
 `(font-lock-warning-face           ((t (:foreground "#504038"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))

(deftheme 4coder-fleury)
(custom-theme-set-faces
 '4coder-fleury
 `(default                          ((t (:foreground "#a08563" :background "#0c0c0c"))))
 `(cursor                           ((t (:background "#EE7700"))))
 `(font-lock-keyword-face           ((t (:foreground "#f0c674"))))
 `(font-lock-operator-face          ((t (:foreground "#907553"))))
 `(font-lock-punctuation-face       ((t (:foreground "#907553"))))
 `(font-lock-bracket-face           ((t (:foreground "#907553"))))
 `(font-lock-delimiter-face         ((t (:foreground "#907553"))))
 `(font-lock-type-face              ((t (:foreground "#d8a51d"))))
 `(font-lock-constant-face          ((t (:foreground "#6b8e23"))))
 `(font-lock-variable-name-face     ((t (:foreground "#b99468"))))
 `(font-lock-builtin-face           ((t (:foreground "#DAB98F"))))
 `(font-lock-string-face            ((t (:foreground "#6b8e23"))))
 `(font-lock-comment-face           ((t (:foreground "#686868"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#686868"))))
 `(font-lock-doc-face               ((t (:foreground "#686868"))))
 `(font-lock-function-name-face     ((t (:foreground "#cc5735"))))
 `(font-lock-doc-string-face        ((t (:foreground "#6b8e23"))))
 `(font-lock-preprocessor-face      ((t (:foreground "#DAB98F"))))
 `(font-lock-warning-face           ((t (:foreground "#504038"))))
 `(region                           ((t (:background "#2f2f37"))))
 `(hl-line                          ((t (:background "#171616"))))
 `(vertico-current                  ((t (:inherit hl-line))))
 `(highlight                        ((t (:foreground nil :background "#2f2f37"))))
 `(mode-line                        ((t (:foreground "#cb9401" :background "#1f1f27"))))
 `(mode-line-inactive               ((t (:foreground "#cb9401" :background "#1f1f27"))))
 `(minibuffer-prompt                ((t (:foreground "#a08563") :bold t)))
 `(show-paren-match                 ((t (:background "#e0741b" :foreground "#000000")))))

(deftheme handmadehero)
(custom-theme-set-faces
  'handmadehero
 `(default                          ((t (:foreground "burlywood2" :background "#161616"))))
 `(hl-line                          ((t (:background "midnight blue"))))
 `(vertico-current                  ((t (:background "midnight blue"))))
 `(region                           ((t (:background "medium blue"))))
 `(cursor                           ((t (:background "#40FF40"))))
 `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod2"))))
 `(font-lock-type-face              ((t (:foreground "burlywood3"))))
 `(font-lock-constant-face          ((t (:foreground "olive drab"))))
 `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
 `(font-lock-builtin-face           ((t (:foreground "gray80"))))
 `(font-lock-string-face            ((t (:foreground "olive drab"))))
 `(font-lock-comment-face           ((t (:foreground "gray50"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
 `(font-lock-doc-face               ((t (:foreground "gray50"))))
 `(font-lock-function-name-face     ((t (:foreground "burlywood2"))))
 `(font-lock-doc-string-face        ((t (:foreground "gray50"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))

(defadvice load-theme (before disable-themes-first (THEME &rest args) activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (when (custom-theme-p THEME)
      (enable-theme THEME)))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(setq custom-safe-themes t)
(load-theme 'handmadehero)
;; (load-theme 'ef-light)

;; C/C++ code style
(use-package cc-vars
  :init
  (setq-default c-default-style "linux" c-basic-offset 4))

;; Autocomplete using Emacs internal completion system.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))


(use-package xref
  :bind
  (
   ("M-a"   . 'xref-find-apropos)
   ("M-."   . 'xref-find-definitions)
   ("C-M-." . 'xref-find-references)
   ("M-,"   . 'xref-go-back)))

(use-package go-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package php-mode :ensure t)
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)

(use-package eglot
  :ensure t
  :defer t
  :bind
  (
   :map eglot-mode-map
	("C-c C-s" . 'eglot-symbols)
	("C-c C-r" . 'eglot-rename)
	("M-l"     . 'eglot-organize-imports-format)
	("C-c C-c" . 'eglot-code-actions))
  :init
  (dolist (mode '(go rust php)) (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities '(:hoverProvider
					  :documentHighlightProvider
					  :codeLensProvider
					  :documentOnTypeFormattingProvider
					  :documentLinkProvider
					  :colorProvider
					  :foldingRangeProvider
					  :executeCommandProvider
					  :inlayHintProvider))
  (with-eval-after-load
      'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio"))))
  (defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
  (setq eglot-stay-out-of '(project))
  (setq eglot-sync-connect nil) ;; no blocking on waiting for the server to start.
  (setq eglot-events-buffer-size 0) ;; no logging of LSP events.
  (defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports)))


(use-package consult-eglot :ensure t :after consult)


;; *Project aware commands*
(defun find-root () "Try to find project root based on deterministic predicates"
  (cond
   ((eq major-mode 'go-mode)                                (locate-dominating-file default-directory "go.mod"))
   (t                                                       (locate-dominating-file default-directory ".git"))))

(defun git-repo-p (DIR) (locate-dominating-file DIR ".git"))
(defun find-root-or-default-directory () (or (find-root) default-directory))

(use-package compile
  :commands (run-git-diff run-compile)
  :bind
  (
   ("M-m" . run-compile)
   ("M-g" . run-git-diff)
   :map compilation-mode-map
   ("<f5>". 'recompile)
   ("M-m" . 'previous-buffer)
   ("G"   . 'compile-in-directory)
   ("n"   . 'next-line)
   ("p"   . 'previous-line)
   ("M-n" . 'jump-down)
   ("M-p" . 'jump-up)
   ("k"   . 'kill-compilation))
   
  :config
  (defun amirreza-compile-buffer-name-function (MODESTR) (let ((dir (find-root-or-default-directory))) (format "*%s-%s*" MODESTR dir)))
  (setq-default compilation-buffer-name-function 'amirreza-compile-buffer-name-function)
  (defun run-git-diff () "run git diff command in `find-root` result or C-u to choose directory interactively." (interactive)
	 (if (null current-prefix-arg)
	     (setq --git-diff-dir (find-root-or-default-directory))
	   (setq --git-diff-dir (read-directory-name "Directory: " default-directory)))

	 (let ((default-directory --git-diff-dir))
	   (compilation-start "git diff HEAD" 'diff-mode)))

  (defun run-compile () "run `compile`." (interactive)
	 (if (null current-prefix-arg)
	     (setq --compile-dir (find-root-or-default-directory))
	   (setq --compile-dir (read-directory-name "Directory: " default-directory)))

	 (let* ((default-directory --compile-dir)
		(command (read-shell-command "Command: "  (cond ;; guess a command based on the context.
							   ((file-exists-p "build.bat") "build.bat")
							   ((file-exists-p "go.mod")    "go build -v ./...")
							   ((file-exists-p "Makefile")  "make")))))
	   (compilation-start command))))



(use-package grep
  :commands (run-grep)
  :bind
  (
   ("C-S-o" . 'run-grep)
   ("M-s"   . 'run-grep)
   :map grep-mode-map
   ("C-c C-p" . 'wgrep-toggle-readonly-area)
   ("<f5>"    . 'recompile)
   ("g"       . 'recompile)
   ("G"       . 'grep-in-directory)
   ("M-q"     . 'previous-buffer)
   ("M-n"     . 'jump-down)
   ("M-p"     . 'jump-up)
   ("k"       . 'kill-compilation))
  
  :config
  (with-eval-after-load 'grep
    (when (executable-find "rg")
      (grep-apply-setting 'grep-command "rg --no-heading --color='never'")
      (grep-apply-setting 'grep-use-null-device nil)))
  (defun run-grep () "Recursive grep in `find-root` result or C-u to choose directory interactively." (interactive)
	 (if (null current-prefix-arg)
	     (setq --grep-dir (find-root-or-default-directory))
	   (setq --grep-dir (read-directory-name "Directory: " default-directory)))

	 (let ((default-directory --grep-dir))
	   (cond
	    ;; when we have consult installed.
	    ((and (fboundp 'consult-ripgrep) (executable-find "rg"))  (consult-ripgrep --grep-dir))
	    ((and (fboundp 'consult-grep)    (executable-find "grep"))  (consult-grep --grep-dir))
	    ;; legacy approach.
	    ((executable-find "rg") (grep (format "rg --no-heading --color='never' %s" (read-string "Ripgrep: "))))
	    ((git-repo-p DIR)       (grep (format "git grep --no-color -n %s" (read-string "Git Grep: "))))
	    (t                      (grep (format "grep --color=auto -R -nH -e %s ." (read-string "Grep: "))))))))


(use-package eshell
  :commands (eshell-dwim)
  :config
  (setq eshell-visual-subcommands '("git" "diff" "log" "show"))
  (defun eshell-dwim () "Jump to eshell buffer associated with current project or create a new." (interactive)
	 (let* ((root (find-root-or-default-directory))
		(default-directory root)
		(eshell-buffer-name (format "*eshell-%s*" root)))
	   (if (get-buffer eshell-buffer-name)
	       (switch-to-buffer eshell-buffer-name)
	     (eshell)))))

(use-package vterm
  :ensure t
  :commands (vterm-dwim)
  :if (if is-windows nil t)
  :init
  (add-hook 'vterm-mode-hook (lambda ()
			       (hl-line-mode -1)))
  (defun vterm-dwim ()
    "Jump to vterm buffer associated with current project or create a new."
    (interactive)
    (let* ((root (find-root-or-default-directory))
	   (default-directory root)
	   (buffer-name (format "*vterm-%s*" root)))
      (if (get-buffer buffer-name)
	  (switch-to-buffer buffer-name)
	(vterm buffer-name)))))



(defun file-finder () "Recursive file find starting from `find-root` result or C-u to choose directory interactively." (interactive)
  (if (null current-prefix-arg)
      (setq --open-file-dir (find-root-or-default-directory))
    (setq --open-file-dir (read-directory-name "Directory: " default-directory)))
  
  (cond
   ((executable-find "rg") (let* ((default-directory --open-file-dir)
				  (command (format "rg --files"))
				  (file (completing-read "Ripgrep Files: " (string-split (shell-command-to-string command) "\n" t) nil t)))
			     (find-file file)))
   
   ((git-repo-p --open-file-dir) (let*
				     ((default-directory --open-file-dir)
				      (command (format "git ls-files"))
				      (file (completing-read "Git Files: " (string-split (shell-command-to-string command) "\n" t))))
				   (find-file file)))
   (t (error "you don't have rg installed and it's not a git repo."))))

(use-package replace
  :bind
  (("C-r"   . 'replace-string)
   ("M-r"   . 'query-replace)
   ("C-M-r" . 'replace-regexp)            
   :map query-replace-map
   ("<return>" . 'act)))

;; Keybindings
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-o")                                          'file-finder)
(global-set-key (kbd "M-o")                                          'file-finder)
(global-set-key (kbd "C-x i")                                        'edit-init)

(when (package-installed-p 'embark)
  (with-eval-after-load 'minibuffer
    (define-key minibuffer-mode-map (kbd "C-q")                      'embark-export)))

(if (package-installed-p 'vterm) (global-set-key (kbd "C-M-j")       'vterm-dwim))
  (global-set-key (kbd "M-j")                                        'eshell-dwim)

(add-hook 'eshell-mode-hook
  (lambda () (define-key eshell-mode-map (kbd "M-j")                 'previous-buffer)))

(add-hook 'vterm-mode-hook
  (lambda () (define-key vterm-mode-map (kbd "M-j")                  'previous-buffer)))

(global-set-key (kbd "C-.")                                          'next-buffer)
(global-set-key (kbd "C-,")                                          'previous-buffer)

(global-set-key (kbd "C-w")                                          'cut)              
(global-set-key (kbd "M-w")                                          'copy)             
(global-set-key (kbd "M-[")                                          'kmacro-start-macro)       
(global-set-key (kbd "M-]")                                          'kmacro-end-or-call-macro) 
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro)
(global-set-key (kbd "C-z")                                          'undo)

(global-set-key (kbd "C-SPC")                                        'set-mark-command)


(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-i")                                          'imenu)
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "C-;")                                          'goto-line)
(global-set-key (kbd "C-0")                                          'delete-window)
(global-set-key (kbd "C-1")                                          'delete-other-windows)
(global-set-key (kbd "C-3")                                          'split-window-right)
(global-set-key (kbd "C-2")                                          'split-window-below)
(global-set-key (kbd "C-j")                                          'completion-at-point)
(global-set-key (kbd "M-g")                                          'run-git-diff)


(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-;")                           'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-'")                           'flymake-goto-next-error))

(global-set-key (kbd "C-x SPC")                                      'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))

