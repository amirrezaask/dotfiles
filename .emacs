(when load-file-name
  (setq BASE_PATH (file-name-directory load-file-name)) ;; Store this file location.
  (setq INIT_FILE load-file-name))

(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 8)
		  gc-cons-percentage 0.1)))

(setq amirreza/emacs-starting-time (float-time)) ;; Store current time for further analysis.

(setq frame-inhibit-implied-resize t     ;; Don't let emacs to resize frame when something inside changes
      initial-scratch-message ""         ;; No starting text in *scratch* buffer.
      gc-cons-threshold (* 1024 1024 10) ;; Default emacs garbage collection threshold is 800KB which is low for today standards, memory is cheap, so we make a bit higher, remember if you set it to high it would cause major pauses.
      gc-cons-percentage 0.5
      redisplay-dont-pause t
      vc-follow-symlinks t               ;; Follow symlinks with no questions
      ring-bell-function (lambda ())     ;; no stupid sounds
      custom-file "~/.custom.el"         ;; set custom file to not meddle with init.el
      make-backup-files nil              ;; no emacs ~ backup files
      is-windows (eq system-type 'windows-nt)                                         
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29)
      use-short-answers t                   ;; Always prefer short answers
      image-types (cons 'svg image-types)   ;; macos issue.
      mac-command-modifier 'meta            ;; macos issue.
      recenter-positions '(middle)
      inhibit-startup-screen t              ;; disable default start screen
      )
(set-default-coding-systems 'utf-8) ;; always use UTF8
(set-frame-parameter nil 'fullscreen 'maximized) ;; Start emacs in maximized state.
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized

(setq-default frame-title-format '("%e" (:eval (format "%s @ %s" default-directory system-name)))) ;; OS window title
(unless (executable-find "rg") (error "Install ripgrep, this configuration relies heavy on it's features."))
(global-unset-key (kbd "C-x C-c"))

(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar

;; Font Stuff
(setq font-family "")
(defun amirreza/set-font (font fontsize)
  "Loads a font."
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defalias 'Font 'amirreza/set-font)

(defun amirreza/set-font-size (fontsize)
  "Set a font size"
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defalias 'FontSize 'amirreza/set-font-size)

(defun amirreza/text-scale-increase ()
  (interactive)
  (text-scale-increase 1))

(defun amirreza/text-scale-decrease ()
  (interactive)
  (text-scale-decrease 1))

(amirreza/set-font "Iosevka" 13)

(global-set-key (kbd "C-=")  'amirreza/text-scale-increase)
(global-set-key (kbd "C--")  'amirreza/text-scale-decrease)

;; Environment Variables
(defun home (path)
  (expand-file-name path (getenv "HOME")))

(unless is-windows
  (add-to-list 'exec-path (home ".local/bin"))
  (add-to-list 'exec-path "/usr/local/go/bin")
  (add-to-list 'exec-path (home ".cargo/bin"))
  (add-to-list 'exec-path "/opt/homebrew/bin"))

(add-to-list 'exec-path (home "bin"))
(when is-windows
      (add-to-list 'exec-path "w:/bin")
      (add-to-list 'exec-path "c:/programs/bin"))

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(defun install (PKG &optional DOC)
  (if (listp PKG)
      (unless (package-installed-p (car PKG))
	(if (fboundp 'package-vc-install)
	    (package-vc-install PKG)
	(warn "package-vc-install is available from Emacs 29, ignoring this install statement.")))
      (unless (package-installed-p PKG)
	(package-install PKG))))

(install 'use-package)

;; @Section Editing
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode) ;; when selected a text and user types delete text

(install 'so-long "So emacs can handle long lines :))")
(global-so-long-mode +1)
(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))

(install 'vlf "Special handling of very large files")
(require 'vlf-setup)

(setq dabbrev-upcase-means-case-search t
      dabbrev-case-replace nil
      dabbrev-case-fold-search t
      dabbrev-upcase-means-case-search nil)

(defun amirreza/copy ()
  "Either copy region or the current line."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
    (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun amirreza/cut ()
  "Either cut region or the current line."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)) ;; copy active region contents
    (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line

(global-set-key (kbd "C-w")                                          'amirreza/cut)
(global-set-key (kbd "M-w")                                          'amirreza/copy)
(global-set-key (kbd "M-y")                                          'consult-yank-pop)
(global-set-key (kbd "M-[")                                          'kmacro-start-macro)
(global-set-key (kbd "M-]")                                          'kmacro-end-or-call-macro)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))
(global-set-key (kbd "C-<return>")                                   'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "C-q")                                          'dabbrev-expand) ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "C-j")                                          'completion-at-point) ;; Manual trigger for completion popup.
(global-set-key (kbd "C-z")                                          'undo) ;; Sane undo key
(global-set-key (kbd "M-0")                                          'query-replace) ;; Replace pattern with a string
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "M-SPC")                                        'rectangle-mark-mode)


;; @Section Themes
(install 'doom-themes)
(install 'gruber-darker-theme)
(defvar amirreza/--themes '())
(defmacro amirreza/deftheme (NAME DOC)
  `(progn
     (deftheme ,NAME ,DOC)
     (add-to-list 'amirreza/--themes (quote ,NAME))))

(amirreza/deftheme Naysayer "Inspired by Jonathan Blow (naysayer).")
(custom-theme-set-faces
 'Naysayer
 `(default                          ((t (:foreground "#d3b58d" :background "#072629"))))
 `(hl-line                          ((t (:background "#0c4141"))))
 `(vertico-current                  ((t (:background "#0c4141"))))
 `(region                           ((t (:background  "medium blue"))))
 `(cursor                           ((t (:background "lightgreen"))))
 `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "white"))))
 `(font-lock-string-face            ((t (:foreground "#0fdfaf"))))
 `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
 `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))

(amirreza/deftheme Dirt "Brown theme inspired by Jonathan Blow (naysayer)")
(custom-theme-set-faces
 'Dirt
 `(default                          ((t (:foreground "#debe95" :background "#161616"))))
 `(hl-line                          ((t (:background "#252525"))))
 `(vertico-current                  ((t (:background "#252525"))))
 `(region                           ((t (:background  "medium blue"))))
 `(cursor                           ((t (:background "green"))))
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

(amirreza/deftheme Handmadehero "Theme from popular handmadehero.")
(custom-theme-set-faces
 'Handmadehero
 `(default                          ((t (:foreground "#cdaa7d" :background "#161616"))))
 `(cursor                           ((t (:background "green"))))
 `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod3"))))
 `(font-lock-type-face              ((t (:foreground "burlywood3"))))
 `(font-lock-constant-face          ((t (:foreground "#olive drab"))))
 `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
 `(font-lock-builtin-face           ((t (:foreground "#DAB98F"))))
 `(font-lock-string-face            ((t (:foreground "olive drab"))))
 `(font-lock-comment-face           ((t (:foreground "gray50"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
 `(font-lock-doc-face               ((t (:foreground "gray50"))))
 `(font-lock-function-name-face     ((t (:foreground "burlywood3"))))
 `(font-lock-doc-string-face        ((t (:foreground "olive drab"))))
 `(font-lock-preprocessor-face      ((t (:foreground "#8cde94"))))
 `(font-lock-warning-face           ((t (:foreground "#504038"))))
 `(region                           ((t (:background "medium blue"))))
 `(hl-line                          ((t (:background "midnight blue"))))
 `(vertico-current                  ((t (:background "midnight blue"))))
 `(mode-line                        ((t (:background "#ffffff" :foreground "#000000"))))
 `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
 `(show-paren-match                 ((t (:background "burlywood3" :foreground "black"))))
 `(highlight                        ((t (:foreground nil :background "medium blue")))))

(amirreza/deftheme 4coder-fleury "Theme from 4coder setup of ryan fleury")
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
 `(vertico-current                  ((t (:background "#171616"))))
 `(highlight                        ((t (:foreground nil :background "#2f2f37"))))
 `(mode-line                        ((t (:foreground "#cb9401" :background "#1f1f27"))))
 `(mode-line-inactive               ((t (:foreground "#cb9401" :background "#1f1f27"))))
 `(minibuffer-prompt                ((t (:foreground "#a08563") :bold t)))
 `(show-paren-match                 ((t (:background "#e0741b" :foreground "#000000")))))

(defun amirreza/set-theme (NAME)
  (interactive (list (intern (completing-read "Theme: " (append (mapcar #'symbol-name amirreza/--themes) (mapcar #'symbol-name (custom-available-themes)))))))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if (member NAME amirreza/--themes)
      (enable-theme NAME)
    (load-theme NAME t)))

(defalias 'Theme 'amirreza/set-theme)
(amirreza/set-theme 'modus-vivendi)

;; @Section: Minibuffer enhancement
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 10
	vertico-cycle t)
  (vertico-mode +1))

(use-package consult :ensure t)

;; @Section Xref stuff.
(use-package dumb-jump :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package xref
  :bind
  (("<f12>" . xref-find-definitions)
   ("C-<f12>" . xref-find-references)))

;; @Section Dired
(use-package dired
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (defun amirreza/side-tree ()
    (interactive)
    (let* ((dir (find-project-root-or-default-directory))
	   (dired-buffer (dired-noselect dir)))
      (select-window (display-buffer-in-side-window dired-buffer '((side . left)
						    (slot . 0)
						    (window-width . 0.2)
						    (window-parameters . ((no-delete-other-window . t)))
						    )))
      (with-current-buffer dired-buffer
	(rename-buffer (format "*Dired-%s*" dir))
	(dired-hide-details-mode +1))))
  :bind
  ((:map global-map
	 ("C-0" . amirreza/side-tree))
   :map dired-mode-map
   ("C-0" . 'kill-current-buffer)))

;; @Section Modeline
(setq-default mode-line-format '("%e"
				 mode-line-front-space
				 mode-line-modified
				 mode-line-remote
				 " "
				 (:eval (or (buffer-file-name) (buffer-name)))
				 "    "
				 mode-line-percent-position
				 " "
				 "(%l, %C)"
				 "    "
				 (vc-mode vc-mode)
				 " "
				 (text-scale-mode
				  (" " text-scale-mode-lighter))))
;; @Section Window stuff
(setq display-buffer-alist '(("\\*compile.*\\*"
			      (display-buffer-in-side-window)
			      (side . right)
			      (window-width . 0.4)
			      (slot . 0))

			     ("\\*(Help|Backtrace|Messages)\\*"
			      (display-buffer-in-side-window)
			      (side . right)
			      (window-width . 0.4)
			      (slot . 0))

			     ("\\*eshell.*\\*"
			      (display-buffer-in-side-window)
			      ((side . bottom)
			       (window-height . 0.25)
			       (slot . 0)))))

;; @Section Buffer stuff
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)
(global-set-key (kbd "C-;") 'consult-goto-line)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

;; @Section File stuff
(global-set-key (kbd "C-o") 'find-file) ;; open files
(global-set-key (kbd "M-o") 'rg-find-files) ;; Find files in project
(defun edit-init ()
  "Edit this file."
  (interactive)
  (find-file INIT_FILE))

(defalias 'Config 'edit-init)

(defun rg-find-files ()
  (interactive)
  (unless (executable-find "rg") (error "rg-find-files needs ripgrep."))
  (let* ((default-directory (or (find-project-root) default-directory))
	 (results (string-split (string-trim (shell-command-to-string "rg --files") "\n" "\n") "\n"))
	 (relfile (completing-read "Files: " results))
	 (absfile (expand-file-name relfile default-directory)))
    (find-file absfile)))

(global-set-key (kbd "<f1>") 'edit-init)

;; @Section ISearch
(global-set-key (kbd "C-.") 'isearch-forward-thing-at-point)

;; @Section Project based commands
(defun find-project-root ()
  "Try to find project root based on deterministic predicates"
  (cond
   ((eq major-mode 'go-mode)                                (locate-dominating-file default-directory "go.mod"))
   ((or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))  (locate-dominating-file default-directory "build.bat"))
   (t                                                       (locate-dominating-file default-directory ".git"))))

(defun git-repo-p ()
  (locate-dominating-file default-directory ".git"))

(defun find-project-root-or-default-directory ()
  (or (find-project-root) default-directory))

(defun amirreza/compile-buffer-name-function (MODE)
  (let ((dir (find-project-root-or-default-directory)))
    (format "*compile-%s*" dir)))

(setq-default compilation-buffer-name-function 'amirreza/compile-buffer-name-function)

;; @Section Compilation
(use-package compile
  :config
  (defun guess-compile-command (DIR)
    (let ((default-directory DIR))
      (cond
       ((file-exists-p "build.bat") "build.bat")
       ((file-exists-p "go.mod")    "go build -v "))))

  (setq amirreza/compile-history '())
  (setq amirreza/last-compile nil)

  (defun amirreza/compile ()
    "Compile in a directory"
    (interactive)
    (when amirreza/last-compile
      (unless (y-or-n-p "Use last compile values?") (setq amirreza/last-compile nil)))
    (let* ((default-directory (or (car amirreza/last-compile) (read-directory-name "[Compile] Directory: " (find-project-root-or-default-directory))))
	   (command (or (car (cdr amirreza/last-compile)) (read-shell-command "[Compile] Command: " (guess-compile-command default-directory) amirreza/compile-history))))
      (setq amirreza/last-compile `(,default-directory ,command))
      (compilation-start command)))

  (defalias 'Compile 'amirreza/compile)
  :bind
  ((:map compilation-mode-map
	 ("<f5>" . 'recompile)
	 ("<k>" . 'kill-compilation)
	 )
   (:map global-map
	 ("M-m" . 'amirreza/compile)
	 ("<f5>" . 'amirreza/compile))))


;; @Section Grep
(defun rg (dir pattern)
  "runs Ripgrep program in a compilation buffer."
  (interactive (list (read-directory-name "[Ripgrep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[Ripgrep] Pattern: " nil)))
  (unless (executable-find "rg") (error "ripgrep executable not found, install from https://github.com/BurntSushi/ripgrep/releases"))

  (let* ((default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[grep] Pattern: " nil)))
  (unless (executable-find "grep") (error "Gnu Grep executable not found"))
  (add-to-list 'amirreza/grep-query-history pattern)

  (let* (
	 (default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza/grep (dir pattern)
  ""
  (interactive (list (read-directory-name "[Grep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[Grep] Pattern: " nil)))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   (t (gnu-grep dir pattern))))

(defalias 'Grep 'amirreza/grep)

(defun amirreza/igrep ()
  ""
  (interactive)
  (unless (package-installed-p 'consult) (error "consult package is needed for this function."))
  (let ((dir (find-project-root-or-default-directory)))
    (cond
     ((or (executable-find "rg") is-windows) (consult-ripgrep dir ""))
     ((git-repo-p)                           (consult-git-grep dir ""))
     (t                                      (consult-grep dir "")))))

(defalias 'IGrep 'amirreza/igrep)

(global-set-key (kbd "M-j") 'amirreza/igrep)
(global-set-key (kbd "C-M-j") 'amirreza/grep)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))


;; @Section Programming
;;; @Golang
(install 'go-mode)
(defun amirreza/go-hook ()
  (interactive)
  (setq gofmt-args '("-s"))
  (setq gofmt-command "gofmt")
  (setq-local devdocs-current-docs '(go))
  (add-hook 'before-save-hook 'gofmt-before-save 0 t))
(add-hook 'go-mode-hook 'amirreza/go-hook)

;; @C/C++
(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++ code style

;; @Elisp
(defun toggle-debug-mode ()
  "Toggle Emacs debug mode." 
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t)))

;; Things that don't deserve a name :)
(install 'php-mode)
(install 'yaml-mode)
(install 'json-mode)
(install 'dockerfile-mode)

;; @Section Autocomplete
(install 'corfu)
(setq corfu-auto nil)
(global-corfu-mode +1)

;; @Section Eglot (LSP client)
(unless (>= emacs-major-version 29) (install 'eglot))
(setq eglot-ignored-server-capabilities '(
					  :hoverProvider
					  :documentHighlightProvider
					  :documentSymbolProvider
					  :workspaceSymbolProvider
					  :codeLensProvider
					  :documentFormattingProvider
					  :documentRangeFormattingProvider
					  :documentOnTypeFormattingProvider
					  :documentLinkProvider
					  :colorProvider
					  :foldingRangeProvider
					  :executeCommandProvider
					  ))
(setq eglot-stay-out-of '(flymake project))

(add-hook 'go-mode-hook #'eglot-ensure)

;; @Section EShell
(setq eshell-visual-subcommands '("git" "diff" "log" "show"))

(defun amirreza/eshell-hook ()
  (define-key eshell-mode-map (kbd "M-;") 'delete-window))

(add-hook 'eshell-mode-hook 'amirreza/eshell-hook)
(defun amirreza/eshell ()
  (interactive)
  (let* ((dir (find-project-root-or-default-directory))
	 (eshell-buffer-name (format "*eshell-%s*" dir))
	 (default-directory dir)
	 (existing-buffer (get-buffer eshell-buffer-name)))

    (if existing-buffer
	(select-window (display-buffer existing-buffer)) ;; NOTE: we have to use display-buffer so our display-buffer-alist configs are used.
      (eshell))))

(defalias 'Sh 'amirreza/eshell)
(defalias 'EShell 'amirreza/eshell)

(global-set-key (kbd "M-;") 'amirreza/eshell)

;; @Section Benchmark startup and report
(defvar amirreza/emacs-init-took (* (float-time (time-subtract (float-time) amirreza/emacs-starting-time)) 1000) "Time took to load my init file, value is in milliseconds.")
(defvar emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000) "Time took Emacs to boot, value is in milliseconds.")
(setq amirreza/emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza/emacs-init-took emacs-init-time-took))
(message amirreza/emacs-init-log-message)

