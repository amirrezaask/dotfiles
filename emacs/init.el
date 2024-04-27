;; Early-init.el
(setq amirreza/early-init "
(setq amirreza/emacs-starting-time (float-time)) ;; Store current time for further analysis.

(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 20) ;; 20 Megabytes
		  gc-cons-percentage 0.2)))

(setq package-enable-at-startup t)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t

      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      )
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

")

(unless (file-exists-p "~/.emacs.d/early-init.el")
  (write-region amirreza/early-init nil "~/.emacs.d/early-init.el"))

(setq native-comp-async-report-warnings-errors nil)

;; Frame
(setq-default frame-title-format '("%e" (:eval (format "Emacs @ %s" default-directory))))
(set-frame-parameter nil 'fullscreen 'maximized) 

;; Basic variables
(when load-file-name
  (setq INIT-FILE load-file-name))

(setq is-windows (eq system-type 'windows-nt)                                      
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

;; MacOS issues
(setq image-types (cons 'svg image-types)
      mac-command-modifier 'meta)

;; General Text Editing
(setq recenter-positions '(middle))
(setq custom-file "~/.custom.el"          ;; set custom file to not meddle with init.el
      make-backup-files nil)              ;; no emacs ~ backup files
(setq vc-follow-symlinks t)
(set-default-coding-systems 'utf-8) ;; always use UTF8
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode) ;; when selected a text and user types delete text
(global-display-line-numbers-mode +1) ;; Display line numbers.

(setq dabbrev-upcase-means-case-search t ;; dabbrev mode: emacs basic word completion based on current buffer words, using C-q, see keybindings section.
      dabbrev-case-replace nil
      dabbrev-case-fold-search t
      dabbrev-upcase-means-case-search nil)

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

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

(global-set-key (kbd "C-/")                                          'comment-line)
(global-set-key (kbd "C-w")                                          'amirreza/cut)
(global-set-key (kbd "M-w")                                          'amirreza/copy)
(global-set-key (kbd "M-[")                                          'kmacro-start-macro)
(global-set-key (kbd "M-]")                                          'kmacro-end-or-call-macro)
(global-set-key (kbd "C-q")                                          'dabbrev-expand) ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "C-z")                                          'undo) ;; Sane undo key
(global-set-key (kbd "C-0")                                          'delete-window)
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "C-<up>")                                       'jump-up)
(global-set-key (kbd "C-<down>")                                     'jump-down)
(global-set-key (kbd "C-;")                                          'goto-line)
;; Rectangle Mode (Almost multi cursors)
(global-set-key (kbd "M-SPC")                                        'rectangle-mark-mode)
(global-set-key (kbd "C-<return>")                                   'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))

(unless (executable-find "rg") (error "Install ripgrep, this configuration relies heavy on it's features."))

(global-unset-key (kbd "C-x C-c"))

;; Window related settings
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-\\") 'split-window-horizontally)

;; Font
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
(defalias 'load-font 'amirreza/set-font)

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

;; (amirreza/set-font "" 15)

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
(add-to-list 'exec-path (home "go/bin"))

(when is-windows
  (add-to-list 'exec-path "w:/bin")
  (add-to-list 'exec-path "c:/programs/bin"))

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH


;; Package manager
(require 'package)
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu-elpa" . 2)
	("melpa" . 1)
	("nongnu" . 0)))

(package-initialize)
(defun install (PKG &optional DOC)
  (if (listp PKG)
      (unless (package-installed-p (car PKG))
	(if (fboundp 'package-vc-install)
	    (package-vc-install PKG)
	  (warn "package-vc-install is available from Emacs 29, ignoring this install statement.")))
    (unless (package-installed-p PKG)
      (package-install PKG))))


;; Let emacs handle large files and lines.
(install 'so-long "So emacs can handle long lines :))")
(global-so-long-mode +1)
(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))

(install 'vlf "Special handling of very large files")
(require 'vlf-setup)


(global-hl-line-mode -1)
;; Dirt Theme (default)
;; (custom-set-faces
;;  `(default                          ((t (:foreground "#debe95" :background "#161616"))))
;;  `(hl-line                          ((t (:background "#252525"))))
;;  `(vertico-current                  ((t (:background "#252525"))))
;;  `(region                           ((t (:background "medium blue"))))
;;  `(cursor                           ((t (:background "lightgreen"))))
;;  `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
;;  `(font-lock-type-face              ((t (:foreground "#8cde94"))))
;;  `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
;;  `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
;;  `(font-lock-builtin-face           ((t (:foreground "white"))))
;;  `(font-lock-string-face            ((t (:foreground "gray70"))))
;;  `(font-lock-comment-face           ((t (:foreground "yellow"))))
;;  `(font-lock-comment-delimiter-face ((t (:foreground "yellow"))))
;;  `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
;;  `(font-lock-function-name-face     ((t (:foreground "white"))))
;;  `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
;;  `(font-lock-warning-face           ((t (:foreground "yellow"))))
;;  `(font-lock-note-face              ((t (:foreground "khaki2" ))))
;;  `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
;;  `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
;;  `(show-paren-match                 ((t (:background "mediumseagreen")))))

;; Naysayer Theme
(custom-set-faces
 `(default                          ((t (:foreground "#d3b58d" :background "#072629"))))
 `(hl-line                          ((t (:background "#0c4141"))))
 `(vertico-current                  ((t (:inherit hl-line))))
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

;; Black Theme
;; (custom-set-faces
;;  `(default                          ((t (:foreground "grey89" :background "grey0"))))
;;  `(cursor                           ((t (:background "grey99"))))
;;  `(font-lock-keyword-face           ((t (:foreground "cyan3"))))
;;  `(font-lock-type-face              ((t (:foreground "lightblue3"))))
;;  `(font-lock-variable-name-face     ((t (:foreground "grey89"))))
;;  `(font-lock-string-face            ((t (:foreground "lightgreen"))))
;;  `(font-lock-comment-face           ((t (:foreground "grey50"))))
;;  `(font-lock-comment-delimiter-face ((t (:foreground "grey50"))))
;;  `(font-lock-doc-face               ((t (:foreground "grey50"))))
;;  `(font-lock-function-name-face     ((t (:foreground "lightblue2"))))
;;  `(font-lock-doc-string-face        ((t (:foreground "grey50"))))
;;  `(region                           ((t (:background "grey23"))))
;;  `(hl-line                          ((t (:background "grey10"))))
;;  `(vertico-current                  ((t (:background "grey10"))))
;;  `(mode-line                        ((t (:background "grey10" :foreground "grey89" :box t))))
;;  `(mode-line-inactive               ((t (:background "grey3" :foreground "grey89" :box t))))
;;  `(highlight                        ((t (:foreground nil     :background "cyan")))))

;; Minibuffer
;; (with-eval-after-load 'minibuffer
;;   (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
;;   (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion))

;; Dumb way to find things
(install 'dumb-jump "Poor's man Jump to def/dec/ref. (using grep)")
(add-hook 'xref-backend-functions        #'dumb-jump-xref-activate)
(global-set-key (kbd "<f12>")            'xref-find-definitions)
(global-set-key (kbd "C-<f12>")          'xref-find-references)
(global-set-key (kbd "C-<down-mouse-1>") 'xref-find-definitions)
(global-set-key (kbd "M-<down-mouse-1>") 'xref-find-references)
(global-set-key (kbd "C-.")              'xref-find-definitions)
(global-set-key (kbd "C-,")              'xref-go-back)

;; Buffer
(setq display-buffer-alist '(("\\*compilation.*\\*"
			      (display-buffer-same-window))
			     
			     ("\\*grep.*\\*"
			      (display-buffer-same-window))
			     
			     ("\\*(Help|Backtrace|Messages)\\*"
			      (display-buffer-in-side-window)
			      (side . right)
			      (window-width . 0.4)
			      (slot . 0))

			     ("\\*eshell.*\\*"
			      (display-buffer-same-window))

			     ("\\*journalsync\\*"
			      (display-buffer-no-window))))

;; Project based commands
;; I have 3 functionalities that I want per-project and by project I mean each directory that we can find a root for, look into `find-project-root` function.
;; These functionalities are `Grep`, `Compile`, `EShell`, I want a buffer for each one per project and by just pressing a key I either create one interactively or jump to it.
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

;; TOOD: Switch to a buffer in project

;; Compile
(defun amirreza/compile-buffer-name-function (MODESTR)
  (let ((dir (find-project-root-or-default-directory)))
    (format "*%s-%s*" MODESTR dir)))

(setq-default compilation-buffer-name-function 'amirreza/compile-buffer-name-function)

(defun guess-compile-command (DIR)
  (let ((default-directory DIR))
    (cond
     ((file-exists-p "build.bat") "build.bat")
     ((file-exists-p "go.mod")    "go build -v "))))

(defun amirreza/compile-in-directory ()
  "Compile in a directory"
  (interactive)
  (let* ((default-directory (read-directory-name "[Compile] Directory: " (find-project-root-or-default-directory)))
	 (command (read-shell-command "[Compile] Command: " (guess-compile-command default-directory))))
    (setq amirreza/last-compile `(,default-directory ,command))
    (compilation-start command)))

(defun amirreza/switch-to-compile-buffer-or-compile-in-directory ()
  "If there is already a compilation buffer for current project switch to it otherwise run `amirreza/compile-in-directory`."
  (interactive)
  (let ((existing-buffer (get-buffer (amirreza/compile-buffer-name-function 'compilation)))) ;; check if we already have a buffer just switch to it, otherwise ask for options.
    (if existing-buffer
	(switch-to-buffer existing-buffer)
      (amirreza/compile-in-directory))))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "M-m")  'previous-buffer)
  (define-key compilation-mode-map (kbd "G")    'amirreza/compile-in-directory)
  (define-key compilation-mode-map (kbd "n")    'next-line)
  (define-key compilation-mode-map (kbd "p")    'previous-line)
  (define-key compilation-mode-map (kbd "k")    'kill-compilation))

(global-set-key (kbd "M-m") 'amirreza/switch-to-compile-buffer-or-compile-in-directory)
(global-set-key (kbd "<f5>") 'amirreza/switch-to-compile-buffer-or-compile-in-directory)

;; Grep + Ripgrep stuff.
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

  (let* ((default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza/grep-in-directory ()
  "Run appropriate grep program in directory."
  (interactive)
  (let ((dir (read-directory-name "[Grep] Directory: " (find-project-root-or-default-directory)))
	(pattern (read-string "[Grep] Pattern: " nil)))
    
    (cond
     ((or (executable-find "rg") is-windows) (rg dir pattern))
     (t (gnu-grep dir pattern)))))
  

(defun amirreza/switch-to-grep-buffer-or-grep-in-directory ()
  "If there is already a grep buffer for current project switch to it otherwise run `amirreza/grep-in-directory`."
  (interactive)
  (let ((existing-buffer (get-buffer (amirreza/compile-buffer-name-function 'grep)))) ;; check if we already have a buffer just switch to it, otherwise ask for options.
    (if existing-buffer
	(switch-to-buffer existing-buffer)
      (amirreza/grep-in-directory))))
  
(global-set-key (kbd "M-j") 'amirreza/switch-to-grep-buffer-or-grep-in-directory)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "g")    'recompile)
  (define-key grep-mode-map (kbd "G")    'amirreza/grep-in-directory)
  (define-key grep-mode-map (kbd "M-j")  'previous-buffer)
  (define-key grep-mode-map (kbd "k")    'kill-compilation))

;; Eshell: Emacs cross platform shell
(setq eshell-visual-subcommands '("git" "diff" "log" "show"))

(defun amirreza/eshell-hook ()
  (define-key eshell-mode-map (kbd "M-;") 'previous-buffer))

(add-hook 'eshell-mode-hook 'amirreza/eshell-hook)
(defun amirreza/eshell ()
  "If there is already an eshell buffer for current project jump to it, otherwise create a new one for current project."
  (interactive)
  (let* ((dir (find-project-root-or-default-directory))
	 (eshell-buffer-name (format "*eshell-%s*" dir))
	 (default-directory dir)
	 (existing-buffer (get-buffer eshell-buffer-name)))

    (if existing-buffer
	(switch-to-buffer existing-buffer) ;; we switch to buffer.
      (eshell))))

(defalias 'EShell 'amirreza/eshell)

(global-set-key (kbd "M-;") 'amirreza/eshell)

;; Golang
(install 'go-mode)

(defun amirreza/go-hook ()
  (interactive)
  (add-hook 'before-save-hook 'gofmt-before-save 0 t))

(add-hook 'go-mode-hook 'amirreza/go-hook)

;; Rust
;; $ rustup component add rust-analyzer
(install 'rust-mode)

;; C/C++
(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++ code style

;; Elisp
(defun toggle-debug-mode ()
  "Toggle Emacs debug mode." 
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t)))

(defun amirreza/edit-init ()
  (interactive)
  (find-file INIT-FILE))

(global-set-key (kbd "C-x i") 'amirreza/edit-init)

;; Autocomplete
(install 'corfu)
(setq corfu-auto nil)
(global-corfu-mode +1)
(global-set-key (kbd "C-j") 'completion-at-point) ;; Manual trigger for completion popup.


;; Eglot: LSP
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
					  :inlayHintProvider
					  ))
(setq eglot-stay-out-of '(flymake project))
(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

;; Magit
(install 'magit)

;;; Final benchmarking
(defvar amirreza/emacs-init-took (* (float-time (time-subtract (float-time) amirreza/emacs-starting-time)) 1000) "Time took to load my init file, value is in milliseconds.")
(defvar emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000) "Time took Emacs to boot, value is in milliseconds.")
(setq amirreza/emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza/emacs-init-took emacs-init-time-took))
(message amirreza/emacs-init-log-message)

