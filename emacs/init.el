(setq amirreza/early-init "
(setq amirreza/emacs-starting-time (float-time)) ;; Store current time for further analysis.

(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 200) ;; 200 Megabytes
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
(setq debug-on-error t)
(unless (file-exists-p "~/.emacs.d/early-init.el")
  (write-region amirreza/early-init nil "~/.emacs.d/early-init.el"))

(setq native-comp-async-report-warnings-errors nil)

(set-frame-parameter nil 'fullscreen 'maximized) 

;; Emacs: Basic
(when load-file-name
  (setq INIT-FILE load-file-name))

(setq is-windows (eq system-type 'windows-nt)                                      
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

;; Emacs: fix macOS issues
(setq image-types (cons 'svg image-types)
      mac-command-modifier 'meta)

;; Emacs: Editing
(setq recenter-positions '(middle))
(setq custom-file "~/.custom.el"          ;; set custom file to not meddle with init.el
      make-backup-files nil)              ;; no emacs ~ backup files
(setq vc-follow-symlinks t)
(set-default-coding-systems 'utf-8) ;; always use UTF8
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode) ;; when selected a text and user types delete text
(global-display-line-numbers-mode +1) ;; Display line numbers.
(toggle-truncate-lines +1)

(setq dabbrev-upcase-means-case-search t ;; dabbrev mode: emacs basic word completion based on current buffer words, using C-q, see keybindings section.
      dabbrev-case-replace nil
      dabbrev-case-fold-search t
      dabbrev-upcase-means-case-search nil)

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

(global-unset-key (kbd "C-x C-c"))

;; Emacs: Font
(setq font-family "")
(defun set-font (font fontsize)
  "Loads a font."
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defalias 'Font 'set-font)
(defalias 'load-font 'set-font)

(defun set-font-size (fontsize)
  "Set a font size"
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defalias 'FontSize 'set-font-size)

(defun font-exists-p (font) "check if font exists" (if (null (x-list-fonts font)) nil t))

;; Load fonts
(when (font-exists-p "Cascadia Code") (set-font "Cascadia Code" 15))
(when (font-exists-p "Hack") (set-font "Hack" 15))
(when (font-exists-p "Jetbrains Mono") (set-font "Jetbrains Mono" 15))
(when (font-exists-p "Consolas") (set-font "Consolas" 15))
(when (font-exists-p "Menlo") (set-font "Menlo" 15))

(defun font-zoom-in ()
  (interactive)
  (text-scale-increase 1))

(defun font-zoom-out ()
  (interactive)
  (text-scale-decrease 1))


(global-hl-line-mode +1)

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


(unless (executable-find "rg") (warn "Install ripgrep, this configuration relies heavy on it's features."))

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


;; Multiple Cursors
(install 'multiple-cursors)


(install 'vlf "Special handling of very large files")
(require 'vlf-setup)

;; Minibuffer
(install 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(with-eval-after-load 'minibuffer ;; This way we don't need a seperate package for completion
  (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion))

;; (install 'vertico)
;; (vertico-mode +1)

;; Consult + Embark
;; (install 'consult)
;; (install 'embark)
;; (install 'embark-consult)


;; Dirt Theme (default)
;; (custom-set-faces
;;   `(default                          ((t (:foreground "#debe95" :background "#202020"))))
;;   `(hl-line                          ((t (:background "#353535"))))
;;   `(vertico-current                  ((t (:background "#353535"))))
;;   `(region                           ((t (:background "medium blue"))))
;;   `(cursor                           ((t (:background "lightgreen"))))
;;   `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
;;   `(font-lock-type-face              ((t (:foreground "#8cde94"))))
;;   `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
;;   `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
;;   `(font-lock-builtin-face           ((t (:foreground "white"))))
;;   `(font-lock-string-face            ((t (:foreground "gray70"))))
;;   `(font-lock-comment-face           ((t (:foreground "yellow"))))
;;   `(font-lock-comment-delimiter-face ((t (:foreground "yellow"))))
;;   `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
;;   `(font-lock-function-name-face     ((t (:foreground "white"))))
;;   `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
;;   `(font-lock-warning-face           ((t (:foreground "yellow"))))
;;   `(font-lock-note-face              ((t (:foreground "khaki2" ))))
;;   `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
;;   `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
;;   `(show-paren-match                 ((t (:background "mediumseagreen")))))

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

;; HandmadeHero
;; (custom-set-faces
;;  `(default                          ((t (:foreground "burlywood3" :background "#161616"))))
;;  `(hl-line                          ((t (:background "midnight blue"))))
;;  `(vertico-current                  ((t (:inherit    hl-line))))
;;  `(region                           ((t (:background "medium blue"))))
;;  `(cursor                           ((t (:background "#40FF40"))))
;;  `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod3"))))
;;  `(font-lock-type-face              ((t (:foreground "burlywood3"))))
;;  `(font-lock-constant-face          ((t (:foreground "olive drab"))))
;;  `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
;;  `(font-lock-builtin-face           ((t (:foreground "white"))))
;;  `(font-lock-string-face            ((t (:foreground "olive drab"))))
;;  `(font-lock-comment-face           ((t (:foreground "gray50"))))
;;  `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
;;  `(font-lock-doc-face               ((t (:foreground "gray50"))))
;;  `(font-lock-function-name-face     ((t (:foreground "burlywood3"))))
;;  `(font-lock-doc-string-face        ((t (:foreground "gray50"))))
;;  `(font-lock-warning-face           ((t (:foreground "yellow"))))
;;  `(font-lock-note-face              ((t (:foreground "khaki2" ))))
;;  `(show-paren-match                 ((t (:background "mediumseagreen")))))

;; Golang
;; $ go install golang.org/x/tools/gopls@latest
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

;; PHP
;; $ npm install -g intelephense
(install 'php-mode)
(global-set-key (kbd "C-x i") 'amirreza/edit-init)

;; Autocomplete
(install 'corfu)
(setq corfu-auto t)
(global-corfu-mode +1)

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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(php-mode . ("intelephense" "--stdio"))) ;; Only viable php langauge server
  )

(setq eglot-stay-out-of '(project))
(setq eglot-sync-connect nil)

(dolist (mode '(go rust php)) ;; Enable LSP for these languages
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))

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

(defun amirreza/compile-buffer-name-function (MODESTR)
  (let ((dir (find-project-root-or-default-directory)))
    (format "*%s-%s*" MODESTR dir)))

(setq-default compilation-buffer-name-function 'amirreza/compile-buffer-name-function)

(defun guess-compile-command (DIR)
  (let ((default-directory DIR))
    (cond
     ((file-exists-p "build.bat") "build.bat")
     ((file-exists-p "go.mod")    "go build -v "))))

(defun amirreza/compile-in-directory (DIR COMMAND)
  "Compile in a directory"
  (interactive (list
		(read-directory-name "[Compile] Directory: " (find-project-root-or-default-directory))
		(read-shell-command "[Compile] Command: " (guess-compile-command default-directory))))
  
  (let* ((default-directory DIR)
	 (command COMMAND))
    (setq amirreza/last-compile `(,default-directory ,command))
    (compilation-start command)))

(defun amirreza/compile-dwim ()
  (interactive)
  (cond
   ((equal (length (find-project-root)) 0) (call-interactively 'amirreza/compile-in-directory)) ;; we are not inside a project so we should ask user for directory.
   (t (amirreza/compile-in-directory (find-project-root) (read-shell-command "[Compile] Command: " (guess-compile-command (find-project-root-or-default-directory)))))))

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

(defun amirreza/grep-in-directory (DIR)
  "Run appropriate grep program in directory."
  (interactive (list
		(read-directory-name "[Grep] Directory: " (find-project-root-or-default-directory))))
  
  (cond
     ;; ((fboundp 'consult-ripgrep)             (consult-ripgrep DIR))
     ((or (executable-find "rg") is-windows) (rg DIR (read-string "[Ripgrep] Pattern: " nil)))
     (t                                      (gnu-grep DIR (read-string "[Ripgrep] Pattern: " nil)))))

(defun amirreza/grep-dwim ()
  "DWIM version of amirreza/grep-in-directory"
  (interactive)
  (cond
   ((equal (length (find-project-root)) 0) (call-interactively 'amirreza/grep-in-directory)) ;; we are not inside a project so we should ask user for directory.
   (t (amirreza/grep-in-directory (find-project-root)))))

  

(setq eshell-visual-subcommands '("git" "diff" "log" "show"))

(defun amirreza/git-files (DIR)
  "run git ls-files to get list of files indexed in git."
  (interactive (list (read-directory-name "[Git Files] Directory: " (find-project-root-or-default-directory))))
  (unless (executable-find "git") (error "git executable not found."))

  (let* ((default-directory DIR)
	 (command (format "git ls-files"))
	 (file (completing-read "Git Files:" (string-split (shell-command-to-string command) "\n" t))))
    (find-file file)))

(defun amirreza/rg-files (DIR)
  "run rg --files to get list of files in current project."
  (interactive (list (read-directory-name "[Ripgrep Files] Directory: " (find-project-root-or-default-directory))))
  (unless (executable-find "rg") (error "rg executable not found."))

  (let* ((default-directory DIR)
	 (command (format "rg --files"))
	 (file (completing-read "Ripgrep Files:" (string-split (shell-command-to-string command) "\n" t)))
	 )
    (find-file file)))


(defun amirreza/find-file-in-directory (DIR)
  "Recursive file find starting from 'DIR'."
  (interactive (list (read-directory-name "DIR: " (find-project-root-or-default-directory))))
  (cond
   ((git-repo-p) (amirreza/git-files DIR))
   ((executable-find "rg") (amirreza/rg-files DIR))
   (t (call-interactively 'find-file))))

(defun amirreza/find-file-dwim ()
  "DWIM version of 'amirreza/find-file-in-directory'."
  (interactive)
  (cond
   ((equal (length (find-project-root)) 0) (call-interactively 'amirreza/find-file-in-directory)) ;; we are not inside a project so we should ask user for directory.
   (t (amirreza/find-file-in-directory (find-project-root)))))


(defun amirreza/eshell-dwim ()
  "Jump to eshell buffer associated with current project or create a new."
  (interactive)
  (let* ((root (find-project-root-or-default-directory))
	 (default-directory root)
	 (eshell-buffer-name (format "*eshell-%s*" root)))
    (if (get-buffer eshell-buffer-name)
	(switch-to-buffer eshell-buffer-name)
      (eshell))))

;; Finding
(global-set-key (kbd "C-o")                                          'find-file)
(global-set-key (kbd "C-S-o")                                        'amirreza/grep-dwim)
(global-set-key (kbd "C-M-o")                                        'amirreza/grep-in-directory)
(global-set-key (kbd "M-o")                                          'amirreza/find-file-dwim)

;; Grep Mode
(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>")                             'recompile)
  (define-key grep-mode-map (kbd "g")                                'recompile)
  (define-key grep-mode-map (kbd "G")                                'amirreza/grep-in-directory)
  (define-key grep-mode-map (kbd "M-q")                              'previous-buffer)
  (define-key grep-mode-map (kbd "M-n")                              'jump-down)
  (define-key grep-mode-map (kbd "M-p")                              'jump-up)
  (define-key grep-mode-map (kbd "k")                                'kill-compilation))

;; Compiling / Building
(global-set-key (kbd "M-m")                                          'amirreza/compile-dwim)
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>")                      'recompile)
  (define-key compilation-mode-map (kbd "M-m")                       'previous-buffer)
  (define-key compilation-mode-map (kbd "G")                         'amirreza/compile-in-directory)
  (define-key compilation-mode-map (kbd "n")                         'next-line)
  (define-key compilation-mode-map (kbd "p")                         'previous-line)
  (define-key compilation-mode-map (kbd "M-n")                       'jump-down)
  (define-key compilation-mode-map (kbd "M-p")                       'jump-up)
  (define-key compilation-mode-map (kbd "k")                         'kill-compilation))

;; Emacs Shell (Eshell)
(global-set-key (kbd "M-j")                                          'amirreza/eshell-dwim)

;; Buffers
(global-set-key (kbd "C-.")                                          'next-buffer)
(global-set-key (kbd "C-,")                                          'previous-buffer)

;; Text Editing
(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>")                     'act)) ;; in replace mode <return> applies.
(global-set-key (kbd "C-w")                                          'amirreza/cut) ;; sane cut/kill
(global-set-key (kbd "M-w")                                          'amirreza/copy) ;; sane copy
(global-set-key (kbd "M-[")                                          'kmacro-start-macro) ;; start macro
(global-set-key (kbd "M-]")                                          'kmacro-end-or-call-macro) ;; end macro
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; call keyboard macro.
(global-set-key (kbd "C-z")                                          'undo)           ;; Sane undo key

;; Multiple Cursors
(global-set-key (kbd "C-M-n")                                        'mc/mark-next-like-this-word)
(global-set-key (kbd "C-M-p")                                        'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-S-n")                                        'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p")                                        'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a")                                      'mc/mark-all-like-this-dwim)

;; Jumping around in buffer
(global-set-key (kbd "M->")                                          'end-of-buffer)
(global-set-key (kbd "M-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "C-;")                                          'goto-line)
;; Splits
(global-set-key (kbd "C-0")                                          'delete-window)
(global-set-key (kbd "C-3")                                          'split-window-right)
(global-set-key (kbd "C-2")                                          'split-window-below)
;; Font
(global-set-key (kbd "C-=")                                          'font-zoom-in)
(global-set-key (kbd "C--")                                          'font-zoom-out)

(global-set-key (kbd "C-j")                                          'completion-at-point)

;; Eglot (LSP)
(global-set-key (kbd "M-.")                                          'xref-find-definitions)
(global-set-key (kbd "C-M-.")                                        'xref-find-references)
(global-set-key (kbd "M-,")                                          'xref-go-back)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c m m")                         'eglot-code-actions))

;; Error navigation
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-;")                           'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-'")                           'flymake-goto-next-error))

;; Rectangle Mode (Almost multi cursors)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))

;;; Final benchmarking
(defvar amirreza/emacs-init-took (* (float-time (time-subtract (float-time) amirreza/emacs-starting-time)) 1000) "Time took to load my init file, value is in milliseconds.")
(defvar emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000) "Time took Emacs to boot, value is in milliseconds.")
(setq amirreza/emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza/emacs-init-took emacs-init-time-took))
(message amirreza/emacs-init-log-message)
