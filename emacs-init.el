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
      frame-title-format '(\"%b\")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      )
(setq-default frame-title-format '(\"%e\" (:eval (format \"%s\" default-directory))))
(set-frame-parameter nil 'fullscreen 'maximized) ;; Start emacs in maximized state.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

")

(unless (file-exists-p "~/.emacs.d/early-init.el")
  (write-region amirreza/early-init nil "~/.emacs.d/early-init.el"))


;;

(when load-file-name
  (setq INIT-FILE load-file-name))

(setq is-windows (eq system-type 'windows-nt)                                      
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

(setq custom-file "~/.custom.el"          ;; set custom file to not meddle with init.el
      make-backup-files nil)              ;; no emacs ~ backup files

;; MacOS issues
(setq image-types (cons 'svg image-types)
      mac-command-modifier 'meta)        

(setq vc-follow-symlinks t)              ;; Follow symlinks with no questions

(setq recenter-positions '(middle))

;; General Text Editing
(set-default-coding-systems 'utf-8) ;; always use UTF8
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode) ;; when selected a text and user types delete text
(global-display-line-numbers-mode +1) ;; Display line numbers.

(setq dabbrev-upcase-means-case-search t
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
(global-set-key (kbd "C-w")                                          'amirreza/cut)
(global-set-key (kbd "M-w")                                          'amirreza/copy)
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
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "C-;")                                          'consult-goto-line)
(global-set-key (kbd "C-c n")                                        'next-buffer)
(global-set-key (kbd "C-c p")                                        'previous-buffer)
(global-set-key (kbd "C-.")                                          'isearch-forward-thing-at-point)

(unless (executable-find "rg") (error "Install ripgrep, this configuration relies heavy on it's features."))

(global-unset-key (kbd "C-x C-c"))


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

(amirreza/set-font "Source Code Pro" 13)

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


;; Colors
(custom-set-faces
 `(default                          ((t (:foreground "#debe95" :background "#161616"))))
 `(hl-line                          ((t (:background "#252525"))))
 `(vertico-current                  ((t (:background "#252525"))))
 `(region                           ((t (:background  "medium blue"))))
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

;; Minibuffer
(install 'orderless "Orderless Completion strategy, sort of like fuzzy but different.")
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(install 'vertico "Provides a richer minibuffer completion facility, cool thing is that it does not need any hooking up and it will work for everything in the minibuffer.")
(vertico-mode +1)
(setq vertico-count 10
      vertico-cycle t)

(install 'consult "Set of helper commands that are powered by vertico completion but they are not dependant on it.")
(global-set-key (kbd "M-y") 'consult-yank-pop)

(install 'marginalia)
(marginalia-mode +1)


;; Dumb way to find things
(install 'dumb-jump "Poor's man Jump to def/dec/ref. (using grep)")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key (kbd "<f12>")   'xref-find-definitions)
(global-set-key (kbd "C-<f12>") 'xref-find-references)


;; Modeline
(setq-default mode-line-format '("%e"
				 mode-line-front-space
				 mode-line-modified
				 mode-line-remote
				 " "
				 (:eval (if (buffer-file-name) (buffer-file-name) (buffer-name)))
				 " "
				 (:eval (when vc-mode (format "| %s |" (string-trim vc-mode))))
				 " "
				 (:eval (format "(%s)" (capitalize (string-remove-suffix "-mode" (symbol-name major-mode)))))
				 " | "
				 mode-line-percent-position
				 " "
				 "(%l, %C)"
				 " | "
				 (text-scale-mode
				  (" " text-scale-mode-lighter))
				 ))

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


;; Finding project root based on determinestic patterns.
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


;; Compile
(defun amirreza/compile-buffer-name-function (MODE)
  (let ((dir (find-project-root-or-default-directory)))
    (format "*compile-%s*" dir)))
(setq-default compilation-buffer-name-function 'amirreza/compile-buffer-name-function)

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

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(global-set-key (kbd "M-m") 'amirreza/compile)
(global-set-key (kbd "<f5>") 'amirreza/compile)

(defun amirreza/open-directory-in-frame (DIR)
  "open a new frame with directory set to DIR."
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let* ((dired-buffer (dired-noselect DIR)))
    (with-current-buffer dired-buffer
      (make-frame))))
(global-set-key (kbd "C-c o") 'amirreza/open-directory-in-frame)


;; Dired.
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
      (rename-buffer (format "*Dired-%s*" dir)))))

(global-set-key (kbd "C-0") 'amirreza/side-tree)

(defun amirreza/dired-hook ()
  (dired-hide-details-mode +1))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'amirreza/dired-hook)
  (define-key dired-mode-map (kbd "C-0") 'kill-current-buffer))


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


;; Golang
(install 'go-mode)
(defun amirreza/go-hook ()
  (interactive)
  (setq gofmt-args '("-s"))
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save 0 t))
(add-hook 'go-mode-hook 'amirreza/go-hook)

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


;; Eshell: Emacs cross platform shell
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

(defalias 'EShell 'amirreza/eshell)

(global-set-key (kbd "M-;") 'amirreza/eshell)

(defvar amirreza/emacs-init-took (* (float-time (time-subtract (float-time) amirreza/emacs-starting-time)) 1000) "Time took to load my init file, value is in milliseconds.")
(defvar emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000) "Time took Emacs to boot, value is in milliseconds.")
(setq amirreza/emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza/emacs-init-took emacs-init-time-took))
(message amirreza/emacs-init-log-message)
