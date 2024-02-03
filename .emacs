(setq amirreza-emacs-starting-time (float-time))                                                              ;; Store current time for further analysis.
(when load-file-name
  (setq BASE_PATH (file-name-directory load-file-name))                                                       ;; Store this file location.
  (setq INIT_FILE load-file-name))
(setq frame-inhibit-implied-resize t)                                                                         ;; Don't let emacs to resize frame when something inside changes
(setq initial-scratch-message "")                                                                             ;; No starting text in *scratch* buffer.
(setq gc-cons-threshold (* 1024 1024 10))                                                                     ;; Default emacs garbage collection threshold is 800KB which is low for today standards, memory is cheap, so we make a bit higher, remember if you set it to high it would cause major pauses.
(setq redisplay-dont-pause t)
(setq vc-follow-symlinks t)                                                                                   ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ()))                                                                         ;; no stupid sounds
(setq custom-file "~/.custom.el")                                                                             ;; set custom file to not meddle with init.el
(setq make-backup-files nil)                                                                                  ;; no emacs ~ backup files
(global-unset-key (kbd "C-x C-c"))
(setq is-windows (eq system-type 'windows-nt))                                         
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(setq has-treesitter (>= emacs-major-version 29))
(unless (executable-find "rg") (error "Install ripgrep, this configuration relies heavy on it's features."))
(setq use-short-answers t)                                                                                    ;; Always prefer short answers
(setq image-types (cons 'svg image-types))                                                                    ;; macos issue.
(setq mac-command-modifier 'meta)                                                                             ;; macos issue.
(setq recenter-positions '(middle))
(setq inhibit-startup-screen t)                                                                               ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))                                                  ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval (format "%s @ %s" default-directory system-name))))            ;; OS window title
(menu-bar-mode -1)                                                                                            ;; disable menu bar
(tool-bar-mode -1)                                                                                            ;; disable tool bar
(scroll-bar-mode -1)                                                                                          ;; disable scroll bar
(setq kill-whole-line t)                                                                                      ;; kill line and newline char
(global-auto-revert-mode +1)                                                                                  ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode)                                                                                       ;; when selected a text and user types delete text
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))               ;; Jump up half of window size.
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))                    ;; Jump down half of window size.

(defun edit-init ()
  "Edit this file."
  (interactive)
  (find-file INIT_FILE))
(global-set-key (kbd "<f1>") 'edit-init)

(defun toggle-debug-mode ()
  "Toggle Emacs debug mode." 
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t)))

;; Package manager
;; With emacs package manager, You should always be careful with what you install
;; because even if you don't use it in your init file at all, it will be semi-loaded with emacs at startup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(defun install (PKG)
  (if (listp PKG)
      (unless (package-installed-p (car PKG))
	(if (fboundp 'package-vc-install)
	    (package-vc-install PKG)
	(warn "package-vc-install is available from Emacs 29, ignoring this install statement.")))
      (unless (package-installed-p PKG)
	(package-install PKG))))
(unless package-archive-contents  (package-refresh-contents))

;; Themes
(install 'ef-themes)
(install 'gruber-darker-theme)
(install 'doom-themes)
(setq custom-safe-themes t)

(defun amirreza/load-theme (theme)
  "disable all active themes and then load-theme"
  (interactive
   (list
    (intern (completing-read "Load Theme!: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t))

(defun theme-available-p (theme)
  (member theme (custom-available-themes)))

(if (fboundp 'package-vc-install)
    (install '(amirreza-themes :vc-backend Git :url "https://github.com/amirrezaask/themes.git"))
  (warn "Install Emacs 29 to access package-vc-install."))

(cond
 ((theme-available-p 'naysayer)       (amirreza/load-theme 'naysayer))
 ((theme-available-p 'doom-one)       (amirreza/load-theme 'doom-one))
 ((theme-available-p 'gruber-darker)  (amirreza/load-theme 'gruber-darker))
 ((theme-available-p 'ef-dark)        (amirreza/load-theme 'ef-dark))
 ((theme-available-p 'modus-vivendi)  (amirreza/load-theme 'modus-vivendi)))

;;;; Minibuffer (Vertico)
(install 'vertico)
(install 'consult)
(install 'orderless)
(install 'vertico-posframe)
(vertico-mode +1)
(vertico-posframe-mode +1)

(setq vertico-count 100
      vertico-posframe-height (truncate (* (frame-height) 0.5)))

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Font
(setq font-family "")
(defun load-font (font fontsize)
  "Loads a font."
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun set-font-size (fontsize)
  "Set a font size"
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(load-font "Consolas" 13)

;; DevDocs, locally install documents and then query on them.
(install 'devdocs)

;; DumbJump
(install 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Env and PATH
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

(defun find-project-root ()
  "Try to find project root based on deterministic predicates"
  (cond
   ((eq major-mode 'go-mode)                                (locate-dominating-file default-directory "go.mod"))
   ((or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))  (locate-dominating-file default-directory "build.bat"))
   (t                                                       (locate-dominating-file default-directory ".git"))))

(defun find-project-root-or-default-directory ()
  (or (find-project-root) default-directory))

(defun guess-build-command (DIR)
  (let ((default-directory DIR))
    (cond
     ((file-exists-p "build.bat") "build.bat")
     ((file-exists-p "go.mod")    "go build -v "))))

(defun guess-run-command (DIR)
  (let ((default-directory DIR))
    (cond
     ((file-exists-p "run.bat") "run.bat")
     ((file-exists-p "go.mod")  "go run "))))

;;;; Building And Running
(setq amirreza-build-history '())
(setq amirreza-last-build nil)

(defun amirreza-build ()
  "Compile in a directory"
  (interactive)
  (when amirreza-last-build
    (unless (y-or-n-p "Use last build configuration?") (setq amirreza-last-build nil)))
  (let* ((default-directory (or (car amirreza-last-build) (read-directory-name "[Build] Directory: " (find-project-root-or-default-directory))))
	(command (or (car (cdr amirreza-last-build)) (read-shell-command "[Build] Command: " (guess-build-command default-directory) amirreza-build-history))))
    (setq amirreza-last-build `(,default-directory ,command))
    (compilation-start command)))


(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

;;;; G/RE/P aka GREP
(setq amirreza-grep-query-history '())
(defun rg (dir pattern)
  "runs Ripgrep program in a compilation buffer."
  (interactive (list (read-directory-name "[Ripgrep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[Ripgrep] Pattern: " nil amirreza-grep-query-history)))
  (unless (executable-find "rg") (error "ripgrep executable not found, install from https://github.com/BurntSushi/ripgrep/releases"))

  (let* ((default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[grep] Pattern: " nil amirreza-grep-query-history)))
  (unless (executable-find "ug") (error "Gnu Grep executable not found"))
  (let* (
	 (default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza-grep (dir pattern &optional SPLIT)
  ""
  (interactive (list (read-directory-name "[Grep] Directory: " (find-project-root-or-default-directory))
		     (read-string "[Grep] Pattern: " nil amirreza-grep-query-history)))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   (t (gnu-grep dir pattern))))

(defalias 'grep 'amirreza-grep)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))

(defun rg-find-files ()
  (interactive)
  (unless (executable-find "rg") (error "rg-find-files needs ripgrep."))
  (let* ((default-directory (or (find-project-root) default-directory))
	 (results (string-split (string-trim (shell-command-to-string "rg --files") "\n" "\n") "\n"))
	 (relfile (completing-read "Files: " results))
	 (absfile (expand-file-name relfile default-directory)))
    (find-file absfile)))

(defalias 'find 'rg-find-files)

;; QueryReplace
;; NOTE: This will also effect y-or-n-p questions, basically pressing <enter> now is considered to be yes.
(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))

;;;; Dabbrev
(setq dabbrev-upcase-means-case-search t
      dabbrev-case-replace nil
      dabbrev-case-fold-search t
      dabbrev-upcase-means-case-search nil)

;;;; Programming
(install 'go-mode)


;; So long mode
(install 'so-long)
(global-so-long-mode +1)

(defun amirreza-go-hook ()
  (interactive)
  (setq-local devdocs-current-docs '(go))
  (add-hook 'before-save-hook 'gofmt-before-save 0 t))

(add-hook 'go-mode-hook 'amirreza-go-hook)

(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++ code style

;;; THINGS I HATE
(install 'php-mode)
(install 'yaml-mode)
(install 'json-mode)
;;; THINGS I HATE ENDED

;;;; Copy/Cut
(defun amirreza-copy ()
  "Either copy region or the current line."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
    (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun amirreza-cut ()
  "Either cut region or the current line."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)) ;; copy active region contents
    (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line

(defun amirreza-text-scale-increase ()
  (interactive)
  (text-scale-increase 1))

(defun amirreza-text-scale-decrease ()
  (interactive)
  (text-scale-decrease 1))

;; Autocompletion popup
;; Sometimes having an autocomplete helps, but enabled manually I don't want a popup always screaming to my face of my options.
(install 'corfu)
(setq corfu-auto nil)
(global-corfu-mode +1)

;; LSP (although I hate them cause running another program on my system to just find where something is defined seems crazy)
;; Eglot is now shipped with Emacs 29, but we make sure to have it.
(unless (>= emacs-major-version 29) (install 'eglot))

;; @Speed: We will disable most features from LSP, they make Emacs slow and probably make you a bad programmer.
(setq eglot-ignored-server-capabilities '(
					  :hoverProvider
					  :documentHighlightProvider
					  :documentSymbolProvider
					  :workspaceSymbolProvider
					  :codeActionProvider
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
					  
(add-hook 'go-mode-hook #'eglot-ensure) ;; Enable eglot by default in Go

;; Keybindings
(global-set-key (kbd "C-h d")                                        'devdocs-lookup)
(global-set-key (kbd "<f12>")                                        'xref-find-definitions)
(global-set-key (kbd "C-o")                                          'find-file) ;; open files
(global-set-key (kbd "C-w")                                          'amirreza-cut) ;; Cut
(global-set-key (kbd "M-w")                                          'amirreza-copy) ;; Copy
(global-set-key (kbd "M-y")                                          'consult-yank-pop)
(global-set-key (kbd "M-k")                                          'kill-buffer) ;; Kill buffer
(global-set-key (kbd "M-m")                                          'amirreza-build) ;; Interactive Build
(global-set-key (kbd "<f5>")                                         'amirreza-build) ;; Interactive Build
(global-set-key (kbd "M-o")                                          'rg-find-files) ;; Find files in project
(global-set-key (kbd "C-.")                                          'isearch-forward-thing-at-point)
(global-set-key (kbd "M-0")                                          'query-replace) ;; Replace pattern with a string
(global-set-key (kbd "C-;")                                          'consult-goto-line)
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "M-j")                                          'consult-ripgrep)
(global-set-key (kbd "C-M-j")                                        'amirreza-grep)
(global-set-key (kbd "C-q")                                          'dabbrev-expand)           ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "C-j")                                          'completion-at-point)       ;; Manual trigger for completion popup.
(global-set-key (kbd "C-z")                                          'undo)                      ;; Sane undo key
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "M-SPC")                                        'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))
(global-set-key (kbd "M-[")                                          'kmacro-start-macro)         ;; start recording keyboard macro.
(global-set-key (kbd "M-]")                                          'kmacro-end-macro)           ;; end recording keyboard macro.
(global-set-key (kbd "C-3")                                          'split-window-horizontally)
(global-set-key (kbd "C-,")                                          'other-window)
(global-set-key (kbd "C-2")                                          'split-window-vertically)
(global-set-key (kbd "C-<return>")                                   'save-buffer)               ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "C-=")                                          'amirreza-text-scale-increase)
(global-set-key (kbd "C--")                                          'amirreza-text-scale-decrease)

;;;; Record times
(defvar amirreza-emacs-init-took (* (float-time (time-subtract (float-time) amirreza-emacs-starting-time)) 1000) "Time took to load my init file, value is in milliseconds.")
(defvar emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000) "Time took Emacs to boot, value is in milliseconds.")
(setq amirreza-emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza-emacs-init-took emacs-init-time-took))
(message amirreza-emacs-init-log-message)
