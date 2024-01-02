(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(setq is-windows (eq system-type 'windows-nt))
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(defun edit-init ()
  (interactive)
  (if is-windows
      (find-file "W:\\dotfiles\\.emacs")
    (find-file "~/w/dotfiles/.emacs")))
(global-set-key (kbd "C-x i") 'edit-init)
(setq use-short-answers t) ;; Always prefer short answers
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again
;; Font stuff
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

(load-font "Liberation Mono" 11)

(defun home (path)
  (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin") ;; homebrew
(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
(add-to-list 'exec-path "c:/programs/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(setq split-window-preferred-function (lambda (window))) ;; Don't change my windows Emacs, please
(setq recenter-positions '(middle))
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval default-directory)))
(menu-bar-mode -1) ;; disable menu bar
(global-hl-line-mode +1) ;; Highlight current line
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(setq kill-whole-line t) ;; kill line and newline char
(pixel-scroll-precision-mode +1) ;; Smooth scrolling
(global-auto-revert-mode +1) ;; auto refresh buffers from disk
(delete-selection-mode) ;; when selected a text and user types delete text

;; Install packages
(defun install (PKG) (unless (package-installed-p PKG) (package-install PKG)))
(install 'go-mode)
(install 'php-mode)
(install 'lsp-mode)
(install 'vertico)
(install 'orderless)


;; Minibuffer improvement
(setq vertico-cycle t)
(setq vertico-count 25)
(vertico-mode)
(setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

;; Themes
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i))) ;; don't stack themes on each other
(setq custom-safe-themes t) ;; all themes are safe, don't ask
(setq themes-directory (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-directory)
(defun theme-file (name) (expand-file-name name themes-directory))
(defun theme-exists (name) (file-exists-p (theme-file name)))
(unless (file-exists-p themes-directory) (make-directory themes-directory))
(unless (theme-exists "jonathan-blow-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/jonathan-blow-theme.el" (theme-file "jonathan-blow-theme.el") t))
(unless (theme-exists "handmadehero-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/handmadehero-theme.el" (theme-file "handmadehero-theme.el") t))
(unless (theme-exists "4coder-fleury-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/4coder-fleury-theme.el" (theme-file "4coder-fleury-theme.el") t))
(load-theme 'handmadehero)

;; Compiling stuff
(defun compile-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Compile] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(setq compile-receipes '( ;; Regex pattern as key and value would be (DIR COMMAND) that will be passed into (compilation-start)
			 ("w:\\/Clockwork\\/.*" . ("w:/Clockwork/" ".\\build.bat"))
			 ))

(defun compile-dwim ()
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name (current-buffer))))
	 (args (alist-get dir compile-receipes nil nil 'string-match-p)))
    (when args
      (message "Compilation Command is '%s'" (car (cdr args)))
      (message "Compilation Dir is '%s'" (car args)))
    (if args
	(let ((default-directory (car args))) (compilation-start (car (cdr args))))
      (call-interactively 'compile-directory))))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; Searching stuff
(defun rg (dir pattern)
  "run Ripgrep"
  (interactive (list (read-directory-name "[Ripgrep] Directory: ") (read-string "[Ripgrep] Pattern: ")))
  (unless (executable-find "rg") (error "ripgrep executable not found, install from https://github.com/BurntSushi/ripgrep/releases"))
  (let* ((default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun ug (dir pattern)
  (interactive (list (read-directory-name "[ug] Directory: ") (read-string "[ug] Pattern: ")))
  (unless (executable-find "ug") (error "ugrep executable not found, install from https://github.com/Genivia/ugrep/releases"))
  (let* ((default-directory dir)
	 (command (format "ug --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: ") (read-string "[grep] Pattern: ")))
  (unless (executable-find "ug") (error "Gnu Grep executable not found"))
  (let* ((default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun grep (dir pattern)
  (interactive (list (read-directory-name "[Grep] Directory: ") (read-string "[Grep] Pattern: ")))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   ((executable-find "ug") (ug dir pattern))
   (t (gnu-grep dir pattern))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))


;; this part contains some regexes that will be defined for each language and let us do search based on language syntax
(defvar amirreza-find-functions-regex nil)
(defvar amirreza-find-types-regex nil)

(defun amirreza-find-functions (DIR)
  (interactive (list (read-directory-name "[Functions] Directory: ")))
  (when (string-equal amirreza-find-functions-regex "") (error "No regex defined for this mode"))
  (grep DIR amirreza-find-functions-regex))

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (setq-local amirreza-find-functions-regex "\\(defun"))))

;; Golang
(setq amirreza-golang-imenu-generic-expression '((nil "^type *\\([^
]*\\)" 1)
						 (nil "^func *\\(.*\\) {" 1)))

(defun amirreza-go-hook ()
  (message "Amirreza Go Hook")
  (setq-local imenu-generic-expression amirreza-golang-imenu-generic-expression)
  (setq-local amirreza-find-functions-regex "^func *\\(.*\\) \\{"))

(with-eval-after-load 'go-mode (add-hook 'go-mode-hook 'amirreza-go-hook))

;; C/C++
(defun amirreza-c++-hook ()
  (message "Amirreza CC Hook"))

(setq-default c-default-style "linux"
	      c-basic-offset 4)

(add-hook 'c++-mode-hook 'amirreza-c++-hook)

;; PHP
(defun amirreza-php-hook ()
  (message "Amirreza PHP Hook")
  (setq-local amirreza-find-functions-regex "function\\s+.*\\(.*\\)")
  (setq-local imenu-generic-expression
	      '((nil "\\(?:^\\s-*\\(\\(?:\\(?:\\(?:abstract\\|final\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|static\\)\\s-+\\)*\\)*function\\s-+\\(?:&\\s-*\\)?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(.*$\\)\\)" 1)
	       (nil "^\\s-*\\(\\(?:\\(?:p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|static\\|var\\)\\s-+\\)+\\(?:\\(?:[?|]?\\(?:\\\\\\|\\sw\\|\\s_\\)\\s-+\\)?\\)*\\$\\(?:\\sw\\|\\s_\\)+\\b\\)" 1)
	       (nil "^\\s-*\\(\\(?:\\(?:p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\s-+\\)*const\\s-+\\(?:\\sw\\|\\s_\\)+\\s-*\\(?:=\\s-*.\\{0,40\\}\\)?\\)" 1)
	       (nil "^\\s-*\\(function\\s-+\\(?:\\sw\\|\\s_\\)+\\s-*(.\\{0,100\\}\\)" 1)
	       (nil "Classes" "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:class\\|interface\\|trait\\|enum\\)\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)" 0)
	       (nil "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?namespace\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)" 1))

	      )
  )
(with-eval-after-load 'php-mode (add-hook 'php-mode-hook 'amirreza-php-hook))

;; Keymaps
(global-set-key (kbd "C-.") 'isearch-forward-thing-at-point)
(global-set-key (kbd "C-/") 'grep) ;; Magical search
(global-set-key (kbd "<f5>") 'compile-dwim) ;; |> little green button of my IDE
(global-set-key (kbd "M-m") 'compile-dwim) ;; |> button
(global-set-key (kbd "C-z") 'undo) ;; Sane undo key
(global-set-key (kbd "C-<return>") 'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter) ;; start recording keyboard macro..
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat) ;; end recording keyboard macro.
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-q") 'dabbrev-expand) ;; expand current word with suggestions from all buffers.
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-set-key (kbd "M-p") 'jump-up)
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "M-i") 'imenu)
;; Split window since no other code can do it
(split-window-horizontally)
