;; Amirreza Emacs

(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
(setq redisplay-dont-pause t)
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

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;
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

(load-font "Liberation Mono" 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;
;; Packages ;;;
;;;;;;;;;;;;;;;
(package-initialize)
(defun install (PKG) (unless (package-installed-p PKG) (package-install PKG)))
(unless package-archive-contents (package-refresh-contents))
(install 'gruber-darker-theme)
(install 'ef-themes)
(install 'php-mode)
(install 'go-mode)

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;
(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

(setq split-window-preferred-function (lambda (window))) ;; Don't change my windows Emacs, please
(setq recenter-positions '(middle))
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("emacs: %e" (:eval default-directory)))
(menu-bar-mode -1) ;; disable menu bar
(global-hl-line-mode +1) ;; Highlight current line
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(setq kill-whole-line t) ;; kill line and newline char
(delete-selection-mode) ;; when selected a text and user types delete text

;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;
(defadvice load-theme (before disable-themes-first activate) (dolist (i custom-enabled-themes) (disable-theme i))) ;; don't stack themes on each other
(setq custom-safe-themes t) ;; all themes are safe, don't ask
(setq themes-directory (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-directory)
(defun theme-file (name) (expand-file-name name themes-directory))
(defun theme-exists (name) (file-exists-p (theme-file name)))
(unless (file-exists-p themes-directory) (make-directory themes-directory))
(unless (theme-exists "jonathan-blow-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/jonathan-blow-theme.el" (theme-file "jonathan-blow-theme.el") t))
(unless (theme-exists "handmadehero-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/handmadehero-theme.el" (theme-file "handmadehero-theme.el") t))
(unless (theme-exists "cmuratori-theme.el") (url-copy-file "https://raw.githubusercontent.com/amirrezaask/themes/main/cmuratori-theme.el" (theme-file "cmuratori-theme.el") t))
(load-theme 'cmuratori)

;;;;;;;;;;;;;;;;;;;;;;
;; Compiling ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
(defun compile-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Compile] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(setq compile-receipes '( ;; Regex pattern as key and value would be (DIR COMMAND) that will be passed into (compilation-start)
			 ("w:\\/Clockwork\\/.*" . ("w:/Clockwork/" ".\\build.bat"))
			 ("w:\\/snappdoctor\\/metric-collector\\/.*" . ("w:/snappdoctor/metric-collector" ".\\build-server.bat"))
			 ))

(defun compile-dwim ()
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name (current-buffer))))
	 (args (alist-get dir compile-receipes nil nil 'string-match-p)))
    (when args
      (message "Compilation Command is '%s'" (car (cdr args)))
      (message "Compilation Dir is '%s'" (car args)))
    (save-some-buffers t nil)
    (if args
	(let ((default-directory (car args))) (compilation-start (car (cdr args))))
      (call-interactively 'compile-directory))))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;
;; Running ;;
;;;;;;;;;;;;;
(defun run-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Run] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(setq run-receipes '( ;; Regex pattern as key and value would be (DIR COMMAND) that will be passed into (compilation-start)
		     ("w:\\/Clockwork\\/.*" . ("w:/Clockwork/" ".\\run.bat"))
		     ("w:\\/snappdoctor\\/metric-collector\\/.*" . ("w:/snappdoctor/metric-collector" ".\\run-server.bat"))
		     ))

(defun run-dwim ()
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name (current-buffer))))
	 (args (alist-get dir run-receipes nil nil 'string-match-p)))
    (when args
      (message "Run Command is '%s'" (car (cdr args)))
      (message "Run Dir is '%s'" (car args)))
    (save-some-buffers t nil)
    (if args
	(let ((default-directory (car args))) (compilation-start (car (cdr args))))
      (call-interactively 'run-directory))))


;;;;;;;;;;
;; GREP ;;
;;;;;;;;;;
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

(defun grep-dwim (dir pattern)
  (interactive (list (read-directory-name "[Grep] Directory: ") (read-string "[Grep] Pattern: " nil nil (word-at-point))))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   ((executable-find "ug") (ug dir pattern))
   (t (gnu-grep dir pattern))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))


;;;;;;;;;;;;;;;;
;; EXPANSIONS ;;
;;;;;;;;;;;;;;;;
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search nil)

(setq amirreza-expansions '(("TO" . "TODO(amirreza): ")
			  ("NO" . "NOTE(amirreza): ")))

(defun amirreza-expand ()
  (interactive)
  (let* ((word (current-word))
	(expansion (alist-get word amirreza-expansions nil nil 'string-equal)))
    (if expansion
	;; expand snippet
	(progn
	  (backward-delete-char (length word))
	  (insert expansion))
      (call-interactively 'dabbrev-expand))))

;;;;;;;;;;;
;; C/C++ ;;
;;;;;;;;;;;
(defun amirreza-c++-hook () (setq-local amirreza-expansions (append '(("for" . "for() {}")) amirreza-expansions)))

(setq-default c-default-style "linux"
	      c-basic-offset 4)

(add-hook 'c++-mode-hook 'amirreza-c++-hook)
(add-hook 'c-mode-hook 'amirreza-c++-hook)

;;;;;;;;;;;;
;; Golang ;;
;;;;;;;;;;;;
(defun amirreza-go-hook ()
  (setq-local imenu-generic-expression '((nil "^type *\\([^ 	
]*\\)" 1)
					(nil "^func *\\(.*\\) {" 1)))

  (setq-local amirreza-expansions (append '(("ifer" . "if err != nil {}")) amirreza-expansions)))

(with-eval-after-load 'go-mode (add-hook 'go-mode-hook 'amirreza-go-hook))

;;;;;;;;;;;;;
;; Keymaps ;;
;;;;;;;;;;;;;
(global-set-key (kbd "C-.") 'isearch-forward-thing-at-point)
(global-set-key (kbd "C-/") 'grep-dwim) ;; Magical search
(global-set-key (kbd "<f5>") 'compile-dwim) ;; |> little green button of my IDE
(global-set-key (kbd "M-m") 'compile-dwim) ;; |> button
(global-set-key (kbd "C-M-m") 'run-dwim) ;; |> button
(global-set-key (kbd "C-z") 'undo) ;; Sane undo key
(global-set-key (kbd "C-<return>") 'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "M-[") 'kmacro-start-macro) ;; start recording keyboard macro.
(global-set-key (kbd "M-]") 'kmacro-end-macro) ;; end recording keyboard macro.
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "C-3") 'split-window-horizontally) ;; | split
(global-set-key (kbd "C-2") 'split-window-vertically) ;; - split
(global-set-key (kbd "C-o") 'other-window) ;; Switch window
(global-set-key (kbd "C-q") 'amirreza-expand) ;; Try snippets and then expand with emacs dabbrev
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-set-key (kbd "M-p") 'jump-up)
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-;") 'previous-error)
(global-set-key (kbd "M-'") 'next-error)
