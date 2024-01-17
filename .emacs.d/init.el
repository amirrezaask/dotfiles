(setq amirreza-emacs-starting-time (float-time)) ;; Store current time for further analysis.
(setq BASE_PATH (file-name-directory load-file-name)) ;; $CWD where this file is.
(setq INIT_FILE load-file-name)
(add-to-list 'load-path (expand-file-name "lisp" BASE_PATH))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" BASE_PATH))

(setq custom-safe-themes t)
(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq initial-scratch-message "") ;; No starting text in *scratch* buffer.
(setq gc-cons-threshold 200000000) ;; 200 MB for the GC threshold
(setq redisplay-dont-pause t)
;; (setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(global-unset-key (kbd "C-x C-c"))
(setq is-windows (eq system-type 'windows-nt))
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(setq has-treesitter (>= emacs-major-version 29))
(unless (executable-find "rg") (error "Install ripgrep, this configuration relies heavy on it's features."))

(defun edit-init ()
  (interactive)
  (find-file INIT_FILE))

(global-set-key (kbd "C-x i") 'edit-init)
(setq use-short-answers t) ;; Always prefer short answers
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again
(setq recenter-positions '(middle))
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("Emacs: %e" (:eval default-directory)))
(menu-bar-mode -1) ;; disable menu bar
(global-hl-line-mode +1) ;; Highlight current line
(tool-bar-mode -1) ;; disable tool bar
(scroll-bar-mode -1) ;; disable scroll bar
(setq kill-whole-line t) ;; kill line and newline char
(delete-selection-mode) ;; when selected a text and user types delete text
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;;;; Package manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 27) (package-initialize))
(defun install (PKG) (unless (package-installed-p PKG) (package-install PKG)))
;; (unless package-archive-contents (package-refresh-contents))

;;;; Themes
(install 'catppuccin-theme)
(install 'gruber-darker-theme)
(install 'dracula-theme)
(install 'ef-themes)
(defadvice load-theme (before disable-themes activate)
  (mapc #'disable-theme custom-enabled-themes))
(load-theme 'catppuccin)


;;;; Minibuffer
(install 'vertico)
(vertico-mode +1)
(setq vertico-count 5)
(setq vertico-cycle t)
(setq vertico-resize nil)


;;;; Modeline
(install 'doom-modeline)
(setq doom-modeline-icon nil)
(setq doom-modeline-height 45)
(doom-modeline-mode +1)


;;;; Window stuff
(setq amirreza-split-window-horizontal-vertical-threshold 250)
(defun amirreza-split-window (WINDOW &optional SWITCH-TO)
  "Split window based on 'amirreza-split-window-horizontal-vertical-threshold'"
  (interactive (list nil))
  (if (> (frame-width nil) amirreza-split-window-horizontal-vertical-threshold)
      (progn
	(delete-other-windows)
	(split-window-horizontally)
	(if SWITCH-TO (other-window 1)))
    (progn
      (delete-other-windows)
      (split-window-vertically)
      (if SWITCH-TO (other-window 1)))))
(setq split-window-preferred-function 'amirreza-split-window) ;; Don't change my windows Emacs, please


;;;; Font
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

(load-font "Hack" 13)

;;;; Env and PATH
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



;;;; Highlight todos
(make-face 'font-lock-todo-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-study-face)
(set-face-attribute 'font-lock-todo-face nil :foreground "Red" :underline t)
(set-face-attribute 'font-lock-note-face nil :foreground "Green" :underline t)
(set-face-attribute 'font-lock-important-face nil :foreground "Yellow" :underline t)
(set-face-attribute 'font-lock-study-face nil :foreground "cyan1" :underline t)

(defun amirreza-add-todo/note-highlight ()
  (font-lock-add-keywords
   major-mode
   '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
     ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
     ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
     ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
(add-hook 'prog-mode-hook 'amirreza-add-todo/note-highlight)


;;;; Building And Running
(defun amirreza-build (DIR &optional SPLIT)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Build] Directory: ")))
  (let ((default-directory DIR)
	(command (read-string "[Build] Command:")))
    (compilation-start command)))

(defun amirreza-run (DIR &optional SPLIT)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Run] Directory: ")))
  (let ((default-directory DIR)
	(command (read-string "[Run] Command:")))
    (compilation-start command)))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

;;;; G/RE/P aka GREP
(defun rg (dir pattern)
  "run Ripgrep"
  (interactive (list (read-directory-name "[Ripgrep] Directory: ") (read-string "[Ripgrep] Pattern: ")))
  (unless (executable-find "rg") (error "ripgrep executable not found, install from https://github.com/BurntSushi/ripgrep/releases"))

  (let* (
	 (default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: ") (read-string "[grep] Pattern: ")))
  (unless (executable-find "ug") (error "Gnu Grep executable not found"))
  (let* (
	 (default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza-grep (dir pattern &optional SPLIT)
  ""
  (interactive (list (read-directory-name "[Grep] Directory: ") (read-string "[Grep] Pattern: ")))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   (t (gnu-grep dir pattern))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))

;;;; Workspaces
(defvar amirreza-workspaces '() "Workspace objects.")
(defvar amirreza-workspaces-file "~/emacs-workspaces" "Path to the workspace file.")
(defun amirreza-workspace-reload-workspaces ()
  (interactive)
  (if (file-readable-p amirreza-workspaces-file)
      (progn
	(setq amirreza-workspaces '())
	(load-file amirreza-workspaces-file)
	(message "#%d workspaces loaded." (length amirreza-workspaces)))
    (error "Workspace file %s is not readable." amirreza-workspaces-file)))

(defun amirreza-list-workspaces ()
  (let* ((workspaces '()))
    (mapc (lambda (workspace-obj)
	    (add-to-list 'workspaces (plist-get (cdr workspace-obj) :name))) amirreza-workspaces)

    workspaces))

(defun amirreza-get-workspace-for-path (PATH) (alist-get PATH amirreza-workspaces nil nil 'string-match-p))

(defun amirreza-get-workspace-by-name (NAME)
  (let* ((workspace nil))
    (mapc (lambda (workspace-obj)
	    (if (string-equal (plist-get (cdr workspace-obj) :name) NAME) (setq workspace workspace-obj))
	    ) amirreza-workspaces)

    workspace))

(defun amirreza-workspace-jump-to-workspace (NAME)
  (interactive (list (completing-read "[Workspace]: " (amirreza-list-workspaces))))
  (let* ((workspace (amirreza-get-workspace-by-name NAME))
	 (workspace (if workspace (cdr workspace))))
    (if workspace
	(find-file (plist-get workspace :cwd)))))

(defun amirreza-workspace-open-workspaces-file ()
  (interactive)
  (find-file amirreza-workspaces-file))

(defun defworkspace (&rest kargs)
  "Defines a workspace, designed to be called from a seperate file, use it in `amirreza-workspaces-file`
  (defworkspace
	:name    Name of the workspace
	:build   Command to be called for building, it will be called in :cwd
	:run     Command to be called for running, it will be called in :cwd
	:cwd     CWD for building and running
	:pattern regex pattern to match files in the workspace
)
"
  (let ((name (plist-get kargs     :name))
	(cwd  (plist-get kargs     :cwd))
	(build  (plist-get kargs   :build))
	(run  (plist-get kargs     :run))
	(pattern  (plist-get kargs :pattern)))
    (add-to-list 'amirreza-workspaces `(,pattern   :name ,name :build ,build :run ,run :cwd ,cwd))))

(defun amirreza-workspace-build ()
  "Runs :build command of workspace inside :cwd."
  (interactive)
  (let* ((file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (save-some-buffers t nil)
    (if (and workspace (plist-get workspace :build))
	(let ((default-directory (plist-get workspace :cwd))) (compilation-start (plist-get workspace :build)))
      (amirreza-build (read-directory-name "[Build] Directory: ") t))))

(defun amirreza-workspace-run ()
  "Runs :run command of workspace inside :cwd."
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (save-some-buffers t nil)
    (if (and workspace (plist-get workspace :run))
	(let ((default-directory (plist-get workspace :cwd))) (compilation-start (plist-get workspace :run)))
      (amirreza-run (read-directory-name "[Run] Directory: ") t))))

(defun amirreza-workspace-grep ()
  "Runs amirreza-grep inside workspace :cwd"
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (save-some-buffers t nil)
    (if (and workspace (plist-get workspace :cwd))
	(let ((default-directory (plist-get workspace :cwd))) (amirreza-grep default-directory (read-string "[Workspace] Search: ") t))
      (call-interactively 'amirreza-grep))))

(defun amirreza-workspace-find-files ()
  (interactive)
  (unless (executable-find "rg") (error "amirreza-workspace-find-files needs ripgrep."))
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file))
	 (default-directory (plist-get workspace :cwd))
	 (relfile (completing-read (format "[%s] Files: " (or (plist-get workspace :name) "Workspace")) (string-split (string-trim (shell-command-to-string "rg --files") "\n" "\n") "\n")))
	 (absfile (expand-file-name relfile default-directory)))
    (find-file absfile)))

(amirreza-workspace-reload-workspaces)


;;;; Git
(defun amirreza-git-status ()
  "Runs git status"
  (interactive)
  (amirreza-split-window t)
  (compilation-start "git status"))

(defun amirreza-git-diff ()
  "Runs git diff"
  (interactive)
  (amirreza-split-window t)

  (compilation-start "git diff" 'diff-mode))

(defun amirreza-git-diff-staged ()
  "Runs git diff --staged"
  (interactive)
  (amirreza-split-window t)

  (compilation-start "git diff --staged" 'diff-mode))

(defun amirreza-git-diff-HEAD ()
  "Runs git diff HEAD"
  (interactive)
  (amirreza-split-window t)
  (compilation-start "git diff HEAD"))

(defalias 'gdiff 'amirreza-git-diff)
(defalias 'gdiffh 'amirreza-git-diff-HEAD)
(defalias 'gdiffs 'amirreza-git-diff-staged)
(defalias 'gstatus 'amirreza-git-status)


;;;; Expansions
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search nil)
(setq amirreza-expansions '(("TO" . "TODO(amirreza): ")
			    ("IM" . "IMPORTANT(amirreza): ")
			    ("ST" . "STUDY(amirreza): ")
			    ("NO"   . "NOTE(amirreza): ")))

(defun amirreza-expand ()
  "First try with amirreza-expansions and then try emacs dabbrev-expand."
  (interactive)
  (let* ((word (current-word))
	(expansion (alist-get word amirreza-expansions nil nil 'string-equal)))
    (if expansion
	;; expand snippet
	(progn
	  (backward-delete-char (length word))
	  (insert expansion))
      (call-interactively 'dabbrev-expand))))


;;;; Programming
(install 'go-mode)
(defun amirreza-go-fmt (&optional BUFFER)
  (interactive (list (current-buffer)))
  (let* ((BUFFER (or BUFFER (current-buffer)))
	 (TEMP (get-buffer-create "*gofmt-temp*"))
	 (_ (with-current-buffer TEMP (erase-buffer)))
	 (exitstatus (call-process "gofmt" nil `(,TEMP nil) nil (buffer-file-name BUFFER)))
	 (oldpoint (point))
	 )
    (when (= exitstatus 0)
      (with-current-buffer BUFFER
	(erase-buffer)
	(insert-buffer-substring TEMP)
	(goto-char oldpoint)
	(set-buffer-modified-p nil)))))

(defun amirreza-go-hook ()
  (interactive)
  (add-hook 'after-save-hook 'amirreza-go-fmt 0 t))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'amirreza-go-hook))

(with-eval-after-load 'go-ts-mode
  (add-hook 'go-ts-mode-hook 'amirreza-go-hook))

(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++


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

;;;; Treesitter
(when has-treesitter
  ;; IMPORTANT(amirreza): This sections needs both Emacs >29 and also a CC compiler. 
  (setq treesit-language-source-alist
	'((go "https://github.com/tree-sitter/tree-sitter-go")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
	  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")))

  (mapc (lambda (LANG) (unless (treesit-language-available-p LANG) (treesit-install-language-grammar LANG))) (mapcar #'car treesit-language-source-alist))
  (setq major-mode-remap-alist '())

  (setq major-mode-remap-alist
	'((yaml-mode . yaml-ts-mode)
	  ;; TODO(amirreza): Fix indentation style of C.
	  ;; (c++-mode  . c++-ts-mode)
	  ;; (c-mode    . c-ts-mode)
	  (go-mode   . go-ts-mode)
	  (json-mode . json-ts-mode)))


  ;; C/C++ syntax style
  (setq c-ts-mode-indent-offset 4)                                                                                        
  (setq c-ts-mode-indent-style 'linux))


;;; Keybindings
(global-set-key (kbd "C-c c")                                        'amirreza-copy)
(global-set-key (kbd "C-c x")                                        'amirreza-cut)
(global-set-key (kbd "C-c v")                                        'yank)
(global-set-key (kbd "M-w")                                          'amirreza-copy)
(global-set-key (kbd "C-w")                                          'amirreza-cut)
(global-set-key (kbd "M-k")                                          'kill-buffer)
(global-set-key (kbd "C-c J")                                        'amirreza-workspace-jump-to-workspace)
(global-set-key (kbd "C-c O")                                        'amirreza-workspace-open-workspaces-file)
(global-set-key (kbd "C-c R")                                        'amirreza-workspace-reload-workspaces)
(global-set-key (kbd "C-c m")                                        'amirreza-workspace-grep)
(global-set-key (kbd "C-c f")                                        'amirreza-workspace-find-files)
(global-set-key (kbd "M-m")                                          'amirreza-workspace-build)
(global-set-key (kbd "C-M-m")                                        'amirreza-workspace-run)
(global-set-key (kbd "C-c ;")                                        'goto-line)
(global-set-key (kbd "C-c p")                                        'previous-error) ;; Move to previous error in compilation buffer
(global-set-key (kbd "C-c n")                                        'next-error)     ;; Move to next error in compilation buffer
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-p")                                          'jump-up) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-n")                                          'jump-down) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-i")                                          'imenu) ;; Symbols
(global-set-key (kbd "C-c C-SPC")                                    'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-c i")                  'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-c r")                  'string-rectangle))
(global-set-key (kbd "C-c h")                                        'previous-buffer)
(global-set-key (kbd "C-c l")                                        'next-buffer)
(global-set-key (kbd "C-0")                                          'delete-other-windows)
(global-set-key (kbd "M-0")                                          'delete-window)
(global-set-key (kbd "M-o")                                          'other-window)                     
(global-set-key (kbd "C-9")                                          'amirreza-split-window)
(global-set-key (kbd "M-[")                                          'kmacro-start-macro) ;; start recording keyboard macro.
(global-set-key (kbd "M-]")                                          'kmacro-end-macro) ;; end recording keyboard macro.
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "C-z")                                          'undo) ;; Sane undo key
(global-set-key (kbd "C-<return>")                                   'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "C-q")                                          'amirreza-expand) ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "M-r")                                          'query-replace) ;; Replace pattern with a string
(global-set-key (kbd "C-=")                                          (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--")                                          (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C-.")                                          'isearch-forward-thing-at-point)

;;;; Record times
(setq amirreza-emacs-init-took (* (float-time (time-subtract (float-time) amirreza-emacs-starting-time)) 1000))
(setq emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000))
(setq amirreza-emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza-emacs-init-took emacs-init-time-took))
(message amirreza-emacs-init-log-message)
