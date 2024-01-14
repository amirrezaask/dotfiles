;; TODO(amirreza): instead of compilation-start we should use our own function like we did on amirreza-git-* so we can control window stuff.
(setq amirreza-emacs-starting-time (float-time)) ;; Store current time for further analysis.
(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq initial-scratch-message "") ;; No starting text in *scratch* buffer.
(setq gc-cons-threshold 200000000) ;; 200 MB for the GC threshold
(setq redisplay-dont-pause t)
(setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(global-unset-key (kbd "C-x C-c"))
(setq is-windows (eq system-type 'windows-nt))
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(setq has-treesitter (>= emacs-major-version 29))
(unless (executable-find "rg") (error "Install rigprep, this configuration relies heavy on it's features."))

(defun edit-init ()
  (interactive)
  (cond
   (is-windows (find-file "W:\\dotfiles\\.emacs"))
   (t  (find-file "~/w/dotfiles/.emacs"))))

(global-set-key (kbd "C-x i") 'edit-init)
(setq use-short-answers t) ;; Always prefer short answers
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again

(setq amirreza-split-window-horizontal-vertical-threshold 250)

(defun amirreza-split-window (&optional SWITCH-TO)
  "Split window based on 'amirreza-split-window-horizontal-vertical-threshold'"
  (interactive)
  (if (> (frame-width nil) amirreza-split-window-horizontal-vertical-threshold)
      (progn
	(delete-other-windows)
	(split-window-horizontally)
	(if SWITCH-TO (other-window 1))
	)
    (progn
      (delete-other-windows)
      (split-window-vertically)
      (if SWITCH-TO (other-window 1)))))

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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(defun install (PKG) (unless (package-installed-p PKG) (package-install PKG)))
(unless package-archive-contents (package-refresh-contents))
(install 'php-mode)
(install 'yaml-mode)
(install 'json-mode)
(install 'go-mode)
(install 'orderless)

;; Minibuffer completion
(setq completion-cycle-threshold 5)
(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-o") 'switch-to-completions))

;; Smarter completion strategy
(setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

;; Highlight todos
(setq hl-todo-modes '(c-mode c++-mode go-mode emacs-lisp))
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

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

(setq split-window-preferred-function (lambda ())) ;; Don't change my windows Emacs, please
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

(defun amirreza-build (DIR &optional SPLIT)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Build] Directory: ")))
  (let ((default-directory DIR)
	(command (read-string "[Build] Command:")))
    (if SPLIT (amirreza-split-window))
    (compilation-start command)))

(defun amirreza-run (DIR &optional SPLIT)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Run] Directory: ")))
  (let ((default-directory DIR)
	(command (read-string "[Run] Command:")))
    (if SPLIT (amirreza-split-window))
    (compilation-start command)))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; G/RE/P aka GREP
(defun rg (dir pattern)
  "run Ripgrep"
  (interactive (list (read-directory-name "[Ripgrep] Directory: ") (read-string "[Ripgrep] Pattern: ")))
  (unless (executable-find "rg") (error "ripgrep executable not found, install from https://github.com/BurntSushi/ripgrep/releases"))
  (amirreza-split-window)

  (let* (
	 (default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: ") (read-string "[grep] Pattern: ")))
  (unless (executable-find "ug") (error "Gnu Grep executable not found"))
  (amirreza-split-window)
  (let* (
	 (default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza-grep (dir pattern &optional SPLIT)
  ""
  (interactive (list (read-directory-name "[Grep] Directory: ") (read-string "[Grep] Pattern: ")))
  (if SPLIT (amirreza-split-window))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   (t (gnu-grep dir pattern))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))

;;; Workspace Layer ;;;
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
    (if workspace
	(let ((default-directory (plist-get workspace :cwd))) (amirreza-split-window) (compilation-start (plist-get workspace :build)))
      (amirreza-build (read-directory-name "[Build] Directory: ") t))))

(defun amirreza-workspace-run ()
  "Runs :run command of workspace inside :cwd."
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (save-some-buffers t nil)
    (if workspace
	(let ((default-directory (plist-get workspace :cwd))) (amirreza-split-window) (compilation-start (plist-get workspace :run)))
      (amirreza-run (read-directory-name "[Run] Directory: ") t))))

(defun amirreza-workspace-grep ()
  "Runs amirreza-grep inside workspace :cwd"
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (save-some-buffers t nil)
    (if workspace
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

(defun amirreza-git-status ()
  "Runs git status"
  (interactive)
  (let* ((buffer (get-buffer-create "*gitstatus*")))
    (amirreza-split-window t)
    (start-process "git status" buffer "git" "status")
    (set-window-buffer nil buffer)
    (with-current-buffer buffer (diff-mode))))

(defun amirreza-git-diff ()
  "Runs git diff"
  (interactive)
  (let* ((buffer (get-buffer-create "*diff*")))
    (amirreza-split-window t)
    (start-process "git diff" buffer "git" "diff")
    (set-window-buffer nil buffer)
    (with-current-buffer buffer (diff-mode))))

(defun amirreza-git-diff-staged ()
  "Runs git diff"
  (interactive)
  (let* ((buffer (get-buffer-create "*diff*")))
    (amirreza-split-window t)
    (start-process "git diff staged" buffer "git" "diff" "--staged")
    (set-window-buffer nil buffer)
    (with-current-buffer buffer (diff-mode))))

(defun amirreza-git-diff-HEAD ()
  "Runs git diff"
  (interactive)
  (let* ((buffer (get-buffer-create "*diff*")))
    (amirreza-split-window t)
    (start-process "git diff staged" buffer "git" "diff" "HEAD")
    (set-window-buffer nil buffer)
    (with-current-buffer buffer (diff-mode))))

(defalias 'gdiff 'amirreza-git-diff)
(defalias 'gdiffh 'amirreza-git-diff-HEAD)
(defalias 'gdiffs 'amirreza-git-diff-staged)
(defalias 'gstatus 'amirreza-git-status)

;; EXPANSIONS aka Snippets
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

;; Programming
(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++

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
	(goto-char oldpoint))))
      )

(defun amirreza-go-hook ()
  (interactive)
  (add-hook 'before-save-hook 'amirreza-go-fmt 0 t))

(with-eval-after-load 'go-modeo
  (add-hook 'go-mode-hook 'amirreza-go-hook))

(with-eval-after-load 'go-ts-mode
  (add-hook 'go-ts-mode-hook 'amirreza-go-hook))

;; Color My Emacs
(defun handmadehero-theme ()
  (interactive)
  (global-hl-line-mode +1)
  (let ((background          "#161616")
	(highlight           "midnight blue")
	(region              "medium blue")
	(text                "#cdaa7d")
	(keyword             "DarkGoldenrod3")
	(comment             "gray50")
	(string              "olive drab")
	(variable            "burlywood3")
	(warning             "#504038")
	(constant            "olive drab")
	(cursor              "green")
	(function            "burlywood3")
	(macro               "#8cde94")
	(punctuation         "burlywood3")
	(builtin             "#DAB98F"))

    (custom-set-faces
     `(default                          ((t (:foreground ,text :background ,background))))
     `(cursor                           ((t (:background ,cursor))))
     `(font-lock-keyword-face           ((t (:foreground ,keyword))))
     `(font-lock-type-face              ((t (:foreground ,punctuation))))
     `(font-lock-constant-face          ((t (:foreground ,constant))))
     `(font-lock-variable-name-face     ((t (:foreground ,variable))))
     `(font-lock-builtin-face           ((t (:foreground ,builtin))))
     `(font-lock-string-face            ((t (:foreground ,string))))
     `(font-lock-comment-face           ((t (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
     `(font-lock-doc-face               ((t (:foreground ,comment))))
     `(font-lock-function-name-face     ((t (:foreground ,function))))
     `(font-lock-doc-string-face        ((t (:foreground ,string))))
     `(font-lock-preprocessor-face      ((t (:foreground ,macro))))
     `(font-lock-warning-face           ((t (:foreground ,warning))))
     `(region                           ((t (:background ,region))))
     `(hl-line                          ((t (:background ,highlight))))
     `(vertico-current                  ((t (:inherit hl-line))))
     `(mode-line                        ((t (:background "#ffffff" :foreground "#000000"))))
     `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
     `(show-paren-match                 ((t (:background "burlywood3" :foreground "black"))))
     `(highlight                        ((t (:foreground nil :background ,region)))))))

(defun jonathan-blow-theme ()
  (interactive)
  (global-hl-line-mode -1)
  (custom-set-faces
   `(default                          ((t (:foreground "#d3b58d" :background "#072626"))))
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
   `(show-paren-match                 ((t (:background "mediumseagreen"))))))

(defun casey-muratori-theme ()
  (interactive)
  (global-hl-line-mode +1)
  (let ((background  "#0C0C0C")
	(highlight   "#171616")
	(region      "#2f2f37")
	(text        "#a08563")
	(keyword     "#f0c674")
	(comment     "#686868")
	(string      "#6b8e23")
	(variable    "#b99468")
	(warning     "#504038")
	(constant    "#6b8e23")
	(cursor      "#EE7700")
	(function    "#cc5735")
	(macro       "#dab98f")
	(type        "#d8a51d")
	(operator    "#907553")
	(modeline-foreground "#cb9401")
	(modeline-background "#1f1f27")
	(paren-match-foreground "#000000")
	(paren-match-background "#e0741b")
	(punctuation "#907553") ;; 
	(bracket     "#907553") ;; [] {} ()
	(delimiter   "#907553") ;; ; :
	(builtin     "#DAB98F"))

    (custom-set-faces
     `(default                          ((t (:foreground ,text :background ,background))))
     `(cursor                           ((t (:background ,cursor))))
     `(font-lock-keyword-face           ((t (:foreground ,keyword))))
     `(font-lock-operator-face          ((t (:foreground ,operator))))
     `(font-lock-punctuation-face       ((t (:foreground ,punctuation))))
     `(font-lock-bracket-face           ((t (:foreground ,bracket))))
     `(font-lock-delimiter-face         ((t (:foreground ,delimiter))))
     `(font-lock-type-face              ((t (:foreground ,type))))
     `(font-lock-constant-face          ((t (:foreground ,constant))))
     `(font-lock-variable-name-face     ((t (:foreground ,variable))))
     `(font-lock-builtin-face           ((t (:foreground ,builtin))))
     `(font-lock-string-face            ((t (:foreground ,string))))
     `(font-lock-comment-face           ((t (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
     `(font-lock-doc-face               ((t (:foreground ,comment))))
     `(font-lock-function-name-face     ((t (:foreground ,function))))
     `(font-lock-doc-string-face        ((t (:foreground ,string))))
     `(font-lock-preprocessor-face      ((t (:foreground ,macro))))
     `(font-lock-warning-face           ((t (:foreground ,warning))))
     `(region                           ((t (:background ,region))))
     `(hl-line                          ((t (:background ,highlight))))
     `(vertico-current                  ((t (:inherit hl-line))))
     `(highlight                        ((t (:foreground nil :background ,region))))
     `(mode-line                        ((t (:foreground ,modeline-foreground :background ,modeline-background))))
     `(mode-line-inactive               ((t (:foreground ,modeline-foreground :background ,modeline-background))))
     `(minibuffer-prompt                ((t (:foreground ,text) :bold t)))
     `(show-paren-match                 ((t (:background ,paren-match-background :foreground ,paren-match-foreground)))))))


(jonathan-blow-theme)


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

;; Keybindings section
;; NOTE(amirreza): All keys preferably should be prefixed on C-c
(global-set-key (kbd "C-c c")                                        'amirreza-copy)
(global-set-key (kbd "C-c x")                                        'amirreza-cut)
(global-set-key (kbd "C-c v")                                        'yank)
(global-set-key (kbd "M-w")                                          'amirreza-copy)
(global-set-key (kbd "C-w")                                          'amirreza-cut)
(global-set-key (kbd "C-c J")                                        'amirreza-workspace-jump-to-workspace)
(global-set-key (kbd "C-c O")                                        'amirreza-workspace-open-workspaces-file)
(global-set-key (kbd "C-c R")                                        'amirreza-workspace-reload-workspaces)
(global-set-key (kbd "C-c m")                                        'amirreza-workspace-grep)
(global-set-key (kbd "C-c b")                                        'amirreza-workspace-build)
(global-set-key (kbd "C-c f")                                        'amirreza-workspace-find-files)
(global-set-key (kbd "M-m")                                          'amirreza-workspace-build)
(global-set-key (kbd "C-M-m")                                        'amirreza-workspace-run)
;; Jump around					               
(global-set-key (kbd "C-c ;")                                        'goto-line)
(global-set-key (kbd "C-c p")                                        'previous-error) ;; Move to previous error in compilation buffer
(global-set-key (kbd "C-c n")                                        'next-error)     ;; Move to next error in compilation buffer
(global-set-key (kbd "C->")                                          'end-of-buffer)
(global-set-key (kbd "C-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-p")                                          'jump-up) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-n")                                          'jump-down) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-i")                                          'imenu) ;; Symbols
;; Rectangle mode
(global-set-key (kbd "C-c C-SPC")                                    'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-c i")                  'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-c r")                  'string-rectangle))
;; Buffer
(global-set-key (kbd "C-c h")                                        'previous-buffer)
(global-set-key (kbd "C-c l")                                        'next-buffer)
;; Window stuff					                     
(global-set-key (kbd "C-0")                                          'delete-other-windows)
(global-set-key (kbd "M-o")                                          'other-window)                     
(global-set-key (kbd "C-9")                                          'amirreza-split-window)
;; Macros					                     
(global-set-key (kbd "M-[")                                          'kmacro-start-macro) ;; start recording keyboard macro.
(global-set-key (kbd "M-]")                                          'kmacro-end-macro) ;; end recording keyboard macro.
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; execute keyboard macro.
						                     
(global-set-key (kbd "C-z")                                          'undo) ;; Sane undo key
(global-set-key (kbd "C-<return>")                                   'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "C-q")                                          'amirreza-expand) ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "M-r")                                          'query-replace) ;; Replace pattern with a string
(global-set-key (kbd "C-=")                                          (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--")                                          (lambda () (interactive) (text-scale-decrease 1)))

;; Treesitter Layer
;; Emacs >29
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

;; Performance benchmark
(setq amirreza-emacs-init-took (* (float-time (time-subtract (float-time) amirreza-emacs-starting-time)) 1000))
(setq emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000))
(setq amirreza-emacs-init-log-message (format "Amirreza emacs init took %fms, Emacs init took: %fms" amirreza-emacs-init-took emacs-init-time-took))
(message amirreza-emacs-init-log-message)
