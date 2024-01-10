;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ___              _                            ___         __         ;;
;;    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__       ;;
;;   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/       ;;
;;  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<          ;;
;; /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|         ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq amirreza-emacs-starting-time (float-time)) ;; Store current time for further analysis.
(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB for the GC threshold
(setq redisplay-dont-pause t)
(setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(setq is-windows (eq system-type 'windows-nt))
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(defalias 'ifnot 'unless)
(defmacro ifwindows (&rest BODY)
  `(when is-windows ,@BODY))

(defmacro ifunix (&rest BODY)
  `(unless is-windows ,@BODY))

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

(defun amirreza-split-window ()
  "Split window based on 'amirreza-split-window-horizontal-vertical-threshold'"
  (interactive)
  (if (> (frame-width nil) amirreza-split-window-horizontal-vertical-threshold)
      (progn
	(delete-other-windows)
	(split-window-horizontally))
    (progn
      (delete-other-windows)
      (split-window-vertically))))

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

(add-to-list 'exec-path (home "bin")) ;; GOPATH/bin
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

(defun amirreza-compile-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Compile] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(defun amirreza-run-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Run] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;;; Workspace Layer ;;;
(defvar amirreza-workspaces '() "Workspace objects.")
(defvar amirreza-workspaces-file "~/emacs-workspaces" "Path to the workspace file.")
(defun amirreza-reload-workspaces ()
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
	    (add-to-list 'workspaces (plist-get (cdr workspace-obj) :name))
	    ) amirreza-workspaces)

    workspaces))

(defun amirreza-get-workspace-for-path (PATH) (alist-get PATH amirreza-workspaces nil nil 'string-match-p))

(defun amirreza-get-workspace-by-name (NAME)
  (let* ((workspace nil))
    (mapc (lambda (workspace-obj)
	    (if (string-equal (plist-get (cdr workspace-obj) :name) NAME) (setq workspace workspace-obj))
	    ) amirreza-workspaces)

    workspace))

(defun amirreza-open-workspace (NAME)
  (interactive (list (completing-read "[Workspace]: " (amirreza-list-workspaces))))
  (let* ((workspace (amirreza-get-workspace-by-name NAME))
	 (workspace (if workspace (cdr workspace))))
    (if workspace
	(find-file (plist-get workspace :cwd)))))

(defun amirreza-open-workspaces-file ()
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


(defun amirreza-build ()
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (when workspace
      (message "[%s] Build Command is '%s'" (plist-get workspace :name) (plist-get workspace :build))
      (message "[%s] Dir is '%s'" (plist-get workspace :name) (plist-get workspace :cwd)))
    (save-some-buffers t nil)
    (amirreza-split-window)
    (if workspace
	(let ((default-directory (plist-get workspace :cwd))) (compilation-start (plist-get workspace :build)))
      (call-interactively 'amirreza-compile-directory))))

(defun amirreza-run ()
  (interactive)
  (let* (
	 (file default-directory)
	 (workspace (amirreza-get-workspace-for-path file)))
    (when workspace
      (message "[%s] Run Command is '%s'" (plist-get workspace :name) (plist-get workspace :run))
      (message "[%s] Dir is '%s'" (plist-get workspace :name) (plist-get workspace :cwd)))
    (save-some-buffers t nil)
    (amirreza-split-window)
    (if workspace
	(let ((default-directory (plist-get workspace :cwd))) (compilation-start (plist-get workspace :run)))
      (call-interactively 'amirreza-run-directory))))



(amirreza-reload-workspaces)

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

(defun amirreza-grep (dir pattern)
  ""
  (interactive (list (read-directory-name "[Grep] Directory: ") (read-string "[Grep] Pattern: ")))
  (cond
   ((or (executable-find "rg") is-windows) (rg dir pattern))
   (t (gnu-grep dir pattern))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))

;; EXPANSIONS aka Snippets
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

(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++

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
     `(mode-line                        ((t (:background "#ffffff" :foreground "#000000"))))
     `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
     `(show-paren-match                 ((t (:background "burlywood3" :foreground "black"))))
     `(highlight                        ((t (:foreground nil :background ,region)))))))


(defun jonathan-blow-theme ()
  (interactive)
  (global-hl-line-mode +1)
  (custom-set-faces
   `(default                          ((t (:foreground "#d3b58d" :background "#072626"))))
   `(hl-line                          ((t (:background "#0c4141"))))
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
     `(highlight                        ((t (:foreground nil :background ,region))))
     `(mode-line                        ((t (:foreground "#cb9401" :background "#1f1f27"))))
     `(mode-line-inactive               ((t (:foreground "#cb9401" :background "#1f1f27"))))
     `(minibuffer-prompt                ((t (:foreground ,text) :bold t)))
     `(show-paren-match                 ((t (:background "#e0741b" :foreground "#000000")))))))


(jonathan-blow-theme)

;; Keybindings section
(global-set-key (kbd "M-o")                      'find-file)
(global-set-key (kbd "C-x w o")                  'amirreza-open-workspace)
(global-set-key (kbd "C-x w f")                  'amirreza-open-workspaces-file)
(global-set-key (kbd "C-x w r")                  'amirreza-reload-workspaces)
(global-set-key (kbd "C-.")                      'isearch-forward-thing-at-point)
(global-set-key (kbd "C-/")                      'amirreza-grep) ;; Magical search
(global-set-key (kbd "M-m")                      'amirreza-build) ;; |> button
(global-set-key (kbd "C-M-m")                    'amirreza-run) ;; |> button
(global-set-key (kbd "C-z")                      'undo) ;; Sane undo key
(global-set-key (kbd "C-0")                      'delete-other-windows)
(global-set-key (kbd "C-9")                      'amirreza-split-window)
(global-set-key (kbd "C-<return>")               'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "M-[")                      'kmacro-start-macro) ;; start recording keyboard macro.
(global-set-key (kbd "M-]")                      'kmacro-end-macro) ;; end recording keyboard macro.
(global-set-key (kbd "M-\\")                     'kmacro-end-and-call-macro) ;; execute keyboard macro.
(global-set-key (kbd "C-o")                      'other-window) ;; Switch window
(global-set-key (kbd "C-q")                      'amirreza-expand) ;; Try pre defined expansions and if nothing was found expand with emacs dabbrev
(global-set-key (kbd "C-x C-c")                  'delete-frame) ;; rebind exit key to just kill frame if possible
(global-set-key (kbd "M-p")                      'jump-up) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-n")                      'jump-down) ;; Jump through the buffer with preserving the cursor position in the center
(global-set-key (kbd "M-r")                      'query-replace) ;; Replace pattern with a string
(global-set-key (kbd "M-<up>")                   'scroll-down-command)
(global-set-key (kbd "M-<down>")                 'scroll-up-command)
(global-set-key (kbd "C-=")                      (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--")                      (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C->")                      'end-of-buffer)
(global-set-key (kbd "C-<")                      'beginning-of-buffer)
(global-set-key (kbd "M-i")                      'imenu) ;; Symbols
(global-set-key (kbd "M--")                      'previous-error) ;; Move to previous error in compilation buffer
(global-set-key (kbd "M-=")                      'next-error) ;; Move to next error in compilation buffer
(global-set-key (kbd "M-1")                      'amirreza-open-notes) ;; Open my local notes file
(global-set-key (kbd "C-S-SPC")                  'rectangle-mark-mode) ;; Toggle rectangle mode
(global-set-key (kbd "C-x r i")                  'string-insert-rectangle) ;; Rectangle insert
(global-set-key (kbd "C-x r r")                  'string-rectangle) ;; Rectangle replace
(global-set-key (kbd "C-{")                      'previous-buffer)
(global-set-key (kbd "C-}")                      'next-buffer)
(global-set-key (kbd "C-;")                      'goto-line)
(global-set-key (kbd "C-x C-SPC")                'rectangle-mark-mode)

(setq amirreza-emacs-init-took (* (float-time (time-subtract (float-time) amirreza-emacs-starting-time)) 1000))
(setq emacs-init-time-took (* (string-to-number (emacs-init-time "%f")) 1000))
(setq amirreza-emacs-init-log-message (format "Amirreza emacs init took %fms\nEmacs init took: %fms" amirreza-emacs-init-took emacs-init-time-took))
(setq amirreza-ascii-art "
     ___              _                            ___         __         
    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__       
   / /| | / __ `__ \\/ / ___/ ___/ _ \\/_  / / __ `/ /| | / ___/ //_/     
  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<          
 /_/  |_/_/ /_/ /_/_/_/  /_/   \\___/ /___/\\__,_/_/  |_/____/_/|_|       
                                                                          
"
)

(setq initial-scratch-message (format "%s\n\n\n\n%s\n" amirreza-ascii-art amirreza-emacs-init-log-message))
