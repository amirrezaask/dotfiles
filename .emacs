(setq frame-inhibit-implied-resize t) ;; Don't let emacs to resize frame when something inside changes
(setq gc-cons-threshold 200000000) ;; 200 MB
(setq debug-on-error t) ;; debug on error
(setq vc-follow-symlinks t) ;; Follow symlinks with no questions
(setq ring-bell-function (lambda ())) ;; no stupid sounds
(setq custom-file "~/.custom.el") ;; set custom file to not meddle with init.el
(setq make-backup-files nil) ;; no emacs ~ backup files
(setq is-windows (eq system-type 'windows-nt))
(setq is-linux (eq system-type 'gnu-linux))
(setq is-macos (eq system-type 'darwin))
(defun edit-init ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-x i") 'edit-init)
;; package manager setup
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)) ;; package manager setup
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq native-comp-async-report-warnings-errors 'silent) ;; silent native compilation warns
(setq use-short-answers t) ;; 
(setq image-types (cons 'svg image-types)) ;; macos bug
(setq mac-command-modifier 'meta) ;; macos again
;; Font stuff
(setq --font-family "")
(defun load-font (font fontsize)
  "Loads a font."
  (interactive (list (read-string "Font Family: ") (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" font fontsize)))
    (setq --font-family font)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(defun set-font-size (fontsize)
  "Set a font size"
  (interactive (list (read-number "Font Size: ")))
  (let ((fontstring (format "%s %d" --font-family fontsize)))
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-frame-font fontstring nil t)
    (set-face-attribute 'default t :font fontstring)))

(load-font "Consolas" 11)
;; environment variables env
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
(setq recenter-positions '(middle))
(setq custom-safe-themes t) ;; all themes are safe, don't ask
(setq inhibit-startup-screen t) ;; disable default start screen
(set-frame-parameter nil 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; always start frames maximized
(setq-default frame-title-format '("%e" (:eval default-directory)))
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable tool bar
(setq kill-whole-line t) ;; kill line and newline char
(global-auto-revert-mode +1) ;; auto refresh buffers from disk
(delete-selection-mode) ;; when selected a text and user types delete text

(custom-set-faces
 ;; `(default ((t (:foreground "#d3b58d" :background "#072626"))))
 `(default ((t (:foreground "#d3b58d" :background "#161616"))))
 `(cursor ((t (:background "lightgreen"))))
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
 `(mode-line ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive ((t (:foreground "black" :background "white"))))
 `(vertico-current ((t (:background "blue3"))))
 `(error ((t (:background "black" :foreground "red")))))

;; Language modes
(straight-use-package 'go-mode)
(straight-use-package 'php-mode)

;; Compiling stuff
(defun compile-directory (DIR)
  "Compile in a directory"
  (interactive (list (read-directory-name "[Compile] Directory: ")))
  (let ((default-directory DIR))
    (call-interactively 'compile)))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "k") 'kill-compilation))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; Searching stuff
(defun amirreza/rg (dir pattern)
  "run Ripgrep"
  (interactive (list (read-directory-name "[Ripgrep] Directory: ") (read-string "[Ripgrep] Pattern: ")))
  (let* ((default-directory dir)
	 (command (format "rg --vimgrep \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza/ug (dir pattern)
  (interactive (list (read-directory-name "[ug] Directory: ") (read-string "[ug] Pattern: ")))
  (let* ((default-directory dir)
	 (command (format "ug --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza/gnu-grep (dir pattern)
  (interactive (list (read-directory-name "[grep] Directory: ") (read-string "[grep] Pattern: ")))
  (let* ((default-directory dir)
	 (command (format "grep --exclude-dir=\".git\" --color=auto -nH --null -r -e \"%s\" ." pattern)))
    (compilation-start command 'grep-mode)))

(defun amirreza/grep-command ()
  (interactive)
  (cond
   ((or (executable-find "rg") is-windows) (call-interactively 'amirreza/rg))
   ((executable-find "ug") (call-interactively 'amirreza/ug))
   (t (call-interactively 'amirreza/gnu-grep))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "<f5>") 'recompile)
  (define-key grep-mode-map (kbd "k") 'kill-compilation))

;; Keymaps
(global-set-key (kbd "M-o") 'find-file)
(global-set-key (kbd "C-/") 'amirreza/grep-command) ;; Magical search
(global-set-key (kbd "<f5>") 'compile-directory) ;; |> little green button of my IDE
(global-set-key (kbd "C-:") 'compile-directory) ;; |> button
(global-set-key (kbd "C-z") 'undo) ;; sane undo key
(global-set-key (kbd "C-<return>") 'save-buffer) ;; Save with one combo not C-x C-s shit
(global-set-key (kbd "M-[") 'kmacro-start-macro-or-insert-counter) ;; start recording keyboard macro..
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro-repeat) ;; end recording keyboard macro.
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "M-=") 'split-window-vertically)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-q") 'dabbrev-expand) ;; expand current word with suggestions from all buffers.
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; rebind exit key to just kill frame if possible
(global-set-key (kbd "M-p") 'jump-up)
(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))
