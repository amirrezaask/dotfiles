;; (setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 200) ;; 200 Megabytes
		  gc-cons-percentage 0.2)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name))

(setq native-comp-async-report-warnings-errors nil)
(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.

(setq is-windows (eq system-type 'windows-nt)                                      
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

(defun amirreza/edit-init ()
  (interactive)
  (find-file INIT-FILE))

(global-unset-key (kbd "C-x C-c"))

(require 'package)
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))
(dolist (pkg '(so-long multiple-cursors vlf vterm magit go-mode php-mode rust-mode corfu eglot wgrep)) (package-install pkg)) ;; Install packages.

(setq image-types (cons 'svg image-types) mac-command-modifier 'meta) ;; Fix macos fucked up bugs.

(setq recenter-positions '(middle))       ;; When doing C-l always recenter.
(setq custom-file "~/.custom.el")         ;; set custom file to not meddle with init.el
(setq make-backup-files nil)              ;; no emacs ~ backup files
(setq vc-follow-symlinks t)               ;; Don't prompt if encounter a symlink file, just follow the link.
(set-default-coding-systems 'utf-8)       ;; always use UTF8
(setq kill-whole-line t)                  ;; kill line and newline char
(global-auto-revert-mode +1)              ;; Revert buffer to disk state when disk changes under our foot.
(delete-selection-mode)                   ;; when selected a text and user types delete text
(global-display-line-numbers-mode +1)     ;; Display line numbers.
(toggle-truncate-lines +1)
(global-so-long-mode +1)
(require 'vlf-setup)

(defun amirreza/copy () "Either copy region or the current line." (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
    (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun amirreza/cut () "Either cut region or the current line." (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)) ;; copy active region contents
    (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line


(if is-macos (set-face-attribute 'default nil :font "Menlo-15"))
(if is-windows (set-face-attribute 'default nil :font "Consolas-15"))

(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path "w:/bin")
(add-to-list 'exec-path "c:/programs/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH


(defun amirreza/vterm-dwim () "Jump to vterm buffer associated with current project or create a new." (interactive)
  (let* ((root (find-project-root-or-default-directory))
	 (default-directory root)
	 (buffer-name (format "*vterm-%s*" root)))
    (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
      (vterm buffer-name))))

(setq eshell-visual-subcommands '("git" "diff" "log" "show"))
(defun amirreza/eshell-dwim () "Jump to eshell buffer associated with current project or create a new." (interactive)
  (let* ((root (find-project-root-or-default-directory))
	 (default-directory root)
	 (eshell-buffer-name (format "*eshell-%s*" root)))
    (if (get-buffer eshell-buffer-name)
	(switch-to-buffer eshell-buffer-name)
      (eshell))))

(setq completion-styles '(flex initials shorthand substring basic))
(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion))

;; naysayer
;; (custom-set-faces
;;   `(default                          ((t (:foreground \"#debe95\" :background \"#202020\"))))
;;   `(hl-line                          ((t (:background \"#353535\"))))
;;   `(vertico-current                  ((t (:background \"#353535\"))))
;;   `(region                           ((t (:background \"medium blue\"))))
;;   `(cursor                           ((t (:background \"lightgreen\"))))
;;   `(font-lock-keyword-face           ((t (:foreground \"#d4d4d4\"))))
;;   `(font-lock-type-face              ((t (:foreground \"#8cde94\"))))
;;   `(font-lock-constant-face          ((t (:foreground \"#7ad0c6\"))))
;;   `(font-lock-variable-name-face     ((t (:foreground \"#c8d4ec\"))))
;;   `(font-lock-builtin-face           ((t (:foreground \"white\"))))
;;   `(font-lock-string-face            ((t (:foreground \"gray70\"))))
;;   `(font-lock-comment-face           ((t (:foreground \"yellow\"))))
;;   `(font-lock-comment-delimiter-face ((t (:foreground \"yellow\"))))
;;   `(font-lock-doc-face               ((t (:foreground \"#3fdf1f\"))))
;;   `(font-lock-function-name-face     ((t (:foreground \"white\"))))
;;   `(font-lock-doc-string-face        ((t (:foreground \"#3fdf1f\"))))
;;   `(font-lock-warning-face           ((t (:foreground \"yellow\"))))
;;   `(font-lock-note-face              ((t (:foreground \"khaki2\" ))))
;;   `(mode-line                        ((t (:foreground \"black\" :background \"#d3b58d\"))))
;;   `(mode-line-inactive               ((t (:background \"gray20\" :foreground \"#ffffff\"))))
;;   `(show-paren-match                 ((t (:background \"mediumseagreen\")))))


;; witness
(custom-set-faces
 `(default                          ((t (:foreground "#d3b58d" :background "#072626"))))
 `(hl-line                          ((t (:background "#0c4141"))))
 `(region                           ((t (:background  "medium blue"))))
 `(cursor                           ((t (:background "lightgreen"))))
 `(font-lock-keyword-face           ((t (:foreground "white"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "lightgreen"))))
 `(font-lock-string-face            ((t (:foreground "#0fdfaf"))))
 `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
 `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
 `(hightlight                       ((t (:foreground "navyblue" :background "darkseegreen2"))))
 `(font-lock-warning-face           ((t (:foreground "#504038"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))

(defun find-project-root ()
  "Try to find project root based on deterministic predicates"
  (cond
   ((eq major-mode 'go-mode)                                (locate-dominating-file default-directory "go.mod"))
   ((or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))  (locate-dominating-file default-directory "build.bat"))
   (t                                                       (locate-dominating-file default-directory ".git"))))

(defun git-repo-p (DIR) (locate-dominating-file DIR ".git"))

(defun find-project-root-or-default-directory () (or (find-project-root) default-directory))

;; $ go install golang.org/x/tools/gopls@latest
;; $ rustup component add rust-analyzer
;; $ npm install -g intelephense

(setq-default c-default-style "linux" c-basic-offset 4) ;; C/C++ code style

(setq corfu-auto nil)
(global-corfu-mode +1)

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

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(setq eglot-stay-out-of '(project))
(setq eglot-sync-connect nil)

(dolist (mode '(go rust php)) ;; Enable LSP for these languages
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))

(defun amirreza/compile-buffer-name-function (MODESTR)
  (let ((dir (find-project-root-or-default-directory)))
    (format "*%s-%s*" MODESTR dir)))

(setq-default compilation-buffer-name-function 'amirreza/compile-buffer-name-function)

(defun guess-compile-command (DIR)
  (let ((default-directory DIR))
    (cond
     ((file-exists-p "build.bat") "build.bat")
     ((file-exists-p "go.mod")    "go build -v ./..."))))

(defun amirreza/compile-in-directory (DIR COMMAND) "Compile in a directory" (interactive (list
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


(with-eval-after-load 'grep
  (when (executable-find "rg")
    (grep-apply-setting 'grep-command "rg --no-heading --color='never'")
    (grep-apply-setting 'grep-use-null-device nil)))

(defun amirreza/grep-in-directory (DIR)
  "Run appropriate grep program in directory."
  (interactive (list
		(read-directory-name "[Grep] Directory: " (find-project-root-or-default-directory))))

  (let ((default-directory DIR))
  (cond
   ((executable-find "rg") (grep (format "rg --no-heading --color='never' %s" (read-string "Ripgrep: "))))
   ((git-repo-p DIR) (grep (format "git grep --no-color -n %s" (read-string "Git Grep: ")))))))
  
(defun amirreza/grep-dwim ()
  "DWIM version of amirreza/grep-in-directory"
  (interactive)
  (cond
   ((equal (length (find-project-root)) 0) (call-interactively 'amirreza/grep-in-directory)) ;; we are not inside a project so we should ask user for directory.
   (t (amirreza/grep-in-directory (find-project-root)))))

(defun amirreza/git-files (DIR)
  "run git ls-files to get list of files indexed in git."
  (interactive (list (read-directory-name "[Git Files] Directory: " (find-project-root-or-default-directory))))
  (unless (executable-find "git") (error "git executable not found."))

  (let* ((default-directory DIR)
	 (command (format "git ls-files"))
	 (file (completing-read "Git Files: " (string-split (shell-command-to-string command) "\n" t))))
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
   ((executable-find "rg") (amirreza/rg-files DIR))
   ((git-repo-p DIR) (amirreza/git-files DIR))
   (t (call-interactively 'find-file))))

(defun amirreza/find-file-dwim ()
  "DWIM version of 'amirreza/find-file-in-directory'."
  (interactive)
  (cond
   ((equal (length (find-project-root)) 0) (call-interactively 'amirreza/find-file-in-directory)) ;; we are not inside a project so we should ask user for directory.
   (t (amirreza/find-file-in-directory (find-project-root)))))


(global-set-key (kbd "C-o")                                          'find-file)
(global-set-key (kbd "C-S-o")                                        'amirreza/grep-dwim)
(global-set-key (kbd "C-M-o")                                        'amirreza/grep-in-directory)
(global-set-key (kbd "M-o")                                          'amirreza/find-file-dwim)
(global-set-key (kbd "C-x i")                                        'amirreza/edit-init)
(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "C-c C-p")                          'wgrep-toggle-readonly-area)
  (define-key grep-mode-map (kbd "<f5>")                             'recompile)
  (define-key grep-mode-map (kbd "g")                                'recompile)
  (define-key grep-mode-map (kbd "G")                                'amirreza/grep-in-directory)
  (define-key grep-mode-map (kbd "M-q")                              'previous-buffer)
  (define-key grep-mode-map (kbd "M-n")                              'jump-down)
  (define-key grep-mode-map (kbd "M-p")                              'jump-up)
  (define-key grep-mode-map (kbd "k")                                'kill-compilation))

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
(global-set-key (kbd "M-j")                                          'amirreza/eshell-dwim)
(global-set-key (kbd "C-M-j")                                        'amirreza/vterm-dwim)

(global-set-key (kbd "C-.")                                          'next-buffer)
(global-set-key (kbd "C-,")                                          'previous-buffer)

(global-set-key (kbd "C-x g")                                        'magit)                     ;; Git Client

(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>")                     'act))                      ;; in replace mode <return> applies.
(global-set-key (kbd "C-q")                                          'set-mark-command)          ;; select
(global-set-key (kbd "C-w")                                          'amirreza/cut)              ;; sane cut/kill
(global-set-key (kbd "M-w")                                          'amirreza/copy)             ;; sane copy
(global-set-key (kbd "M-[")                                          'kmacro-start-macro)        ;; start macro
(global-set-key (kbd "M-]")                                          'kmacro-end-or-call-macro)  ;; end macro
(global-set-key (kbd "M-\\")                                         'kmacro-end-and-call-macro) ;; call keyboard macro.
(global-set-key (kbd "C-z")                                          'undo)                      ;; Sane undo key
(global-set-key (kbd "M-r")                                          'replace-string)            ;; replace strings ( no regex )
(global-set-key (kbd "C-M-r")                                        'query-replace)             ;; replace using regex

(global-set-key (kbd "C-M-n")                                        'mc/mark-next-like-this-word)
(global-set-key (kbd "C-M-p")                                        'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-S-n")                                        'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p")                                        'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a")                                      'mc/mark-all-like-this-dwim)

(defun jump-up () (interactive) (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))
(global-set-key (kbd "M-i")                                          'imenu)
(global-set-key (kbd "M->")                                          'end-of-buffer)
(global-set-key (kbd "M-<")                                          'beginning-of-buffer)
(global-set-key (kbd "M-n")                                          'jump-down)
(global-set-key (kbd "M-p")                                          'jump-up)
(global-set-key (kbd "C-;")                                          'goto-line)
(global-set-key (kbd "C-0")                                          'delete-window)
(global-set-key (kbd "C-3")                                          'split-window-right)
(global-set-key (kbd "C-2")                                          'split-window-below)
(global-set-key (kbd "C-j")                                          'completion-at-point)

(global-set-key (kbd "M-.")                                          'xref-find-definitions)
(global-set-key (kbd "C-M-.")                                        'xref-find-references)
(global-set-key (kbd "M-,")                                          'xref-go-back)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r")                         'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-o")                         'eglot-organize-imports)
  (define-key eglot-mode-map (kbd "C-c C-c")                         'eglot-code-actions))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-;")                           'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-'")                           'flymake-goto-next-error))

(global-set-key (kbd "C-x SPC")                                      'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")                    'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")                    'string-rectangle))

