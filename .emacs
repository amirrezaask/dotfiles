;; (setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.


;; Startup hacks to make emacs boot faster.
(defvar amirreza-emacs--file-name-handler-alist file-name-handler-alist)
(defvar amirreza-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 20)
                  gc-cons-percentage 0.1
                  file-name-handler-alist amirreza-emacs--file-name-handler-alist
                  vc-handled-backends     amirreza-emacs--vc-handled-backends)))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Enable packages index at startup
(setq package-enable-at-startup t)
(setq package-quickstart t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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
      is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu-linux)
      is-macos (eq system-type 'darwin)
      has-treesitter (>= emacs-major-version 29))

(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq redisplay-dont-pause t)

;; @PATH
(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "w:/bin")
(add-to-list 'exec-path "c:/programs/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(setq image-types (cons 'svg image-types) mac-command-modifier 'meta) ;; Fix macos fucked up bugs.

(defun edit-init () "Edit this file." (interactive) (find-file INIT-FILE))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name))

(global-set-key (kbd "C-x i") 'edit-init) ;; Edit this file.

(if is-windows
    (cd "C:\\w")
  (cd "~/w"))

;; overrides: minor mode to register keys that I want to override in all other modes.
(defvar global-overrides (make-sparse-keymap))
(define-minor-mode amirreza-overrides ""
  :global t
  :lighter " Overrides"
  :init-value t
  :keymap global-overrides)
(amirreza-overrides +1)
(defun GLOBAL (KBD ACTION) (define-key global-overrides KBD ACTION))

;; Font
(setq font-size 16)
(setq current-font-family "")
(setq font-families (font-family-list))
(require 'cl-lib)
(cl-loop for font in '(
                       "Monaspace Neon"
                       "Consolas"
                       "Liberation Mono"
                       "Menlo"
                       "JetBrains Mono"
                       "Intel One Mono"
                       )
         do
         (let* ((font-family (car (string-split font "-"))))
           (when (member font-family font-families)
             (setq current-font-family font)
             (set-face-attribute 'default nil :font (format "%s-%d" font font-size))
             (cl-return))))

(defun set-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq current-font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(setq native-comp-async-report-warnings-errors nil) ;; silently do jit compiling.

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(defun install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(dolist (pkg '(
               vlf       ;; handle [V]ery [L]arge [F]iles
               wgrep     ;; Editable Grep Buffers
               go-mode
               rust-mode
               php-mode
               json-mode
               yaml-mode
               )) (install pkg))

(defun split-window-right-balance-and-switch () (interactive)
       (split-window-right)
       (balance-windows)
       (other-window 1))

(defun split-window-below-balance-and-switch () (interactive)
       (split-window-below)
       (balance-windows)
       (other-window 1))

(defun delete-window-and-balance () (interactive)
       (delete-window)
       (balance-windows))

(defun jump-up ()   (interactive)   (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom))

(setq recenter-positions '(middle))
(GLOBAL          (kbd "C-<return>")  'save-buffer)
(GLOBAL          (kbd "C-0")         'delete-window-and-balance)
(GLOBAL          (kbd "C-1")         'delete-other-windows)
(GLOBAL          (kbd "C-3")         'split-window-right-balance-and-switch)
(GLOBAL          (kbd "C-\\")        'split-window-right-balance-and-switch)
(GLOBAL          (kbd "C-2")         'split-window-below-balance-and-switch)
(GLOBAL          (kbd "C--")         'text-scale-decrease)
(GLOBAL          (kbd "C-=")         'text-scale-increase)
(GLOBAL          (kbd "M--")         'text-scale-decrease)
(GLOBAL          (kbd "M-=")         'text-scale-increase)
(GLOBAL          (kbd "C-o")         'other-window)
(GLOBAL          (kbd "M-n")         'jump-down)
(GLOBAL          (kbd "M-p")         'jump-up)
(GLOBAL          (kbd "C-S-n")       'jump-down)
(GLOBAL          (kbd "C-S-p")       'jump-up)
(GLOBAL          (kbd "M-k")         'kill-current-buffer)

(defun kill-current-buffer () (interactive)
       (kill-buffer (current-buffer)))

(blink-cursor-mode -1)
(setq make-backup-files nil)              ;; no emacs ~ backup files
(setq vc-follow-symlinks t)               ;; Don't prompt if encounter a symlink file, just follow the link.
(set-default-coding-systems 'utf-8)       ;; always use UTF8
(global-auto-revert-mode +1)              ;; Auto revert to disk changes, do we really want this ??
(delete-selection-mode +1)                ;; Delete selected region before inserting.

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme." (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))

(global-set-key (kbd "M-RET") 'indent-buffer) ;; Format buffer

(global-set-key (kbd "C-/") 'comment-line) ;; Comment

(setq kill-whole-line t)

(defun copy () "Either copy region or the current line." (interactive)
       (if (use-region-p)
           (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
         (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun cut () "Either cut region or the current line." (interactive)
       (if (use-region-p)
           (kill-region (region-beginning) (region-end)) ;; copy active region contents
         (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line

(global-set-key (kbd "C-;")   'goto-line)
(global-set-key (kbd "C-w")   'cut) ;; modern cut
(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "M-z")   'undo)
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy

;; Unset keys that I dont use
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-l"))

(toggle-truncate-lines +1) ;; wrap long lines

(global-so-long-mode +1) ;; don't choke on minified code.

(require 'vlf-setup)

;; Minibuffer
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 15)
(setq completions-auto-select nil)
(setq completion-show-help nil)
(setq completion-styles '(basic flex partial-completion emacs22))

(defun minibuffer-choose-completion-and-exit () "" (interactive)
       (minibuffer-choose-completion t nil)
       (catch 'exit (exit-minibuffer)))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map           (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map           (kbd "C-p") 'minibuffer-previous-completion))


;; Completion
(global-set-key (kbd "C-j") 'completion-at-point)
(install 'corfu)
(global-corfu-mode +1)
(setq corfu-auto nil)

;; Colors
(install 'ef-themes)
(install 'fleetish-theme)
(defun save-theme (name definition)
  (mkdir (expand-file-name "themes" user-emacs-directory) t)
  (write-region (format "(deftheme %s)
%s
" name definition) nil (expand-file-name (format "%s-theme.el" name) (expand-file-name "themes" user-emacs-directory))))

;; Brownish
;; (custom-set-faces
;;   `(default                          ((t (:foreground "#debe95" :background "#282828"))))
;;   `(hl-line                          ((t (:background "#353535"))))
;;   `(vertico-current                  ((t (:background "medium blue"))))
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


;; Greenish
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

;; Handmadehero
;; (custom-set-faces
;;  `(default                          ((t (:foreground "burlywood2" :background "#161616"))))
;;  `(hl-line                          ((t (:background "midnight blue"))))
;;  `(vertico-current                  ((t (:background "midnight blue"))))
;;  `(region                           ((t (:background "medium blue"))))
;;  `(cursor                           ((t (:background "#40FF40"))))
;;  `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod2"))))
;;  `(font-lock-type-face              ((t (:foreground "burlywood3"))))
;;  `(font-lock-constant-face          ((t (:foreground "olive drab"))))
;;  `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
;;  `(font-lock-builtin-face           ((t (:foreground "gray80"))))
;;  `(font-lock-string-face            ((t (:foreground "olive drab"))))
;;  `(font-lock-comment-face           ((t (:foreground "gray50"))))
;;  `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
;;  `(font-lock-doc-face               ((t (:foreground "gray50"))))
;;  `(font-lock-function-name-face     ((t (:foreground "burlywood2"))))
;;  `(font-lock-doc-string-face        ((t (:foreground "gray50"))))
;;  `(font-lock-warning-face           ((t (:foreground "yellow"))))
;;  `(font-lock-note-face              ((t (:foreground "khaki2" ))))
;;  `(show-paren-match                 ((t (:background "mediumseagreen")))))

(setq-default c-default-style "linux" c-basic-offset 4)

;; Xref
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-?") 'xref-find-references)

(global-set-key (kbd "C-.") 'xref-find-definitions)
(global-set-key (kbd "C-,") 'xref-go-back)
(global-set-key (kbd "C-?") 'xref-find-references)

;; LSP (Eglot)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-i")     'consult-eglot-symbols)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(setq eldoc-echo-area-use-multiline-p nil)

(dolist (mode '(go rust php))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))


(setq eglot-ignored-server-capabilities '(
                                          :documentHighlightProvider
                                          :codeLensProvider
                                          :documentOnTypeFormattingProvider
                                          :documentLinkProvider
                                          :colorProvider
                                          :foldingRangeProvider
                                          :executeCommandProvider
                                          :inlayHintProvider))
(with-eval-after-load
    'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio"))))

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(setq eglot-stay-out-of '(project)) ;;
(setq eglot-sync-connect nil)       ;; no blocking on waiting for the server to start.
(setq eglot-events-buffer-size 0)   ;; no logging of LSP events.

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

(defun find-project-root () "Try to find project root based on deterministic predicates"
       (cond
        ((git-repo-p default-directory) (locate-dominating-file default-directory ".git"))
        ((eq major-mode 'go-mode)   (locate-dominating-file default-directory "go.mod"))
        ((eq major-mode 'php-mode)  (locate-dominating-file default-directory "composer.json"))
        (t                          default-directory)))


(defun git-repo-p (DIR) (locate-dominating-file DIR ".git"))
(defun find-project-root-or-default-directory () (or (find-project-root) default-directory))

(require 'project)

;; Compile Mode
(GLOBAL (kbd "M-m") 'compile-dwim)

(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "g")    'recompile) ;; g as always just does the same thing.
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t)))
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "M-m")  'previous-buffer)
  (define-key compilation-mode-map (kbd "k")    'kill-compilation))

;; Diff Mode
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "g") 'recompile))

(defun amirreza-compile-buffer-name-function (MODESTR) (format "*Compile-%s*" --compile-dir))
(defun amirreza-grep-buffer-name-function (MODESTR)    (format "*Grep-%s*" --grep-dir))

(defun git-add (file) (interactive (list (read-file-name "Git Add: ")))
       (shell-command-to-string (format "git add %s" file)))

(defun git-diff () "run git diff command in `find-project-root` result or C-u to choose directory interactively." (interactive)
       (if (null current-prefix-arg)
           (setq --git-diff-dir (find-project-root-or-default-directory))
         (setq --git-diff-dir (read-directory-name "Directory: " default-directory)))

       (let ((default-directory --git-diff-dir))
         (compilation-start "git diff HEAD" 'diff-mode)))

(defun compile-dwim () "run `compile`. If prefixed it wil ask for compile directory." (interactive)
       (setq compilation-buffer-name-function 'amirreza-compile-buffer-name-function)
       (if (null current-prefix-arg)
           (setq --compile-dir (find-project-root-or-default-directory))
         (setq --compile-dir (read-directory-name "Directory: " default-directory)))

       (if (get-buffer (format "*Compile-%s*" --compile-dir))
           (switch-to-buffer (format "*Compile-%s*" --compile-dir)) ;; we have a compile buffer associated with this project.
         ;; we need to create a new compile buffer for this

         (let* ((default-directory --compile-dir)
                (command (read-shell-command "Command: "  (cond ;; guess a command based on the context.
                                                           ((file-exists-p "build.bat") "build.bat")
                                                           ((file-exists-p "go.mod")    "go build -v ./...")
                                                           ((file-exists-p "Makefile")  "make")))))
           (compilation-start command)
           (delete-window)
           (switch-to-buffer (format "*Compile-%s*" --compile-dir)))))

;; Grep Mode
(setq-default case-fold-search t)
(GLOBAL (kbd "M-s") 'grep-dwim)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "M-s")     'previous-buffer)
  (define-key grep-mode-map (kbd "g")       'recompile)
  (define-key grep-mode-map (kbd "G")       (lambda () (interactive) (grep-dwim)))
  (define-key grep-mode-map (kbd "C-r")     'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "<f5>")    'recompile)
  (define-key grep-mode-map (kbd "k")       'kill-compilation))

(defun wgrep-finish-edit-and-save () (interactive)
       (wgrep-finish-edit)
       (wgrep-save-all-buffers))

(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "C-c C-c")  'wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "C-x C-s")  'wgrep-finish-edit-and-save)
  (define-key wgrep-mode-map (kbd "C-c C-k")  'wgrep-abort-changes))

(with-eval-after-load 'grep
  (when (executable-find "rg")
    (grep-apply-setting 'grep-command "rg --no-heading --color='never' ")
    (grep-apply-setting 'grep-use-null-device nil)))

(defun grep-dwim () "Recursive grep in `find-project-root` result or C-u to choose directory interactively." (interactive)
       ;; Set correct compilation buffer name function
       (setq compilation-buffer-name-function 'amirreza-grep-buffer-name-function)

       ;; Set directory for grep.
       (if (null current-prefix-arg)
           (setq --grep-dir (find-project-root-or-default-directory))
         (setq --grep-dir (read-directory-name "Directory: " default-directory)))

       (if (and
            (get-buffer (format "*Grep-%s*" --grep-dir))
            (not (eq (get-buffer (format "*Grep-%s*" --grep-dir)) (current-buffer))))
           (switch-to-buffer (format "*Grep-%s*" --grep-dir)) ;; we have a compile buffer associated with this project.

         (progn
           (setq --last-grep-command-format
                 (cond
                  ((executable-find "rg") "rg --no-heading --color='never' '%s'")
                  ((git-repo-p DIR)       "git grep --no-color -n '%s'")
                  (t                      "grep --color=auto -R -nH -e '%s' .")))
           (setq --last-grep-string (read-string "Grep: "))
           (let ((default-directory --grep-dir))
             (grep (format --last-grep-command-format --last-grep-string))
             (delete-window)
             (switch-to-buffer (format "*Grep-%s*" --grep-dir))))))

;; Find File
(GLOBAL (kbd "M-o") 'find-file-dwim)

(defun find-file-dwim () "Recursive file find starting from `find-project-root` result or C-u to choose directory interactively." (interactive)
       (if (null current-prefix-arg)
           (setq --open-file-dir (find-project-root-or-default-directory))
         (setq --open-file-dir (read-directory-name "Directory: " default-directory)))

       (cond
        ((executable-find "find") (let*
                                      ((default-directory --open-file-dir)
                                       (command (format "find . -type f -not -path \"*/.git/*\""))
                                       (file (completing-read "Find: " (string-split (shell-command-to-string command) "\n" t))))
                                    (find-file file)))

        ((git-repo-p --open-file-dir) (let*
                                          ((default-directory --open-file-dir)
                                           (command (format "git ls-files"))
                                           (file (completing-read "Git Files: " (string-split (shell-command-to-string command) "\n" t))))
                                        (find-file file)))

        ((executable-find "rg") (let* ((default-directory --open-file-dir)
                                       (command (format "rg --files"))
                                       (file (completing-read "Ripgrep Files: " (string-split (shell-command-to-string command) "\n" t) nil t)))
                                  (find-file file)))

        (t (error "you don't have rg installed and it's not a git repo."))))

;; ISearch
(GLOBAL (kbd "C-S-s") 'isearch-forward-thing-at-point)

;; Replace
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "M-r") 'replace-regexp)

(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))

;; Flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-;") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-'") 'flymake-goto-next-error))

;; Macros
(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

;; Rectangle Mode
(global-set-key (kbd "C-x C-SPC") 'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-i")     'string-insert-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-k")     'kill-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-w")     'copy-rectangle-as-kill)
  (define-key rectangle-mark-mode-map (kbd "M-w")     'yank-rectangle)
  (define-key rectangle-mark-mode-map (kbd "C-r")     'string-rectangle))

;; Magit: Git client
(install 'magit)
(global-set-key (kbd "C-x g") 'magit)
