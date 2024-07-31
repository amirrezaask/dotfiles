;; (setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.
(unless (file-exists-p (expand-file-name "early-init.el" user-emacs-directory))
  (write-region "
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
" nil (expand-file-name "early-init.el" user-emacs-directory)))

(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.


(setq frame-title-format "GNU Emacs")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

;; @Font
(setq font-families (font-family-list))
(require 'cl-lib)
(cl-loop for font in '("Liberation Mono-17"
                       "Menlo-16"
                       "Consolas-16")
         do
         (let* ((font-family (car (string-split font "-"))))
           (when (member font-family font-families)
             (set-face-attribute 'default nil :font (format "%s" font))
             (cl-return))))


(defun set-font (font) "Set font" (interactive (list (read-string "Font: ")))
       (set-face-attribute 'default nil :font (format "%s" font)))

(global-unset-key (kbd "C-x C-c"))

(setq native-comp-async-report-warnings-errors nil) ;; silently do jit compiling.

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(defun install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; @Packages
(dolist (pkg '(
               vlf       ;; handle [V]ery [L]arge [F]iles
               wgrep     ;; Editable Grep Buffers
               go-mode
               rust-mode
               php-mode
               json-mode
               yaml-mode)) (install pkg))

;; @Window @Buffer
(defun jump-up () (interactive) (forward-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))
(defun jump-down () (interactive) (forward-line (/ (window-height) 2)) (recenter-top-bottom))

(setq recenter-positions '(middle))

(global-set-key (kbd "C-.")      'next-buffer)
(global-set-key (kbd "C-,")      'previous-buffer)
(global-set-key (kbd "C->")      'end-of-buffer)
(global-set-key (kbd "C-<" )     'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'jump-down)
(global-set-key (kbd "M-<up>")   'jump-up)
(global-set-key (kbd "M-n")      'jump-down)
(global-set-key (kbd "M-p")      'jump-up)
(global-set-key (kbd "C-0")      'delete-window)
(global-set-key (kbd "C-1")      'delete-other-windows)
(global-set-key (kbd "C-3")      'split-window-right)
(global-set-key (kbd "C-2")      'split-window-below)


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

(global-set-key (kbd "M-l") 'indent-buffer) ;; Format buffer

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
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy


(toggle-truncate-lines +1) ;; wrap long lines

(global-so-long-mode +1) ;; don't choke on minified code.

(require 'vlf-setup)

;; @Minibuffer
(global-set-key (kbd "C-q") 'completion-at-point)

(dolist (pkg '(vertico
               consult
               marginalia
               embark
               embark-consult)) (install pkg))

(vertico-mode +1)
(marginalia-mode +1)
(setq vertico-count 10)
(setq vertico-cycle t)

(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "M-i")   'consult-imenu)
(global-set-key (kbd "M-y")   'consult-yank-from-kill-ring)
(global-set-key (kbd "C-;")   'consult-goto-line)
(global-set-key (kbd "M--")   'consult-flymake)
(if (executable-find "rg")
    (global-set-key (kbd "M-j") 'consult-ripgrep)
  (global-set-key (kbd "M-j") 'consult-grep))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-q") 'embark-export))

(setq completion-in-region-function #'consult-completion-in-region)

;; @Helpful: the way help pages should be.
(install 'helpful)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h x") 'helpful-command)
(global-set-key (kbd "C-h .") 'helpful-at-point)
(global-set-key (kbd "C-h F") 'helpful-function)

;; @Themes
(install 'ef-themes)
(defun save-theme (name definition)
  (mkdir (expand-file-name "themes" user-emacs-directory) t)
  (write-region (format "(deftheme %s)
%s
" name definition) nil (expand-file-name (format "%s-theme.el" name) (expand-file-name "themes" user-emacs-directory))))

(save-theme "braid" "
(custom-theme-set-faces
   'braid
  `(default                          ((t (:foreground \"#debe95\" :background \"#202020\"))))
  `(hl-line                          ((t (:background \"#353535\"))))
  `(vertico-current                  ((t (:background \"medium blue\"))))
  `(region                           ((t (:background \"medium blue\"))))
  `(cursor                           ((t (:background \"lightgreen\"))))
  `(font-lock-keyword-face           ((t (:foreground \"#d4d4d4\"))))
  `(font-lock-type-face              ((t (:foreground \"#8cde94\"))))
  `(font-lock-constant-face          ((t (:foreground \"#7ad0c6\"))))
  `(font-lock-variable-name-face     ((t (:foreground \"#c8d4ec\"))))
  `(font-lock-builtin-face           ((t (:foreground \"white\"))))
  `(font-lock-string-face            ((t (:foreground \"gray70\"))))
  `(font-lock-comment-face           ((t (:foreground \"yellow\"))))
  `(font-lock-comment-delimiter-face ((t (:foreground \"yellow\"))))
  `(font-lock-doc-face               ((t (:foreground \"#3fdf1f\"))))
  `(font-lock-function-name-face     ((t (:foreground \"white\"))))
  `(font-lock-doc-string-face        ((t (:foreground \"#3fdf1f\"))))
  `(font-lock-warning-face           ((t (:foreground \"yellow\"))))
  `(font-lock-note-face              ((t (:foreground \"khaki2\" ))))
  `(mode-line                        ((t (:foreground \"black\" :background \"#d3b58d\"))))
  `(mode-line-inactive               ((t (:background \"gray20\" :foreground \"#ffffff\"))))
  `(show-paren-match                 ((t (:background \"mediumseagreen\")))))
(global-hl-line-mode -1)
(blink-cursor-mode +1)
(setq-default cursor-type 'box)
")

(save-theme "witness" "
(custom-theme-set-faces
  'witness
 `(default                          ((t (:foreground \"#d3b58d\" :background \"#072626\"))))
 `(hl-line                          ((t (:background \"#0c4141\"))))
 `(region                           ((t (:background  \"medium blue\"))))
 `(cursor                           ((t (:background \"lightgreen\"))))
 `(font-lock-keyword-face           ((t (:foreground \"white\"))))
 `(font-lock-type-face              ((t (:foreground \"#8cde94\"))))
 `(font-lock-constant-face          ((t (:foreground \"#7ad0c6\"))))
 `(font-lock-variable-name-face     ((t (:foreground \"#c8d4ec\"))))
 `(font-lock-builtin-face           ((t (:foreground \"lightgreen\"))))
 `(font-lock-string-face            ((t (:foreground \"#0fdfaf\"))))
 `(font-lock-comment-face           ((t (:foreground \"#3fdf1f\"))))
 `(font-lock-comment-delimiter-face ((t (:foreground \"#3fdf1f\"))))
 `(font-lock-doc-face               ((t (:foreground \"#3fdf1f\"))))
 `(font-lock-function-name-face     ((t (:foreground \"white\"))))
 `(font-lock-doc-string-face        ((t (:foreground \"#3fdf1f\"))))
 `(hightlight                       ((t (:foreground \"navyblue\" :background \"darkseegreen2\"))))
 `(font-lock-warning-face           ((t (:foreground \"#504038\"))))
 `(font-lock-note-face              ((t (:foreground \"khaki2\" ))))
 `(mode-line                        ((t (:foreground \"black\" :background \"#d3b58d\"))))
 `(mode-line-inactive               ((t (:background \"gray20\" :foreground \"#ffffff\"))))
 `(show-paren-match                 ((t (:background \"mediumseagreen\")))))
(global-hl-line-mode -1)
(setq-default cursor-type 'box)
(blink-cursor-mode +1)
")

(save-theme "handmadehero" "
(custom-theme-set-faces
  'handmadehero
 `(default                          ((t (:foreground \"burlywood2\" :background \"#161616\"))))
 `(hl-line                          ((t (:background \"midnight blue\"))))
 `(vertico-current                  ((t (:background \"midnight blue\"))))
 `(region                           ((t (:background \"medium blue\"))))
 `(cursor                           ((t (:background \"#40FF40\"))))
 `(font-lock-keyword-face           ((t (:foreground \"DarkGoldenrod2\"))))
 `(font-lock-type-face              ((t (:foreground \"burlywood3\"))))
 `(font-lock-constant-face          ((t (:foreground \"olive drab\"))))
 `(font-lock-variable-name-face     ((t (:foreground \"burlywood3\"))))
 `(font-lock-builtin-face           ((t (:foreground \"gray80\"))))
 `(font-lock-string-face            ((t (:foreground \"olive drab\"))))
 `(font-lock-comment-face           ((t (:foreground \"gray50\"))))
 `(font-lock-comment-delimiter-face ((t (:foreground \"gray50\"))))
 `(font-lock-doc-face               ((t (:foreground \"gray50\"))))
 `(font-lock-function-name-face     ((t (:foreground \"burlywood2\"))))
 `(font-lock-doc-string-face        ((t (:foreground \"gray50\"))))
 `(font-lock-warning-face           ((t (:foreground \"yellow\"))))
 `(font-lock-note-face              ((t (:foreground \"khaki2\" ))))
 `(show-paren-match                 ((t (:background \"mediumseagreen\")))))

(global-hl-line-mode +1)
(setq-default cursor-type 'box)
(blink-cursor-mode -1)
")

(save-theme "4coder-fleury" "
(custom-theme-set-faces
 '4coder-fleury
 `(default                          ((t (:foreground \"#a08563\" :background \"#0c0c0c\"))))
 `(cursor                           ((t (:background \"green\"))))
 `(font-lock-keyword-face           ((t (:foreground \"#f0c674\"))))
 `(font-lock-operator-face          ((t (:foreground \"#907553\"))))
 `(font-lock-punctuation-face       ((t (:foreground \"#907553\"))))
 `(font-lock-bracket-face           ((t (:foreground \"#907553\"))))
 `(font-lock-delimiter-face         ((t (:foreground \"#907553\"))))
 `(font-lock-type-face              ((t (:foreground \"#d8a51d\"))))
 `(font-lock-constant-face          ((t (:foreground \"#6b8e23\"))))
 `(font-lock-variable-name-face     ((t (:foreground \"#b99468\"))))
 `(font-lock-builtin-face           ((t (:foreground \"#DAB98F\"))))
 `(font-lock-string-face            ((t (:foreground \"#6b8e23\"))))
 `(font-lock-comment-face           ((t (:foreground \"#686868\"))))
 `(font-lock-comment-delimiter-face ((t (:foreground \"#686868\"))))
 `(font-lock-doc-face               ((t (:foreground \"#686868\"))))
 `(font-lock-function-name-face     ((t (:foreground \"#cc5735\"))))
 `(font-lock-doc-string-face        ((t (:foreground \"#6b8e23\"))))
 `(font-lock-preprocessor-face      ((t (:foreground \"#DAB98F\"))))
 `(font-lock-warning-face           ((t (:foreground \"#504038\"))))
 `(region                           ((t (:background \"#2f2f37\"))))
 `(hl-line                          ((t (:background \"#171616\"))))
 `(vertico-current                  ((t (:inherit hl-line))))
 `(highlight                        ((t (:foreground nil :background \"#2f2f37\"))))
 `(mode-line                        ((t (:foreground \"#cb9401\" :background \"#1f1f27\"))))
 `(mode-line-inactive               ((t (:foreground \"#cb9401\" :background \"#1f1f27\"))))
 `(minibuffer-prompt                ((t (:foreground \"#a08563\") :bold t)))
 `(show-paren-match                 ((t (:background \"#e0741b\" :foreground \"#000000\")))))

(blink-cursor-mode -1)
(global-hl-line-mode +1)
")


(save-theme "solarized-dark" "
(custom-theme-set-faces
   'solarized-dark
   `(default ((t (:foreground \"#839496\" :background \"#002b36\"))))
   `(cursor ((t (:foreground \"#002b36\" :background \"#839496\"))))
   `(font-lock-keyword-face ((t (:foreground \"#859900\"))))
   `(font-lock-type-face ((t (:foreground \"#b58900\"))))
   `(font-lock-constant-face ((t (:foreground \"#268bd2\"))))
   `(font-lock-variable-name-face ((t (:foreground \"#268bd2\"))))
   `(font-lock-builtin-face ((t (:foreground \"#839496\"))))
   `(font-lock-string-face ((t (:foreground \"#2aa198\"))))
   `(font-lock-comment-face ((t (:foreground \"#586e75\"))))
   `(font-lock-comment-delimiter-face ((t (:foreground \"#586e75\"))))
   `(font-lock-doc-face ((t (:foreground \"#2aa198\"))))
   `(font-lock-function-name-face ((t (:foreground \"#268bd2\"))))
   `(font-lock-preprocessor-face ((t (:foreground \"#268bd2\"))))
   `(region ((t (:foreground \"#002b36\" :background \"#93a1a1\"))))
   `(hl-line ((t (:background \"#073642\"))))
   `(vertico-current ((t (:background \"#073642\"))))
   `(highlight ((t (:background \"#073642\"))))
   `(mode-line ((t (:foreground \"#839496\" :background \"#174652\"))))
   `(mode-line-inactive ((t (:foreground \"#586e75\" :background \"#002b36\"))))
   `(minibuffer-prompt ((t (:foreground \"#839496\"))))
   `(show-paren-match ((t (:foreground \"#d33682\")))))
(global-hl-line-mode +1)
(setq-default cursor-type 'box)
(blink-cursor-mode +1)
")

(save-theme "solarized-light" "
(custom-theme-set-faces
   'solarized-light
  `(default ((t (:foreground \"#657b83\" :background \"#fdf6e3\"))))
  `(cursor ((t (:foreground \"#fdf6e3\" :background \"#657b83\"))))
  `(font-lock-keyword-face ((t (:foreground \"#859900\"))))
  `(font-lock-type-face ((t (:foreground \"#b58900\"))))
  `(font-lock-constant-face ((t (:foreground \"#268bd2\"))))
  `(font-lock-variable-name-face ((t (:foreground \"#268bd2\"))))
  `(font-lock-builtin-face ((t (:foreground \"#657b83\"))))
  `(font-lock-string-face ((t (:foreground \"#2aa198\"))))
  `(font-lock-comment-face ((t (:foreground \"#93a1a1\"))))
  `(font-lock-comment-delimiter-face ((t (:foreground \"#93a1a1\"))))
  `(font-lock-doc-face ((t (:foreground \"#2aa198\"))))
  `(font-lock-function-name-face ((t (:foreground \"#268bd2\"))))
  `(font-lock-preprocessor-face ((t (:foreground \"#268bd2\"))))
  `(region ((t (:foreground \"#fdf6e3\" :background \"#586e75\"))))
  `(hl-line ((t (:background \"#eee8d5\"))))
  `(vertico-current ((t (:background \"#eee8d5\"))))
  `(highlight ((t (:background \"#eee8d5\"))))
  `(mode-line ((t (:foreground \"#657b83\" :background \"#eee8d5\"))))
  `(mode-line-inactive ((t (:foreground \"#93a1a1\" :background \"#fdf6e3\"))))
  `(minibuffer-prompt ((t (:foreground \"#657b83\"))))
  `(show-paren-match ((t (:foreground \"#d33682\")))))
(global-hl-line-mode +1)
(setq-default cursor-type 'box)
(blink-cursor-mode +1)
")

(save-theme "default-dark" "
(custom-theme-set-faces
'default-dark
`(default ((t (:foreground \"grey85\" :background \"grey10\")))))
(blink-cursor-mode +1)
")

(save-theme "default-light" "
(dolist (i custom-enabled-themes)
  (disable-theme i))
")

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(install 'ef-themes)

(defadvice load-theme (before disable-themes-first (THEME &rest args) activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(setq custom-safe-themes t)
;; (load-theme 'braid)
(load-theme 'ef-maris-light)

(setq-default c-default-style "linux" c-basic-offset 4)

;; @xref: Jump to definitions
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-?") 'xref-find-references)

;; @LSP (@Eglot)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-l")     'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(dolist (mode '(go rust php)) (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))
(setq eglot-ignored-server-capabilities '(:hoverProvider
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

(setq eglot-stay-out-of '(project))

(setq eglot-sync-connect nil) ;; no blocking on waiting for the server to start.

(setq eglot-events-buffer-size 0) ;; no logging of LSP events.

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

(when (package-installed-p 'consult)
  (install 'consult-eglot)
  (defalias 'eglot-symbols 'consult-eglot-symbols))

(defun find-root () "Try to find project root based on deterministic predicates"
       (cond
        ((eq major-mode 'go-mode)   (locate-dominating-file default-directory "go.mod"))
        ((eq major-mode 'php-mode)  (locate-dominating-file default-directory "composer.json"))
        (t                          (locate-dominating-file default-directory ".git"))))

(defun git-repo-p (DIR) (locate-dominating-file DIR ".git"))
(defun find-root-or-default-directory () (or (find-root) default-directory))

;; @Compile
(global-set-key (kbd "M-m") 'compile-dwim)
(global-set-key (kbd "M-g") 'run-git-diff)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "<f5>") 'recompile)
  (define-key compilation-mode-map (kbd "M-m")  'recompile)
  (define-key compilation-mode-map (kbd "M-n")  'jump-down)
  (define-key compilation-mode-map (kbd "M-p")  'jump-up)
  (define-key compilation-mode-map (kbd "k")    'kill-compilation))

(defun amirreza-compile-buffer-name-function (MODESTR) (let ((dir (find-root-or-default-directory))) (format "*%s-%s*" MODESTR dir)))

(setq-default compilation-buffer-name-function 'amirreza-compile-buffer-name-function)

(defun run-git-diff () "run git diff command in `find-root` result or C-u to choose directory interactively." (interactive)
       (if (null current-prefix-arg)
           (setq --git-diff-dir (find-root-or-default-directory))
         (setq --git-diff-dir (read-directory-name "Directory: " default-directory)))

       (let ((default-directory --git-diff-dir))
         (compilation-start "git diff HEAD" 'diff-mode)))

(defun compile-dwim () "run `compile`. If prefixed it wil ask for compile directory." (interactive)
       (if (null current-prefix-arg)
           (setq --compile-dir (find-root-or-default-directory))
         (setq --compile-dir (read-directory-name "Directory: " default-directory)))

       (let* ((default-directory --compile-dir)
              (command (read-shell-command "Command: "  (cond ;; guess a command based on the context.
                                                         ((file-exists-p "build.bat") "build.bat")
                                                         ((file-exists-p "go.mod")    "go build -v ./...")
                                                         ((file-exists-p "Makefile")  "make")))))
         (compilation-start command)))


;; @Grep
(global-set-key (kbd "M-j") 'grep-dwim)
(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "C-c C-p") 'wgrep-toggle-readonly-area)
  (define-key grep-mode-map (kbd "<f5>")    'recompile)
  (define-key grep-mode-map (kbd "g")       'recompile)
  (define-key grep-mode-map (kbd "M-n")     'jump-down)
  (define-key grep-mode-map (kbd "M-p")     'jump-up)
  (define-key grep-mode-map (kbd "k")       'kill-compilation))

(with-eval-after-load 'grep
  (when (executable-find "rg")
    (grep-apply-setting 'grep-command "rg --no-heading --color='never'")
    (grep-apply-setting 'grep-use-null-device nil)))

(defun grep-dwim () "Recursive grep in `find-root` result or C-u to choose directory interactively." (interactive)
       (if (null current-prefix-arg)
           (setq --grep-dir (find-root-or-default-directory))
         (setq --grep-dir (read-directory-name "Directory: " default-directory)))

       (let ((default-directory --grep-dir))
         (cond
          ((executable-find "rg") (grep (format "rg --no-heading --color='never' '%s'" (read-string "Ripgrep: "))))
          ((git-repo-p DIR)       (grep (format "git grep --no-color -n '%s'" (read-string "Git Grep: "))))
          (t                      (grep (format "grep --color=auto -R -nH -e '%s' ." (read-string "Grep: ")))))))


;; @Find File
(global-set-key (kbd "C-j") 'find-file-dwim)

;; @TODO: Add gnu find backend for this function.
(defun find-file-dwim () "Recursive file find starting from `find-root` result or C-u to choose directory interactively." (interactive)
       (if (null current-prefix-arg)
           (setq --open-file-dir (find-root-or-default-directory))
         (setq --open-file-dir (read-directory-name "Directory: " default-directory)))

       (cond
        ((executable-find "rg") (let* ((default-directory --open-file-dir)
                                       (command (format "rg --files"))
                                       (file (completing-read "Ripgrep Files: " (string-split (shell-command-to-string command) "\n" t) nil t)))
                                  (find-file file)))

        ((git-repo-p --open-file-dir) (let*
                                          ((default-directory --open-file-dir)
                                           (command (format "git ls-files"))
                                           (file (completing-read "Git Files: " (string-split (shell-command-to-string command) "\n" t))))
                                        (find-file file)))
        (t (error "you don't have rg installed and it's not a git repo."))))



;; @Eshell
(global-set-key (kbd "M-;") 'eshell-dwim)
(with-eval-after-load 'esh-mode
  (define-key eshell-mode-map (kbd "M-;") 'previous-buffer))

(setq eshell-visual-subcommands '("git" "diff" "log" "show"))
(defun eshell-dwim () "Jump to eshell buffer associated with current project or create a new." (interactive)
       (let* ((root (find-root-or-default-directory))
              (default-directory root)
              (eshell-buffer-name (format "*eshell-%s*" root)))
         (if (get-buffer eshell-buffer-name)
             (switch-to-buffer eshell-buffer-name)
           (eshell))))

;; @Replace
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "M-r") 'replace-regexp)

(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))

;; @Flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-9") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-0") 'flymake-goto-next-error))

;; @Macros
(global-set-key (kbd "M-[") 'kmacro-start-macro)
(global-set-key (kbd "M-]") 'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

;; @Rectangle Mode
(global-set-key (kbd "C-x C-SPC") 'rectangle-mark-mode)
(with-eval-after-load 'rect
  (define-key rectangle-mark-mode-map (kbd "C-x r i") 'string-insert-rectangle))


;; @Terminal
(install 'eat)
(defun eat-dwim () "" (interactive)
       (if (null current-prefix-arg)
           (setq --eat-dir (find-root-or-default-directory))
         (setq --eat-dir (read-directory-name "Directory: " default-directory)))
       (let ((default-directory --eat-dir)
	     (eat-buffer-name (format "*eat-%s*" --eat-dir)))
	 (eat)
	 ))
(global-set-key (kbd "M-;") 'eat-dwim)
