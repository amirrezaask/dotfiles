(defun write-forms-to-file (FILE forms)
  (with-temp-file FILE
    (mapcar (lambda (form)
              (insert (if (stringp form)
                          (prin1-to-string form :no-escape)
                        (pp-to-string form))
                      "\n"))
            forms)))

(setq early-init-forms
      `(
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
	      redisplay-dont-pause t
	      native-comp-async-report-warnings-errors nil ;; silently do jit compiling.
	      is-windows (eq system-type 'windows-nt)
	      is-linux (eq system-type 'gnu/linux)
	      is-macos (eq system-type 'darwin))

	(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.

	(setq-default frame-title-format "Emacs: Amirreza Edition")

	(defconst user/default-gc-cons-threshold gc-cons-threshold)
	(defconst user/default-gc-cons-percentage gc-cons-percentage)

	(defun user/defer-garbage-collection ()
	  "Defer garbage collection by maximizing the collection threshold."
	  (setq gc-cons-threshold most-positive-fixnum
		gc-cons-percentage 1.0))
	(defun user/restore-garbage-collection ()
	  "Restore the garbage collection threshold parameters to their default values."
	  (setq gc-cons-threshold user/default-gc-cons-threshold
		gc-cons-percentage user/default-gc-cons-percentage))

	";; Defer garbage collection until after initialization"
	(user/defer-garbage-collection)
	(add-hook 'emacs-startup-hook #'user/restore-garbage-collection)

	";; Clear `file-name-handler-alist' until after initialization"
	(setq user/file-name-handler-alist file-name-handler-alist)
	(setq file-name-handler-alist nil)
	(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist user/file-name-handler-alist)))))

(unless (file-exists-p early-init-file)
  (write-forms-to-file early-init-file early-init-forms))


;; (setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.
(unless (executable-find "fd")    (warn "Install fd for better support for file finding."))

;; Enable packages index at startup
(setq package-enable-at-startup t)
(setq package-quickstart t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; MacOS
(setq mac-command-modifier 'meta)

;; env: PATH
(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "d:/bin")
(add-to-list 'exec-path "d:/Go/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(defun edit-init () "Edit this file." (interactive) (find-file INIT-FILE))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(global-set-key (kbd "C-x i") 'edit-init) ;; Edit this file.

;; Overrides: minor mode to register keys that I want to override in all other modes.
(defvar amirreza-keys (make-sparse-keymap))
(define-minor-mode amirreza-mode ""
  :global t
  :lighter " AmirrezaMode"
  :init-value t
  :keymap amirreza-keys)

(amirreza-mode +1)
(defun GLOBAL (KBD ACTION) (define-key amirreza-keys KBD ACTION))

;; Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

;; Highlight Current line
(global-hl-line-mode)

;; Font
(setq font-size 21)
(setq current-font-family "")
(setq font-families (font-family-list))

(defun set-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq current-font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(cond
 ((member "JetBrains Mono" font-families) (set-font "JetBrains Mono" 11))
 (is-windows                              (set-font "Consolas"    11))
 (is-linux                                (set-font "Ubuntu Mono" 11))
 (is-macos                                (set-font "Menlo"       11)))

;; Package archives
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
        ("melpa"    . "https://melpa.org/packages/")))

(defun install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))


;; Splits
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
(GLOBAL (kbd "C-<return>")  'save-buffer)
(GLOBAL (kbd "C-0")         'delete-window-and-balance)
(GLOBAL (kbd "C-1")         'delete-other-windows)
(GLOBAL (kbd "C-2")         'split-window-below-balance-and-switch)
(GLOBAL (kbd "C-3")         'split-window-right-balance-and-switch)
(GLOBAL (kbd "C--")         'text-scale-decrease)
(GLOBAL (kbd "C-=")         'text-scale-increase)
(GLOBAL (kbd "C-o")         'other-window)
(GLOBAL (kbd "M-n")         'jump-down)
(GLOBAL (kbd "M-p")         'jump-up)
(GLOBAL (kbd "M-k")         'kill-current-buffer)

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

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

(toggle-truncate-lines -1) ;; wrap long lines

(global-so-long-mode +1) ;; don't choke on minified code.


;; Helpful ( replacement for help buffers )
(install 'helpful)
(global-set-key (kbd "C-h f")   #'helpful-callable)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h v")   #'helpful-variable)
(global-set-key (kbd "C-h k")   #'helpful-key)
(global-set-key (kbd "C-h x")   #'helpful-command)

;; Minibuffer (vertico + consult)
(install 'vertico)
(install 'marginalia)
(install 'orderless)
(install 'consult)
(install 'nerd-icons-completion)
(with-eval-after-load 'vertico
  (nerd-icons-completion-mode))

(setq vertico-count 20)
(setq vertico-cycle t)
(setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
(vertico-mode +1)
(marginalia-mode +1)

;; Nerd Icons
(install 'nerd-icons)

;; Dired
(install 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)


;; Completion
(install 'corfu)
(setq corfu-auto t)
(global-corfu-mode +1)
(install 'nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(global-set-key (kbd "C-j") 'completion-at-point)

;; Major modes
(install 'go-mode)
(install 'rust-mode)
(install 'php-mode)
(install 'json-mode)
(install 'yaml-mode)
(setq-default c-default-style "linux" c-basic-offset 4)

;; Compilation
(setq compilation-ask-about-save nil) ;; Don't ask about saving unsaved buffers before compile command.
(setq compilation-always-kill t)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

;; Projects
(defun index-projects (dir)
  "Recursively scan for and index any project roots within DIR."
  (interactive
   (list (read-directory-name "Look for projects in: ")))
  (when-let (project-roots (directory-files-recursively dir "\\.git$" #'project--find-in-directory t))
    (message "%d directories indexed as projects."
             (seq-reduce '+ (mapcar #'project-remember-projects-under project-roots) 0))))

(GLOBAL (kbd "C-x p")   project-prefix-map) ;; project.el does the rest.
(GLOBAL (kbd "C-x p e") 'project-eshell)
(GLOBAL (kbd "C-x p c") 'project-compile)
(GLOBAL (kbd "M-o")     'project-find-file)
(GLOBAL (kbd "M-m")     'project-compile)
(GLOBAL (kbd "M-s")     #'deadgrep)

;; Pixel scrolling
(pixel-scroll-precision-mode +1)

;; Better ISearch
(install 'ctrlf)
(ctrlf-mode)

;; Replace
(GLOBAL (kbd "M-r") 'replace-regexp)
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))

;; Macros
(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

;; Multicursors
(install 'multiple-cursors)
(require 'multiple-cursors)
(GLOBAL (kbd "C-M-n") 'mc/mark-next-like-this-symbol)
(GLOBAL (kbd "C-M-p") 'mc/mark-previous-like-this-symbol)
(GLOBAL (kbd "C-S-n") 'mc/mark-next-like-this)
(GLOBAL (kbd "C-S-p") 'mc/mark-previous-like-this)
(GLOBAL (kbd "C-M->") 'mc/mark-all-like-this-dwim)

;; Origami ( Code folding )
(install 'origami)
(with-eval-after-load 'origami
  (define-key origami-mode-map (kbd "C-=") 'origami-toggle-node))

;; Eglot (LSP)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-i")     'consult-eglot-symbols)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(install 'eldoc-box)
(add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode)

(setq eldoc-echo-area-use-multiline-p nil)

(dolist (mode '(go rust php)) ;; Enable LSP automatically.
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))

(setq eglot-ignored-server-capabilities '(
					  ;; Enabled features
					  ;; :completionProvider               ;; "Code completion" 
					  ;; :definitionProvider               ;; "Go to definition" 
					  ;; :typeDefinitionProvider           ;; "Go to type definition" 
					  ;; :implementationProvider           ;; "Go to implementation" 
					  ;; :declarationProvider              ;; "Go to declaration" 
					  ;; :referencesProvider               ;; "Find references" 
					  ;; :renameProvider                   ;; "Rename symbol"

					  ;; Disabled features
					  ;; :signatureHelpProvider               ;; "Function signature help" 
					  ;; :hoverProvider                       ;; "Documentation on hover"
					  :documentHighlightProvider           ;; "Highlight symbols automatically" 
					  :documentSymbolProvider              ;; "List symbols in buffer" 
					  :workspaceSymbolProvider             ;; "List symbols in workspace" 
					  ;; :codeActionProvider                  ;; "Execute code actions" 
					  ;; :codeLensProvider                    ;; "Code lens" 
					  ;; :documentFormattingProvider          ;; "Format buffer" 
					  ;; :documentRangeFormattingProvider     ;; "Format portion of buffer" 
					  :documentOnTypeFormattingProvider    ;; "On-type formatting" 

					  :documentLinkProvider                ;; "Highlight links in document" 
					  :colorProvider                       ;; "Decorate color references" 
					  :foldingRangeProvider                ;; "Fold regions of buffer" 
					  :executeCommandProvider              ;; "Execute custom commands" 
					  :inlayHintProvider                   ;; "Inlay hints" 
					  ))   


(with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

(setq eglot-stay-out-of '(project flymake) ;; Don't polute buffer with flymake diganostics.
      eglot-sync-connect nil               ;; no blocking on waiting for the server to start.
      eglot-events-buffer-size 0)          ;; no logging of LSP events.

;; Themes
(install 'adwaita-dark-theme)
(install 'base16-theme)
(install 'modus-themes)
(install 'ef-themes)

(setq custom-safe-themes t)

(defadvice load-theme (before disable-themes-first activate) ;; Disable theme stacking
  (dolist (i custom-enabled-themes)
    (disable-theme i)))


(setq themes-dir (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-dir)

(unless (file-exists-p themes-dir)
  (mkdir themes-dir))

;; Colors
(unless (file-exists-p (expand-file-name "witness-theme.el" themes-dir))
  (write-forms-to-file (expand-file-name "witness-theme.el" themes-dir)
		       `(
			 (deftheme witness)
			 (custom-theme-set-faces                   ;; Witness
			  'witness
			  `(default                          ((t (:foreground "#d3b58d" :background "#042428"))))
			  `(hl-line                          ((t (:background "#0c4141"))))
			  `(region                           ((t (:background "#0000cd"))))
			  `(cursor                           ((t (:background "#90ee90"))))
			  `(font-lock-keyword-face           ((t (:foreground "#ffffff"))))
			  `(font-lock-type-face              ((t (:foreground "#8cde94"))))
			  `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
			  `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
			  `(font-lock-builtin-face           ((t (:foreground "#90ee90"))))
			  `(font-lock-string-face            ((t (:foreground "#0fdfaf"))))
			  `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
			  `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
			  `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
			  `(font-lock-function-name-face     ((t (:foreground "#ffffff"))))
			  `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
			  `(hightlight                       ((t (:foreground "#000080" :background "#b4eeb4"))))
			  `(font-lock-warning-face           ((t (:foreground "#504038"))))
			  `(font-lock-note-face              ((t (:foreground "#eee685" ))))
			  `(mode-line                        ((t (:foreground "#000000" :background "#d3b58d"))))
			  `(mode-line-inactive               ((t (:background "#333333" :foreground "#ffffff"))))
			  `(show-paren-match                 ((t (:background "#3cb371"))))
			  `(corfu-default                    ((t (:background "#072626"))))
			  `(corfu-border                     ((t (:background "#0c4141")))))
			 (provide 'witness-theme))))


(unless (file-exists-p (expand-file-name "braid-theme.el" themes-dir))
  (write-forms-to-file (expand-file-name "braid-theme.el" themes-dir)
		       `(
			 (deftheme braid)
			 (custom-theme-set-faces ;; Braid
			  'braid
			  `(default                          ((t (:foreground "#debe95" :background "#252525"))))
			  `(hl-line                          ((t (:background "#353535"))))
			  `(vertico-current                  ((t (:background "#0000cd"))))
			  `(region                           ((t (:background "#0000cd"))))
			  `(cursor                           ((t (:background "#90ee90"))))
			  `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
			  `(font-lock-type-face              ((t (:foreground "#8cde94"))))
			  `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
			  `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
			  `(font-lock-builtin-face           ((t (:foreground "#ffffff"))))
			  `(font-lock-string-face            ((t (:foreground "#b3b3b3"))))
			  `(font-lock-comment-face           ((t (:foreground "#ffff00"))))
			  `(font-lock-comment-delimiter-face ((t (:foreground "#ffff00"))))
			  `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
			  `(font-lock-function-name-face     ((t (:foreground "#ffffff"))))
			  `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
			  `(font-lock-warning-face           ((t (:foreground "#ffff00"))))
			  `(font-lock-note-face              ((t (:foreground "#eee685" ))))
			  `(mode-line                        ((t (:foreground "#000000" :background "#d3b58d"))))
			  `(mode-line-inactive               ((t (:background "#333333" :foreground "#ffffff"))))
			  `(show-paren-match                 ((t (:background "#3cb371"))))
			  `(corfu-default                    ((t (:background "#252525"))))
			  `(corfu-border                     ((t (:background "#353535")))))
			 (provide 'braid-theme))))

(unless (file-exists-p (expand-file-name "handmadehero-theme.el" themes-dir))
  (write-forms-to-file (expand-file-name "handmadehero-theme.el" themes-dir)
		       `(
			 (deftheme handmadehero)
			 (custom-theme-set-faces ;; HandmadeHero
			  'handmadehero
			  `(default                          ((t (:foreground "burlywood2" :background "#161616"))))
			  `(hl-line                          ((t (:background "midnight blue"))))
			  `(vertico-current                  ((t (:background "midnight blue"))))
			  `(region                           ((t (:background "medium blue"))))
			  `(cursor                           ((t (:background "#40FF40"))))
			  `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod2"))))
			  `(font-lock-type-face              ((t (:foreground "burlywood3"))))
			  `(font-lock-constant-face          ((t (:foreground "olive drab"))))
			  `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
			  `(font-lock-builtin-face           ((t (:foreground "gray80"))))
			  `(font-lock-string-face            ((t (:foreground "olive drab"))))
			  `(font-lock-comment-face           ((t (:foreground "gray50"))))
			  `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
			  `(font-lock-doc-face               ((t (:foreground "gray50"))))
			  `(font-lock-function-name-face     ((t (:foreground "burlywood2"))))
			  `(font-lock-doc-string-face        ((t (:foreground "gray50"))))
			  `(font-lock-warning-face           ((t (:foreground "yellow"))))
			  `(font-lock-note-face              ((t (:foreground "khaki2" ))))
			  `(show-paren-match                 ((t (:background "mediumseagreen")))))

			 (provide 'handmadehero-theme)
			 )
		       ))

(load-theme 'ef-bio t)
;; (set-background-color "#052525")

;; String conversions
(install 'string-inflection)


;; Treesitter
;; (unless is-windows
;;   (install 'treesit-auto)
;;   (global-treesit-auto-mode))



