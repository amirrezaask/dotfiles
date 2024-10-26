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


(unless (executable-find "fd")    (warn "Install fd for better support for file finding."))
(unless (executable-find "ugrep") (warn "Install ugrep for better grep support."))

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
      redisplay-dont-pause t
      native-comp-async-report-warnings-errors nil ;; silently do jit compiling.
      is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu/linux)
      is-macos (eq system-type 'darwin))

(set-frame-parameter nil 'fullscreen 'maximized) ;; Always start emacs window in maximized mode.

;; @PATH
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

(setq image-types (cons 'svg image-types) mac-command-modifier 'meta) ;; Fix macos fucked up bugs.

(defun edit-init () "Edit this file." (interactive) (find-file INIT-FILE))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(global-set-key (kbd "C-x i") 'edit-init) ;; Edit this file.

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
(setq font-size 21)
(setq current-font-family "")
(setq font-families (font-family-list))

(defun set-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq current-font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(cond
 (is-windows     (set-font "Consolas"    14))
 (is-linux       (set-font "Ubuntu Mono" 14))
 (is-macos       (set-font "Menlo"       14)))


(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
        ("melpa"    . "https://melpa.org/packages/")))

(defun install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

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

(toggle-truncate-lines -1) ;; wrap long lines

(global-so-long-mode +1) ;; don't choke on minified code.

;; Minibuffer/Completion:
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 15)
(setq completions-auto-select nil)
(setq completion-show-help nil)
(setq completion-styles '(basic flex partial-completion emacs22))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map           (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map           (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "RET") 'minibuffer-choose-completion))

;; Completion
(global-set-key (kbd "C-j") 'completion-at-point)

(setq-default c-default-style "linux" c-basic-offset 4)

;; Languages
(install 'go-mode)
(install 'rust-mode)
(install 'php-mode)
(install 'json-mode)
(install 'yaml-mode)

;; LSP (Eglot)

;; (with-eval-after-load 'eglot
;;   (define-key eglot-mode-map (kbd "M-i")     'consult-eglot-symbols)
;;   (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
;;   (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
;;   (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

;; (setq eldoc-echo-area-use-multiline-p nil)

;; (dolist (mode '(go rust php)) ;; Enable LSP automatically.
;;   (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))


;; (setq eglot-ignored-server-capabilities '(
;; 					  ;; Enabled features
					  
;; 					  ;; :completionProvider               ;; "Code completion" 
;; 					  ;; :definitionProvider               ;; "Go to definition" 
;; 					  ;; :typeDefinitionProvider           ;; "Go to type definition" 
;; 					  ;; :implementationProvider           ;; "Go to implementation" 
;; 					  ;; :declarationProvider              ;; "Go to declaration" 
;; 					  ;; :referencesProvider               ;; "Find references" 
;; 					  ;; :renameProvider                   ;; "Rename symbol"

;; 					  ;; Disabled features
;; 					  :signatureHelpProvider               ;; "Function signature help" 
;; 					  :hoverProvider                       ;; "Documentation on hover"
;; 					  :documentHighlightProvider           ;; "Highlight symbols automatically" 
;; 					  :documentSymbolProvider              ;; "List symbols in buffer" 
;; 					  :workspaceSymbolProvider             ;; "List symbols in workspace" 
;; 					  :codeActionProvider                  ;; "Execute code actions" 
;; 					  :codeLensProvider                    ;; "Code lens" 
;; 					  :documentFormattingProvider          ;; "Format buffer" 
;; 					  :documentRangeFormattingProvider     ;; "Format portion of buffer" 
;; 					  :documentOnTypeFormattingProvider    ;; "On-type formatting" 

;; 					  :documentLinkProvider                ;; "Highlight links in document" 
;; 					  :colorProvider                       ;; "Decorate color references" 
;; 					  :foldingRangeProvider                ;; "Fold regions of buffer" 
;; 					  :executeCommandProvider              ;; "Execute custom commands" 
;; 					  :inlayHintProvider                   ;; "Inlay hints" 
;; 					  ))   


;; (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

;; (defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
;; (defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

;; (setq eglot-stay-out-of '(project flymake) ;; Don't polute buffer with flymake diganostics.
;;       eglot-sync-connect nil               ;; no blocking on waiting for the server to start.
;;       eglot-events-buffer-size 0)          ;; no logging of LSP events.

;; Compile/Grep
(setq compilation-ask-about-save nil) ;; Don't ask about saving unsaved buffers before compile command.
(setq compilation-always-kill t)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k")    'kill-compilation)
  (define-key grep-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(defun find-project-root () "Try to find project root based on deterministic predicates"
       (cond
        ((git-repo-p default-directory) (locate-dominating-file default-directory ".git"))
        ((eq major-mode 'go-mode)   (locate-dominating-file default-directory "go.mod"))
        ((eq major-mode 'php-mode)  (locate-dominating-file default-directory "composer.json"))
        (t                          default-directory)))

(defun git-repo-p (DIR) (locate-dominating-file DIR ".git"))
(defun find-project-root-or-default-directory () (or (find-project-root) default-directory))

(defun get-grep-default-command ()
  (cond
   ((executable-find "ugrep")                       "ugrep --include=\"*.*\" -rne ")
   ((executable-find "rg")                          "rg --no-heading --color=\"never\" -g *.* ")
   ((and (executable-find "grep") is-linux)         "grep --include=\"*.*\" -ren ")
   ((and (executable-find "findstr") is-windows)    "findstr /SN /C: *.*") ;; Windows only
   (t                                               (error "No valid grep programs found, install ugrep or ripgrep or gnu-grep to use this function."))))

(with-eval-after-load 'grep
  (grep-apply-setting 'grep-command (get-grep-default-command))
  (grep-apply-setting 'grep-use-null-device nil))

(defun run-in-project (fn &rest args) "Run given function at project root, if you want to choose directory use C-u."
       (let ((default-directory
              (if (null current-prefix-arg)
                  (find-project-root-or-default-directory)
                (read-directory-name "Directory: " default-directory))))
         (apply fn args)))

(defun run-in-dir (fn &rest args)
  (let ((default-directory (read-directory-name "Directory: " (find-project-root-or-default-directory))))
    (apply fn args)))

(GLOBAL (kbd "M-m") (lambda () (interactive)  (run-in-project 'compile (read-shell-command "Compile Command: "))))
(GLOBAL (kbd "M-s") (lambda () (interactive)  (run-in-project 'grep (concat (get-grep-default-command)
									    (format "\"%s\""
										    (read-string (format "%s: " (get-grep-default-command))))))))

;; Find File
(GLOBAL (kbd "M-o") 'find-file-dwim)

(defun find-file-dwim () "Recursive file find starting from `find-project-root` result or C-u to choose directory interactively." (interactive)
       (let (
	     (default-directory (if (null current-prefix-arg)
                  (find-project-root-or-default-directory)
                (read-directory-name "Directory: " default-directory)))

             (command
              (cond
	       ((executable-find "fd")   "fd -c never")
	       ((executable-find "rg")   "rg --files")
	       ((and is-windows          "dir /S /B"))
	       ((executable-find "find") "find . -type f -not -path \"*/.git/*\""))))
               

         (find-file (completing-read "File: " (string-split (shell-command-to-string command) "\n" t)))))

;; ISearch
(GLOBAL (kbd "C-S-s") 'isearch-forward-thing-at-point)
(setq-default case-fold-search t)

;; Replace
(GLOBAL (kbd "C-r") 'replace-string)
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

;; Colors
(custom-set-faces                   ;; Witness
 `(default                          ((t (:foreground "#d3b58d" :background "#072626"))))
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

;; (custom-set-faces ;; Braid
;;  `(default                          ((t (:foreground "#debe95" :background "#252525"))))
;;  `(hl-line                          ((t (:background "#353535"))))
;;  `(vertico-current                  ((t (:background "#0000cd"))))
;;  `(region                           ((t (:background "#0000cd"))))
;;  `(cursor                           ((t (:background "#90ee90"))))
;;  `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
;;  `(font-lock-type-face              ((t (:foreground "#8cde94"))))
;;  `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
;;  `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
;;  `(font-lock-builtin-face           ((t (:foreground "#ffffff"))))
;;  `(font-lock-string-face            ((t (:foreground "#b3b3b3"))))
;;  `(font-lock-comment-face           ((t (:foreground "#ffff00"))))
;;  `(font-lock-comment-delimiter-face ((t (:foreground "#ffff00"))))
;;  `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
;;  `(font-lock-function-name-face     ((t (:foreground "#ffffff"))))
;;  `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
;;  `(font-lock-warning-face           ((t (:foreground "#ffff00"))))
;;  `(font-lock-note-face              ((t (:foreground "#eee685" ))))
;;  `(mode-line                        ((t (:foreground "#000000" :background "#d3b58d"))))
;;  `(mode-line-inactive               ((t (:background "#333333" :foreground "#ffffff"))))
;;  `(show-paren-match                 ((t (:background "#3cb371"))))
;;  `(corfu-default                    ((t (:background "#252525"))))
;;  `(corfu-border                     ((t (:background "#353535")))))


