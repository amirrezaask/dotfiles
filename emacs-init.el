(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      redisplay-dont-pause t
      native-comp-async-report-warnings-errors nil
      is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu/linux)
      is-macos (eq system-type 'darwin))

(set-frame-parameter nil 'fullscreen 'maximized)

(defconst user/default-gc-cons-threshold gc-cons-threshold)

(defconst user/default-gc-cons-percentage gc-cons-percentage)

(defun user/defer-garbage-collection nil "Defer garbage collection by maximizing the collection threshold."
       (setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 1.0))

(defun user/restore-garbage-collection nil "Restore the garbage collection threshold parameters to their default values."
       (setq gc-cons-threshold user/default-gc-cons-threshold gc-cons-percentage user/default-gc-cons-percentage))

;; Defer garbage collection until after initialization
(user/defer-garbage-collection)

(add-hook 'emacs-startup-hook #'user/restore-garbage-collection)

;; Clear `file-name-handler-alist' until after initialization
(setq user/file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda nil
	    (setq file-name-handler-alist user/file-name-handler-alist)))


(defun dirt-colors () ;; Colors from really old jonathan blow streams, a brownish feel.
  (interactive)
  (global-hl-line-mode -1)
  (custom-set-faces
   `(default				((t	(:foreground "#debe95" :background "#252525"))))
   `(hl-line				((t	(:background "#353535"))))
   `(vertico-current			((t	(:background "#0000cd"))))
   `(region				((t	(:background "#0000cd"))))
   `(cursor				((t	(:background "#90ee90"))))
   `(font-lock-keyword-face		((t 	(:foreground "#d4d4d4"))))
   `(font-lock-type-face		((t 	(:foreground "#8cde94"))))
   `(font-lock-constant-face		((t 	(:foreground "#7ad0c6"))))
   `(font-lock-variable-name-face	((t 	(:foreground "#c8d4ec"))))
   `(font-lock-builtin-face		((t 	(:foreground "#ffffff"))))
   `(font-lock-string-face		((t 	(:foreground "#b3b3b3"))))
   `(font-lock-comment-face		((t 	(:foreground "#ffff00"))))
   `(font-lock-comment-delimiter-face	((t 	(:foreground "#ffff00"))))
   `(font-lock-doc-face			((t 	(:foreground "#3fdf1f"))))
   `(font-lock-function-name-face	((t 	(:foreground "#ffffff"))))
   `(font-lock-doc-string-face		((t 	(:foreground "#3fdf1f"))))
   `(font-lock-warning-face		((t 	(:foreground "#ffff00"))))
   `(font-lock-note-face		((t 	(:foreground "#eee685"))))
   `(mode-line				((t	(:foreground "#000000" :background "#d3b58d"))))
   `(mode-line-inactive			((t 	(:background "#333333" :foreground "#ffffff"))))
   `(show-paren-match			((t 	(:background "#3cb371"))))))

(defun grass-colors () ;; Emacs colors from jonathan blow streams.
  (interactive)
  (global-hl-line-mode -1)
  (custom-set-faces
   `(default				((t	(:foreground "#d3b58d" :background "#042428"))))
   `(hl-line				((t	(:background "#0c4141"))))
   `(vertico-current                    ((t     (:background "#0c4141"))))
   `(region				((t	(:background "#0000cd"))))
   `(cursor				((t	(:background "#90ee90"))))
   `(font-lock-keyword-face		((t	(:foreground "#ffffff"))))
   `(font-lock-type-face		((t	(:foreground "#8cde94"))))
   `(font-lock-constant-face		((t	(:foreground "#7ad0c6"))))
   `(font-lock-variable-name-face	((t	(:foreground "#c8d4ec"))))
   `(font-lock-builtin-face		((t	(:foreground "#90ee90"))))
   `(font-lock-string-face		((t	(:foreground "#0fdfaf"))))
   `(font-lock-comment-face		((t	(:foreground "#3fdf1f"))))
   `(font-lock-comment-delimiter-face	((t     (:foreground "#3fdf1f"))))
   `(font-lock-doc-face			((t	(:foreground "#3fdf1f"))))
   `(font-lock-function-name-face	((t	(:foreground "#ffffff"))))
   `(font-lock-doc-string-face		((t	(:foreground "#3fdf1f"))))
   `(hightlight				((t	(:foreground "#000080" :background "#b4eeb4"))))
   `(font-lock-warning-face		((t	(:foreground "#504038"))))
   `(font-lock-note-face		((t	(:foreground "#eee685"))))
   `(mode-line				((t	(:foreground "#000000" :background "#d3b58d"))))
   `(mode-line-inactive			((t	(:background "#333333" :foreground "#ffffff"))))
   `(show-paren-match			((t	(:background "#3cb371"))))))

(defun casey-colors () ;; Emacs colors from handmadehero series by casey muratori.
  (interactive)
  (global-hl-line-mode +1)
  (custom-set-faces
   `(default				((t       (:foreground "burlywood2" :background "#161616"))))
   `(hl-line				((t       (:background "midnight blue"))))
   `(vertico-current			((t       (:background "midnight blue"))))
   `(region				((t       (:background "medium blue"))))
   `(cursor				((t       (:background "#40FF40"))))
   `(font-lock-keyword-face		((t       (:foreground "DarkGoldenrod2"))))
   `(font-lock-type-face		((t       (:foreground "burlywood3"))))
   `(font-lock-constant-face		((t       (:foreground "olive drab"))))
   `(font-lock-variable-name-face	((t       (:foreground "burlywood3"))))
   `(font-lock-builtin-face		((t       (:foreground "gray80"))))
   `(font-lock-string-face		((t       (:foreground "olive drab"))))
   `(font-lock-comment-face		((t       (:foreground "gray50"))))
   `(font-lock-comment-delimiter-face	((t       (:foreground "gray50"))))
   `(font-lock-doc-face			((t       (:foreground "gray50"))))
   `(font-lock-function-name-face	((t       (:foreground "burlywood2"))))
   `(font-lock-doc-string-face		((t       (:foreground "gray50"))))
   `(font-lock-warning-face		((t       (:foreground "yellow"))))
   `(font-lock-note-face		((t       (:foreground "khaki2"))))
   `(show-paren-match			((t       (:background "mediumseagreen"))))))

(grass-colors)

(defun edit-init () "Edit this file." (interactive) (find-file INIT-FILE))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(defun RELOAD ()
  (interactive)
  (load-file INIT-FILE))

(global-set-key (kbd "C-x i") 'edit-init) ;; Edit this file.

;; (setq debug-on-error t) ;; Uncomment when you debug your emacs lisp code.

;; Enable packages index at startup
(setq package-enable-at-startup t)
(setq package-quickstart t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq mac-command-modifier 'meta)

(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")

(if (eq system-type 'windows-nt)
    (setenv "PATH" (string-join exec-path ";"))
  (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH



;; Overrides: minor mode to register keys that I want to override in all other modes.
(defvar amirreza-keys (make-sparse-keymap))
(define-minor-mode amirreza-mode ""
  :global t
  :lighter " AmirrezaMode"
  :init-value t
  :keymap amirreza-keys)

(amirreza-mode +1)
(defun GLOBAL (KBD ACTION) (define-key amirreza-keys KBD ACTION))

;; Font
(setq font-size 11)
(setq current-font-family "")
(setq font-families (font-family-list))

(defun load-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq current-font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(defun set-font-size (size) (interactive (list (read-number "Size: ")))
       (setq font-size size)
       (load-font current-font-family font-size))

(cond
 (is-windows                              (load-font "Consolas"    11))
 (is-linux                                (load-font "Ubuntu Mono" 11))
 (is-macos                                (load-font "Menlo"       11)))

;; Package archives
(setq package-archives
      '(("gnu-elpa"  . "https://elpa.gnu.org/packages/")
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
(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(setq recenter-positions '(middle))

(GLOBAL		(kbd "C-<return>")	'save-buffer)
(GLOBAL		(kbd "C-0")		'delete-window-and-balance)
(GLOBAL		(kbd "C-1")		'delete-other-windows)
(GLOBAL		(kbd "C-2")		'split-window-below-balance-and-switch)
(GLOBAL		(kbd "C-3")		'split-window-right-balance-and-switch)
(GLOBAL		(kbd "C--")		'text-scale-decrease)
(GLOBAL		(kbd "C-=")		'text-scale-increase)
(GLOBAL		(kbd "M-n")		'jump-down)
(GLOBAL		(kbd "M-p")		'jump-up)
(GLOBAL		(kbd "M-k")		'kill-current-buffer)
(global-set-key (kbd "C-q")             'completion-at-point)
(global-set-key (kbd "M-q")             'quoted-insert)
(global-set-key (kbd "C-o")		'other-window)


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
(global-set-key (kbd "C-z")   'undo) ;; undo
(global-set-key (kbd "M-z")   'undo)
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy
(global-unset-key (kbd "M-z")) ;; UNUSED
(global-unset-key (kbd "M-l")) ;; UNUSED
(toggle-truncate-lines -1) ;; wrap long lines
(global-so-long-mode +1) ;; don't choke on minified code.

;; I use vertico package for basic minibuffer completion which is much better that Emacs default,
;; Consult package adds functionality on top of vertico which I use for in buffer completion (aka Autocomplete),
;; Embark package is useful for acting on minibuffer completion items, I only use it to export results into a buffer, mostly when grepping in project.
(install 'vertico)
(setq vertico-count 5)
(install 'consult)
(install 'embark)
(install 'embark-consult)


;; I constantly switch between having vertico and not having that to see which workflow matches my preference.
(vertico-mode +1)
(setq completion-in-region-function #'consult-completion-in-region)
(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-;") 'embark-export))




(unless vertico-mode ;; Telling emacs to configure default minibuffer in a way that is usable without any package.
  (setq completions-format 'one-column)
  (setq completions-header-format nil)
  (setq completions-max-height 30)
  (setq completion-auto-select nil)
  (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (defun my/minibuffer-choose-completion (&optional no-exit no-quit)
    (interactive "P")
    (with-minibuffer-completions-window
      (let ((completion-use-base-affixes nil))
	(choose-completion nil no-exit no-quit)))))


(unless is-windows (install 'magit)) ;; One of the best git clients ever.

(install 'go-mode)
(install 'rust-mode)
(install 'php-mode)
(install 'json-mode)
(install 'yaml-mode)
(setq-default c-default-style "linux" c-basic-offset 4)

(setq compilation-ask-about-save nil) ;; Don't ask about saving unsaved buffers before compile command.
(setq compilation-always-kill t)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(defun find-project-root (DIR) (locate-dominating-file DIR ".git"))
(defun find-project-root-or-default-directory () (or (find-project-root default-directory) default-directory))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k")    'kill-compilation)
  (define-key grep-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(defun get-grep-default-command ()
  (cond
   ((executable-find "ugrep")            "ugrep -nr \"%s\"")
   ((executable-find "rg")               "rg --no-heading --color=\"never\" %s")
   ((git-repo-p default-directory)       "git grep --no-color -n \"%s\"")
   (t                                    "grep -rn \"%s\"")))

(with-eval-after-load 'grep
  (grep-apply-setting 'grep-command (get-grep-default-command))
  (grep-apply-setting 'grep-use-null-device nil))

(defun recompile-project (&optional EDIT-COMMAND)
  (interactive "P")
  (let ((default-directory (find-project-root-or-default-directory)))
    (recompile EDIT-COMMAND)))

(defun grep-project (&optional EDIT)
  (interactive "P")
  (let ((default-directory (find-project-root-or-default-directory)))
    (grep (format (get-grep-default-command) (read-string "Grep: ")))))

(GLOBAL (kbd "M-m") 'recompile-project)
(GLOBAL (kbd "M-s") 'grep-project)

(GLOBAL (kbd "M-}") 'next-error)
(GLOBAL (kbd "M-{") 'previous-error)
(GLOBAL (kbd "M-o") 'project-find-file)

;; Pixel scrolling
(pixel-scroll-precision-mode +1)

;; Replace
(GLOBAL (kbd "M-r") 'replace-regexp)
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))

;; Macros
(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)



;; String conversions
(install 'string-inflection)

;; XREF
(with-eval-after-load 'xref
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-,") 'xref-go-back)
  (global-set-key (kbd "M-?")  'xref-find-references)
  (global-set-key (kbd "M-/")  'xref-find-references))

;; Eglot (LSP)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-i")     'consult-eglot-symbols)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(setq eldoc-echo-area-use-multiline-p nil)

(dolist (mode '(go go-ts rust php php-ts)) ;; Enable LSP automatically.
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))


(setq eglot-ignored-server-capabilities '( ;; Disable fancy LSP features.
                                          :documentHighlightProvider           ;; "Highlight symbols automatically"
                                          :documentSymbolProvider              ;; "List symbols in buffer"
                                          :workspaceSymbolProvider             ;; "List symbols in workspace"
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

