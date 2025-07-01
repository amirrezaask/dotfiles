;; Options and variables.
(setq-default frame-resize-pixelwise t
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
              is-macos (eq system-type 'darwin)
              user/file-name-handler-alist file-name-handler-alist
              file-name-handler-alist nil
              package-enable-at-startup t
              package-quickstart t
              font-size 11
              current-font-family ""
              font-families (font-family-list)
              make-backup-files nil              ;; no emacs ~ backup files
              vc-follow-symlinks t               ;; Don't prompt if encounter a symlink file, just follow the link.
              package-archives '(("gnu-elpa"  . "https://elpa.gnu.org/packages/") ("melpa"    . "https://melpa.org/packages/"))
              recenter-positions '(middle)
              kill-whole-line t
              compilation-ask-about-save nil ;; Don't ask about saving unsaved buffers before compile command.
              compilation-always-kill t
              vertico-count 5
              eldoc-echo-area-use-multiline-p nil
              eglot-ignored-server-capabilities '( ;; Disable fancy LSP features.
                                                  :documentHighlightProvider           ;; "Highlight symbols automatically"
                                                  :documentOnTypeFormattingProvider    ;; "On-type formatting"
                                                  :documentLinkProvider                ;; "Highlight links in document"
                                                  :colorProvider                       ;; "Decorate color references"
                                                  :foldingRangeProvider                ;; "Fold regions of buffer"
                                                  :executeCommandProvider              ;; "Execute custom commands"
                                                  :inlayHintProvider                   ;; "Inlay hints"
                                                  )
              eglot-stay-out-of '(project flymake) ;; Don't polute buffer with flymake diganostics.
              eglot-sync-connect nil               ;; no blocking on waiting for the server to start.
              eglot-events-buffer-size 0           ;; no logging of LSP events.
              c-default-style "linux"
              c-basic-offset 4
              completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))
              cursor-type 'bar
              vertico-count 18
	      corfu-auto t
	      doom-modeline-height 35
              )

(set-frame-parameter nil 'fullscreen 'maximized)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 50 * 1000 * 1000 ;; 50MB
                                      gc-cons-percentage 0.8
                                      file-name-handler-alist user/file-name-handler-alist)))

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))


;; Ensure packages are installed
(dolist (pkg '(vertico
               orderless
               consult
               embark
               embark-consult
               consult-eglot
               go-mode
               rust-mode
               php-mode
               json-mode
               yaml-mode
               string-inflection
               eglot
               corfu
	       doom-themes
	       doom-modeline
               ))
  (unless (package-installed-p pkg)
    (package-install pkg)))


(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode +1)

;; Theme And UI
(load-theme 'doom-one t)
(doom-modeline-mode +1)

(setq mac-command-modifier 'meta)

(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(if (eq system-type 'windows-nt) (setenv "PATH" (string-join exec-path ";")) (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(pixel-scroll-precision-mode +1)        ;; better scrolling experience.
(toggle-truncate-lines -1)              ;; Wrap long lines
(global-so-long-mode +1)                ;; Don't choke on minified code.
(set-default-coding-systems 'utf-8)     ;; Always use UTF8
(global-auto-revert-mode +1)            ;; Auto revert to disk changes, do we really want this ??
(global-hl-line-mode +1)                ;; Highlight current line.
(delete-selection-mode +1)              ;; Delete selected region before inserting.

(defun jump-up ()
  (interactive)
  (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))

(defun jump-down ()
  (interactive)
  (next-line (/ (window-height) 2)) (recenter-top-bottom))

(defun copy () "Either copy region or the current line."
       (interactive)
       (if (use-region-p)
           (kill-ring-save (region-beginning) (region-end)) ;; copy active region contents
         (kill-ring-save (line-beginning-position) (line-end-position)))) ;; copy current line

(defun cut () "Either cut region or the current line."
       (interactive)
       (if (use-region-p)
           (kill-region (region-beginning) (region-end)) ;; copy active region contents
         (kill-region (line-beginning-position) (line-end-position)))) ;; copy current line

(defun EDIT () "Edit this file." (interactive) (find-file INIT-FILE))

(defun RELOAD ()  (interactive) (load-file INIT-FILE))

;; Overrides: minor mode to register keys that I want to override in all other modes.
(defvar global-override-keys (make-sparse-keymap))
(define-minor-mode global-override-mode ""
  :global t
  :lighter " GlobalOverride"
  :init-value t
  :keymap global-override-keys)

(global-override-mode +1)
(defun GLOBAL (KBD ACTION) (define-key global-override-keys KBD ACTION))

;; Font
(defun load-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq current-font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(defun set-font-size (size) (interactive (list (read-number "Size: ")))
       (setq font-size size)
       (load-font current-font-family font-size))

(load-font "Fira Code" 15)

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

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme."
       (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))


(global-corfu-mode +1)


(vertico-mode +1)

;; LSP
(dolist (mode '(go rust php)) ;; Enable LSP automatically.
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))

(with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

;; Keybindings
(global-set-key (kbd "C-x i") 'EDIT) ;; Edit this file.

(with-eval-after-load 'xref
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-,") 'xref-go-back)
  (global-set-key (kbd "M-?")  'xref-find-references)
  (global-set-key (kbd "M-/")  'xref-find-references))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

(GLOBAL (kbd "M-m") 'compile-project)
(GLOBAL (kbd "M-s") 'consult-ripgrep)
(GLOBAL (kbd "M-}") 'next-error)
(GLOBAL (kbd "M-{") 'previous-error)
(GLOBAL (kbd "M-o") 'project-find-file)
(GLOBAL (kbd "M-r") 'replace-regexp)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k")    'kill-compilation)
  (define-key grep-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(global-set-key (kbd "C-;")   'goto-line)
(global-set-key (kbd "C-w")   'cut) ;; modern cut
(global-set-key (kbd "C-z")   'undo) ;; undo
(global-set-key (kbd "M-z")   'undo) ;; undo
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy
(global-unset-key (kbd "M-z")) ;; UNUSED
(global-unset-key (kbd "M-l")) ;; UNUSED

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-;") 'embark-export))

(unless vertico-mode
  (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion))

(GLOBAL         (kbd "C-<return>")      'save-buffer)
(GLOBAL         (kbd "C-0")             'delete-window-and-balance)
(GLOBAL         (kbd "C-1")             'delete-other-windows)
(GLOBAL         (kbd "C-2")             'split-window-below-balance-and-switch)
(GLOBAL         (kbd "C-3")             'split-window-right-balance-and-switch)
(GLOBAL         (kbd "C--")             'text-scale-decrease)
(GLOBAL         (kbd "C-=")             'text-scale-increase)
(GLOBAL         (kbd "M-n")             'jump-down)
(GLOBAL         (kbd "M-p")             'jump-up)
(GLOBAL         (kbd "M-k")             'kill-current-buffer)
(global-set-key (kbd "C-j")             'completion-at-point)
(global-set-key (kbd "M-q")             'quoted-insert)
(global-set-key (kbd "C-o")             'other-window)

(global-set-key (kbd "M-RET")           'indent-buffer) ;; Format buffer
(global-set-key (kbd "C-/")             'comment-line) ;; Comment

(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))
