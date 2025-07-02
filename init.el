(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(use-package amirrezathemes
  :defer t
  :init (add-to-list 'custom-theme-load-path (expand-file-name "amirrezathemes" (expand-file-name "elpa" user-emacs-directory)))
  :vc (:url "https://github.com/amirrezaask/amirrezathemes"))

(setq mac-command-modifier 'meta)

;; Path
(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(if (eq system-type 'windows-nt) (setenv "PATH" (string-join exec-path ";")) (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH


;; Theme And UI
(use-package ns-auto-titlebar :ensure t
  :if (eq system-type 'darwin)
  :config (ns-auto-titlebar-mode +1))

(use-package ef-themes :ensure t)
(use-package doom-themes :ensure t)
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(load-theme 'ef-bio t)


;; Text editing and navigation
(pixel-scroll-precision-mode +1)        ;; better scrolling experience.
(toggle-truncate-lines -1)              ;; Wrap long lines
(global-so-long-mode +1)                ;; Don't choke on minified code.
(set-default-coding-systems 'utf-8)     ;; Always use UTF8
(global-auto-revert-mode +1)            ;; Auto revert to disk changes, do we really want this ??
(global-hl-line-mode +1)                ;; Highlight current line.
(delete-selection-mode +1)              ;; Delete selected region before inserting.

(use-package string-inflection :ensure t)
;; (use-package multiple-cursors :ensure t)

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
(GLOBAL         (kbd "C-0")             'delete-window-and-balance)
(GLOBAL         (kbd "C-1")             'delete-other-windows)
(GLOBAL         (kbd "C-2")             'split-window-below-balance-and-switch)
(GLOBAL         (kbd "C-3")             'split-window-right-balance-and-switch)
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

;; Completion
(use-package corfu :ensure t
  :init
  (setq corfu-auto t)
  (setq corfu-preselect 'prompt)
  :config
  (global-corfu-mode +1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package vertico
  :ensure t
  :init
  (setq vertico-count 15)
  :config
  (vertico-mode +1))

(use-package consult        :ensure t)
(use-package embark         :ensure t)
(use-package embark-consult :ensure t)
(use-package consult-eglot  :ensure t)
(use-package minibuffer :after embark :bind (:map minibuffer-mode-map ("C-;" . embark-export)))


;; LSP
(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
	
	(("C-c C-r" . 'eglot-rename)
	("M-RET"    . 'eglot-organize-imports-format)
	("C-c C-c"  . 'eglot-code-actions)))
  :init
  (setq 
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
   )
  (dolist (mode '(go rust php)) ;; Enable LSP automatically.
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))
  (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense
  (defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
  (defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports)))

;; Language modes
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package go-mode   :ensure t)
(use-package rust-mode :ensure t)
(use-package php-mode  :ensure t)

;; Compile and grep
(GLOBAL (kbd "M-m") 'compile-project)
(GLOBAL (kbd "M-s") 'consult-ripgrep)
(GLOBAL (kbd "M-o") 'project-find-file)
(GLOBAL (kbd "M-r") 'replace-regexp)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k")    'kill-compilation)
  (define-key compilation-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k")    'kill-compilation)
  (define-key grep-mode-map (kbd "G")    (lambda () (interactive) (recompile t))))

(defun find-project-root-or-default-directory () (or (locate-dominating-file default-directory ".git") default-directory))
(defun compile-project (&optional EDIT-COMMAND)
  (interactive "P")
  (let ((default-directory (find-project-root-or-default-directory)))
    (recompile EDIT-COMMAND)))

(defun grep-project (&optional EDIT)
  (interactive "P")
  (let ((default-directory (find-project-root-or-default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))

(use-package xref
  :bind
  ((("M-."  .  'xref-find-definitions))
     (("M-,"  . 'xref-go-back))
     (("M-?"  . 'xref-find-references))
     (("M-/"  . 'xref-find-references))))

;; Keybindings
(global-set-key (kbd "C-x i") 'EDIT) ;; Edit this file.

(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

(global-set-key (kbd "C-;")   'goto-line)
(global-set-key (kbd "C-w")   'cut) ;; modern cut
(global-set-key (kbd "C-z")   'undo) ;; undo
(global-set-key (kbd "M-z")   'undo) ;; undo
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy
(global-unset-key (kbd "M-z")) ;; UNUSED
(global-unset-key (kbd "M-l")) ;; UNUSED


(GLOBAL         (kbd "C-<return>")      'save-buffer)
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
