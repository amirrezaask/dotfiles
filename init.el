;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Never use dialog boxes for questions.
(setq use-dialog-box nil)

;; Never use dialog boxes for asking about files.
(setq use-file-dialog nil)

;; Prefer shorter answers.
(setq use-short-answers t)

;; Don't report warnings and errors when native compiling packages, I don't care about other people's code.
(setq native-comp-async-report-warnings-errors nil)

;; Set package mirrors.
(setq package-archives '(("gnu-elpa"  . "https://elpa.gnu.org/packages/") ("melpa"    . "https://melpa.org/packages/")))


(setq-default ring-bell-function 'ignore
              redisplay-dont-pause t
              mac-command-modifier 'meta
              custom-safe-themes t)


;; Setting up variables to help with multi OS codes.
(setq is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu/linux)
      is-macos (eq system-type 'darwin))


(unless (eq system-type 'darwin) (menu-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)                  ;; Distracting


(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))


;; @Overrides: minor mode to register keys that I want to override in all other modes.
(defvar global-override-keys (make-sparse-keymap))
(define-minor-mode global-override-mode ""
  :global t
  :lighter " GlobalOverride"
  :init-value t
  :keymap global-override-keys)

(global-override-mode +1)
(defun GLOBAL (KBD ACTION) (define-key global-override-keys KBD ACTION))

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; In macos set title bar color automatically everytime background color of emacs changes.
(use-package ns-auto-titlebar :ensure t
  :if (eq system-type 'darwin)
  :config (ns-auto-titlebar-mode +1))



;; Add directory to exec-path and also set emacs process PATH variable.
(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(if (eq system-type 'windows-nt) (setenv "PATH" (string-join exec-path ";")) (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH


;; @Theme @UI
(use-package nerd-icons :ensure t)

(use-package amirrezathemes
  :defer t
  :init (add-to-list 'custom-theme-load-path (expand-file-name "amirrezathemes" (expand-file-name "elpa" user-emacs-directory)))
  :vc (:url "https://github.com/amirrezaask/amirrezathemes"))

(use-package ef-themes :ensure t)
(use-package doom-themes :ensure t)
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(load-theme 'doom-one t)

;; better scrolling experience.
(pixel-scroll-precision-mode +1)

;; Wrap long lines
(toggle-truncate-lines -1)

;; Don't choke on minified code.
(global-so-long-mode +1)

;; Always use UTF8
(set-default-coding-systems 'utf-8)

;; Auto revert to disk changes, do we really want this ??
(global-auto-revert-mode +1)

;; Highlight current line.
(global-hl-line-mode +1)

;; Delete selected region before inserting.
(delete-selection-mode +1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)
;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)
(global-display-line-numbers-mode +1)

;; no emacs ~ backup files
(setq make-backup-files nil)

;; Don't prompt if encounter a symlink file, just follow the link.
(setq vc-follow-symlinks t)

;; Using C-l always puts cursor at the middle.
(setq recenter-positions '(middle))

(setq kill-whole-line t)

(use-package string-inflection :ensure t)

(use-package multiple-cursors :ensure t
  :bind
  (("C-S-n" . mc/mark-next-like-this)
   ("C-S-p" . mc/mark-previous-like-this)))

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

(global-set-key (kbd "C-;")   'goto-line)
(global-set-key (kbd "C-w")   'cut) ;; modern cut
(global-set-key (kbd "C-z")   'undo) ;; undo
(global-set-key (kbd "M-z")   'undo) ;; undo
(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection
(global-set-key (kbd "M-w")   'copy) ;; modern copy
(global-unset-key (kbd "M-z")) ;; UNUSED
(global-unset-key (kbd "M-l")) ;; UNUSED
(global-set-key (kbd "C-x i") 'EDIT) ;; Edit this file.
(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-RET")           'indent-buffer) ;; Format buffer
(global-set-key (kbd "C-/")             'comment-line) ;; Comment
(GLOBAL         (kbd "C-<return>")      'save-buffer)
(GLOBAL         (kbd "C--")             'text-scale-decrease)
(GLOBAL         (kbd "C-=")             'text-scale-increase)
(GLOBAL         (kbd "M-n")             'jump-down)
(GLOBAL         (kbd "M-p")             'jump-up)
(GLOBAL         (kbd "M-k")             'kill-current-buffer)
(global-set-key (kbd "M-q")             'quoted-insert)
(global-set-key (kbd "C-o")             'other-window)
(GLOBAL         (kbd "M-r")             'replace-regexp)
(GLOBAL         (kbd "M-s")             'consult-ripgrep)
(GLOBAL         (kbd "M-o")             'project-find-file)


(with-eval-after-load 'replace
  (define-key query-replace-map (kbd "<return>") 'act))


(defun EDIT () "Edit this file." (interactive) (find-file INIT-FILE))

(defun RELOAD ()  (interactive) (load-file INIT-FILE))

;; @Font
(setq
 font-size 11
 font-family "")

(setq font-families (font-family-list))
(defun load-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(defun set-font-size (size) (interactive (list (read-number "Size: ")))
       (setq font-size size)
       (load-font current-font-family font-size))

(load-font "Hack" 15)

;; @Splits
;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

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

;; @Completion
(setq completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(global-set-key (kbd "C-j")             'completion-at-point)

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
(use-package marginalia     :ensure t :config (marginalia-mode +1))
(use-package minibuffer :after embark :bind (:map minibuffer-mode-map ("C-;" . embark-export)))


;; @LSP @IDE
(use-package xref
  :bind
  ((("M-."  .  'xref-find-definitions))
   (("M-,"  . 'xref-go-back))
   (("M-?"  . 'xref-find-references))
   (("M-/"  . 'xref-find-references))))

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

;; @Languages modes
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package go-mode   :ensure t)
(use-package rust-mode :ensure t)
(use-package php-mode  :ensure t)

;; @Compile and @grep
(setq compilation-always-kill t       ; kill compilation process before starting another
      compilation-ask-about-save nil  ; save all buffers on `compile'
      compilation-scroll-output 'first-error)

(GLOBAL (kbd "M-m")   'compile-project)
(GLOBAL (kbd "C-M-s") 'grep-project)

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
