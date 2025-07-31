;; Font
(set-face-attribute 'default nil :font "GoMono-13")

;; Show line number and column in modeline
(line-number-mode +1)
(column-number-mode +1)

(setq is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu/linux)
      is-macos (eq system-type 'darwin))

;; Add directory to exec-path and also set emacs process PATH variable.
(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(if is-windows (setenv "PATH" (string-join exec-path ";")) (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

;; Set package mirrors.
(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")))

(setq packages '(
                 ;; vertico
                 ;; consult
                 ;; embark
                 ;; embark-consult
                 corfu
                 wgrep
                 ;; gruber-darker-theme
                 ;; base16-theme
                 ))

(defun install-optional-packages ()
  (interactive)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;; Update builtin packages as well.
(setq package-install-upgrade-built-in t)

;; Stack traces on errors.
(setq debug-on-error nil)

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

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; No flashing and sounds.
(setq-default ring-bell-function 'ignore)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; In macos use CMD key as Meta.
(setq mac-command-modifier 'meta)

;; Cursor blinking is both distracting and CPU consuming.
(blink-cursor-mode -1)

;; Since on macos menubar is a section that is wasted anyway let's just have it.
(menu-bar-mode -1)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(setq alpha-level 80)

;; for some reason macos version uses different face attribute than the linux/windows port.
(when is-macos
  (set-frame-parameter (selected-frame) 'alpha alpha-level)
  (add-to-list 'default-frame-alist `(alpha . ,alpha-level)))

(unless is-macos
  (set-frame-parameter (selected-frame) 'alpha-background alpha-level)
  (add-to-list 'default-frame-alist `(alpha-background . ,alpha-level)))

(when (and is-macos (fboundp 'ns-auto-titlebar-mode)
           (ns-auto-titlebar-mode +1)))

;; Load all themes without asking for permission.
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; This advice will make sure that themes don't get stacked on top of each other when loading.
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(load-theme 'tango-dark)

(setq
  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
  ;; feedback after typing is better UX than no feedback at all.
  echo-keystrokes 0.02

  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
  ;; while we're in the minibuffer.
  enable-recursive-minibuffers t

  ;; Single column for completion items.
  completions-format 'one-column

  completions-max-height 15

  ;; case insensitive search and sorting of the candidates.
  completion-ignore-case t

  ;; TAB will first try to indent the line then acts as 'complete-at-point
  tab-always-indent 'complete

  completion-styles '(basic partial-completion substring flex)

  ;; same as completeion-ignore-case but for buffers.
  read-buffer-completion-ignore-case t

  ;; same as completeion-ignore-case but for files.
  read-file-name-completion-ignore-case t

  ;; Don't show help message in *Completions* buffer
  completion-show-help nil

  ;; display completions with details added as prefix/suffix.
  completions-detailed t

  ;; Enables grouping of completion candidates on Emacs.
  completions-group t

  completion-auto-help 'visible

  ;; Switch to *completions* buffer on second TAB, first TAB will just show the *completions* buffer.
  completion-auto-select 'second-tab

  completions-header-format nil
 )


(if (package-installed-p 'vertico)
    (progn
      (vertico-mode +1))
  (progn
    (keymap-set minibuffer-local-map "C-p" #'minibuffer-previous-completion)
    (keymap-set minibuffer-local-map "C-n" #'minibuffer-next-completion)))

(when (package-installed-p 'embark)
  (keymap-set minibuffer-local-map "C-;" 'embark-export))

;; If we want better autocomplete experience.
(when (package-installed-p 'corfu)
  (setq corfu-auto t)
  (global-corfu-mode +1))

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
;; (global-hl-line-mode +1)

;; Delete selected region before inserting.
(delete-selection-mode +1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; no emacs ~ backup files
(setq make-backup-files nil)

;; Don't prompt if encounter a symlink file, just follow the link.
(setq vc-follow-symlinks t)

;; Using C-l always puts cursor at the middle.
(setq recenter-positions '(middle))

(setq kill-whole-line t)

(global-set-key (kbd "C-/") 'comment-line) ;; Comment
(global-set-key (kbd "C-<return>") 'save-buffer)

(global-set-key (kbd "C-;") 'goto-line) ;;

(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection

(global-set-key (kbd "M-RET") 'indent-buffer) ;; Format buffer

(global-set-key (kbd "M-q") 'quoted-insert)

;; search/replace
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))
(global-set-key (kbd "M-r") 'replace-regexp)

;; By default emacs resizes font with C-x -/+ but it's faster this way.
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)


(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme."
       (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))


;; jump-up/down are utility functions that I use to move around code to emulate C-d/u functionality from vim.
(defun jump-up ()
  (interactive)
  (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))

(defun jump-down ()
  (interactive)
  (next-line (/ (window-height) 2)) (recenter-top-bottom))


(keymap-set global-map "C-v" 'jump-down)
(keymap-set global-map "M-v" 'jump-up)

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

(global-set-key (kbd "C-w") 'cut)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-w") 'copy)


(defun project-grep (&optional EDIT)
  (interactive "P")
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))

(defun project-ansi-term ()
  (interactive)
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (ansi-term "/bin/zsh" (format "ansi-term-%s" (project-root (project-current))))))

(define-key project-prefix-map (kbd "g") (if (package-installed-p 'consult) 'consult-ripgrep 'project-grep))
(define-key project-prefix-map (kbd "s") 'project-async-shell-command)
(define-key project-prefix-map (kbd "t") 'project-ansi-term)
(global-set-key (kbd "C-x p g") (if (package-installed-p 'consult) 'consult-ripgrep 'project-grep))
(global-set-key (kbd "C-x p t") 'project-ansi-term)
(global-set-key (kbd "C-x p s") 'project-async-shell-command)

;; TODO: project-switch-to-buffer is really handy but even better would be to have project-switch-dwim command that supports both files and buffers.
(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-dir "Find directory")
        (project-switch-to-buffer "Switch to buffer")
        (project-grep "Grep")
        (project-eshell "Eshell")
        (project-ansi-term "AnsiTerm")))


;; Wgrep ( writable GREP buffers)
(when (package-installed-p 'wgrep)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e"))

(defun system/configs ()
  (interactive)
  (let ((default-directory "~/src/github/dotfiles"))
    (call-interactively 'project-find-file)))

(global-set-key (kbd "C-x i") 'system/configs)

;; to make project-grep function even better we add keys to grep-mode buffers so we can kill a grep process and restart it.
(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k") 'kill-compilation)
  (define-key grep-mode-map (kbd "G") (lambda () (interactive) (recompile t))))

;; kill compilation process before starting another
(setq compilation-always-kill t)

;; save all buffers on `compile'
(setq compilation-ask-about-save nil)

;; scroll to first error in compile buffer.
(setq compilation-scroll-output 'first-error)

;; same keys as grep buffers.
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k") 'kill-compilation)
  (define-key compilation-mode-map (kbd "G") (lambda () (interactive) (recompile t))))

(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

;; Eglot (LSP Client)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions))

(setq eldoc-echo-area-use-multiline-p nil)
(setq eglot-ignored-server-capabilities '( ;; Disable fancy LSP features.
                                          :documentHighlightProvider           ;; "Highlight symbols automatically"
                                          :documentOnTypeFormattingProvider    ;; "On-type formatting"
                                          :documentLinkProvider                ;; "Highlight links in document"
                                          :colorProvider                       ;; "Decorate color references"
                                          :foldingRangeProvider                ;; "Fold regions of buffer"
                                          :executeCommandProvider              ;; "Execute custom commands"
                                          :inlayHintProvider                   ;; "Inlay hints"
                                          ))
(setq eglot-stay-out-of '(project flymake)) ;; Don't polute buffer with flymake diganostics.
(setq eglot-sync-connect nil)               ;; no blocking on waiting for the server to start.
(setq eglot-events-buffer-size 0)           ;; no logging of LSP events.

(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'php-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

;; Splits
;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

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

(global-set-key (kbd "C-x 0") 'delete-window-and-balance)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-below-balance-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-balance-and-switch)

(defun user/eshell-mode-hook ()
  (setenv "TERM" "xterm-256color"))

(add-hook 'eshell-mode-hook 'user/eshell-mode-hook)

(defun user/eshell-prompt-function ()
  (format " %s > "
          (propertize (if (project-current) (project-name (project-current)) default-directory) 'face 'font-lock-warning-face)))


(setq eshell-prompt-function 'user/eshell-prompt-function)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M->") 'xref-find-references)
