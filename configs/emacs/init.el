;; -*- lexical-binding: t; -*-
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

;;;;
; Optimize startup performance by adjusting GC and file handler settings.
;;;;
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(defvar my/original-gc-cons-percentage gc-cons-percentage)
(defvar my/original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore to reasonable defaults
            (setq gc-cons-threshold my/original-gc-cons-threshold
                  gc-cons-percentage my/original-gc-cons-percentage
                  file-name-handler-alist my/original-file-name-handler-alist)
            ;; Optionally, run GC once after startup
            (garbage-collect)
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))


;; Cursor blinking is both distracting and CPU consuming.
(blink-cursor-mode -1)

;; Since on macos menubar is a section that is wasted anyway let's just have it.
(unless is-macos (menu-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq-default cursor-type 'bar)

;;; Stack traces on errors.
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

;; No flashing and sounds.
(setq-default ring-bell-function 'ignore)

;; In macos use CMD key as Meta.
(setq mac-command-modifier 'meta)

;; setting up package manager
(setq package-quickstart t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Set package mirrors.
(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")))
;; Update builtin packages as well.
(setq package-install-upgrade-built-in t)

(defun ensure-package (package) "Ensures a package is installed through package.el"
       (unless (package-installed-p package) (package-install package)))

(defun ensure-package-vc (package repo) "Same as ensure-package but get's package from a VC backend."
  (unless (package-installed-p package) (package-vc-install package repo)))

(set-frame-font "Maple Mono-17" nil t)

(setq-default custom-file "~/.custom.el")

;; No startup screen.
(setq inhibit-startup-screen t)

;; better scrolling experience.
(pixel-scroll-precision-mode +1)


;; Highlight current line
(global-hl-line-mode +1)
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

;; Enable line numbers globally.
(global-display-line-numbers-mode +1)

;; no emacs ~ backup files
(setq make-backup-files nil)

;; Don't prompt if encounter a symlink file, just follow the link.
(setq vc-follow-symlinks t)

;; Using C-l always puts cursor at the middle.
(setq recenter-positions '(middle))

(setq kill-whole-line t)

;; handy string transformation functions.
(ensure-package 'string-inflection)

;; Multiple cursors.
(ensure-package 'multiple-cursors)
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-g") 'mc/mark-all-like-this-dwim)



;; jump-up/down are utility functions that I use to move around code to emulate C-d/u functionality from vim.
(defun jump-up ()
  (interactive)
  (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))

(defun jump-down ()
  (interactive)
  (next-line (/ (window-height) 2)) (recenter-top-bottom))

(global-set-key (kbd "M-n") 'jump-down)
(global-set-key (kbd "M-p") 'jump-up)

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

(global-set-key (kbd "C-/") 'comment-line) ;; Comment
(global-set-key (kbd "C-<return>") 'save-buffer)

(global-set-key (kbd "C-;") 'goto-line) ;;

(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection

(global-set-key (kbd "M-RET") 'indent-buffer) ;; Format buffer

(global-set-key (kbd "M-q") 'quoted-insert)


;; Themes
(setq custom-safe-themes t)
(ensure-package 'catppuccin-theme)
(ensure-package 'ef-themes)
(load-theme 'catppuccin)

;; Autocomplete UI
(ensure-package 'corfu)
(setq corfu-auto t)
(global-corfu-mode +1)
(global-set-key (kbd "C-j") 'completion-at-point)

;; Minibuffer completion
(ensure-package 'vertico)
(ensure-package 'consult)
(setq vertico-count 20)
(vertico-mode +1)

(ensure-package 'go-mode)
(ensure-package 'php-mode)

(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'php-mode-hook #'eglot-ensure)

      
;; magit is emacs git client.
(ensure-package 'magit)
(global-set-key (kbd "C-x g") 'magit)

;; 
;; by default It uses C-x p acs prefix.
;; It has functionality to search in project but it's slow, so I use a custom function for that.
(defun project-grep (&optional EDIT)
  (interactive "P")
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))
(define-key project-prefix-map (kbd "g") 'project-grep)
(global-set-key (kbd "C-x p g") 'project-grep)


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


(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k") 'kill-compilation)
  (define-key compilation-mode-map (kbd "G") (lambda () (interactive) (recompile t))))


;; search/replace
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))
(global-set-key (kbd "M-r") 'replace-regexp)

;; macros, i don't use but let's have better keys
(global-set-key   (kbd "M-[")  'kmacro-start-macro)
(global-set-key   (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key   (kbd "M-\\") 'kmacro-end-and-call-macro)

;; By default emacs resizes font with C-x -/+ but it's faster this way.
(global-set-key   (kbd "C--") 'text-scale-decrease)
(global-set-key   (kbd "C-=") 'text-scale-increase)


;; Splits
;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
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

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme."
       (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))



;; Completion
;; Orderless provides a searching algorithm to be used in minibuffer completion.
(ensure-package 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

;; LSP
(ensure-package 'eglot)
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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))


;; xref is emacs infrastructure that provides functionality to jump to definition, references, ...
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-?") 'xref-find-references)
(global-set-key (kbd "M-/") 'xref-find-references)

;; Installing support for languages, hopefuly emacs will start shipping all needed tressitter parsers soon and I can remove these.
(ensure-package 'json-mode)
(ensure-package 'yaml-mode)
(ensure-package 'go-mode)
(ensure-package 'rust-mode)
(ensure-package 'php-mode )

;; Evil section ...
(ensure-package 'evil)
(ensure-package 'evil-commentary)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(evil-mode +1)

(evil-global-set-key 'normal (kbd "SPC o") 'find-file)
(evil-global-set-key 'normal (kbd "SPC SPC") 'project-find-file)
(evil-global-set-key 'normal (kbd "SPC j")   'project-grep)
(evil-global-set-key 'normal (kbd "SPC p f") 'project-find-file)
(evil-global-set-key 'normal (kbd "SPC p p") 'project-switch-project)
(evil-global-set-key 'normal (kbd "SPC h v") 'describe-variable)
(evil-global-set-key 'normal (kbd "SPC h f") 'describe-function)

(evil-commentary-mode)

(ensure-package 'evil-collection)
(evil-collection-init)
