;; Setting up variables to help with multi OS codes.
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

;; Set package mirrors.
(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")))

;; Update builtin packages as well.
(setq package-install-upgrade-built-in t)

;; No flashing and sounds.
(setq-default ring-bell-function 'ignore)

;; In macos use CMD key as Meta.
(setq mac-command-modifier 'meta)

;; Cursor blinking is both distracting and CPU consuming.
(blink-cursor-mode -1)

;; Since on macos menubar is a section that is wasted anyway let's just have it.
(unless is-macos (menu-bar-mode -1))

(scroll-bar-mode -1)

(tool-bar-mode -1)

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(global-set-key (kbd "C-x i") (lambda () (interactive) (find-file INIT-FILE))) ;; Edit this file.

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; In macos set title bar color automatically everytime background color of emacs changes.
;; (when is-macos
;;   (ensure-package 'ns-auto-titlebar)

(when (and is-macos (package-installed-p 'ns-auto-titlebar)
    (ns-auto-titlebar-mode +1)))

;; Load all themes without asking for permission.
(setq custom-safe-themes t)

;; Set of custom themes that I made from streams of jonathan blow, cmuratori, ryan fleury.
;; (ensure-package 'ef-themes)
;; (ensure-package 'modus-themes)

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(setopt modus-vivendi-palette-overrides ;; palenight like colors
        `((bg-main "#292D3E")
          (bg-active bg-main)
          (fg-main "#EEFFFF")
          (fg-active fg-main)
          (bg-line-number-inactive bg-main)
          (bg-line-number-active bg-main)
          (fringe unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fg-mode-line-active "#A6Accd")
          (bg-mode-line-active "#232635")
          (fg-mode-line-inactive "#676E95")
          (bg-mode-line-inactive "#282c3d")
          (bg-tab-bar      "#242837")
          (bg-tab-current  bg-main)
          (bg-tab-other    bg-active)
          (fg-prompt "#c792ea")
          (bg-prompt unspecified)
          (bg-hover-secondary "#676E95")
          (bg-completion "#2f447f")
          (fg-completion white)
          (bg-region "#3C435E")
          (fg-region white)

          (fg-heading-0 "#82aaff")
          (fg-heading-1 "#82aaff")
          (fg-heading-2 "#c792ea")
          (fg-heading-3 "#bb80b3")
          (fg-heading-4 "#a1bfff")

          (fg-prose-verbatim "#c3e88d")
          (bg-prose-block-contents "#232635")
          (fg-prose-block-delimiter "#676E95")
          (bg-prose-block-delimiter bg-prose-block-contents)

          (accent-1 "#79a8ff")

          (keyword "#89DDFF")
          (builtin "#82aaff")
          (comment "#676E95")
          (string "#c3e88d")
          (fnname "#82aaff")
          (type "#c792ea")
          (variable "#ffcb6b")
          (docstring "#8d92af")
          (constant "#f78c6c")))

;; better color background for ef-bio
;; similar in tone with jonathan blow setup.
(setq ef-bio-palette-overrides
      '((bg-main "#052525")))

(load-theme 'modus-vivendi t)

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


;; Setting up modeline
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

;; Project.el is emacs builtin package to work with projects.
;; by default It uses C-x p acs prefix.
;; It has functionality to search in project but it's slow, so I use a custom function for that.
(defun project-grep (&optional EDIT)
  (interactive "P")
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))
(define-key project-prefix-map (kbd "g") 'project-grep)
(global-set-key (kbd "C-x p g") 'project-grep)

;; TODO: project-switch-to-buffer is really handy but even better would be to have project-switch-dwim command that supports both files and buffers.

;; to make project-grep function even better we add keys to grep-mode buffers so we can kill a grep process and restart it.
(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k") 'kill-compilation)
  (define-key grep-mode-map (kbd "G") (lambda () (interactive) (recompile t))))

(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-dir "Find directory")
        (project-grep "Grep")
        (project-eshell "Eshell")))

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

;; search/replace
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))
(global-set-key (kbd "M-r") 'replace-regexp)

;; macros, i don't use but let's have better keys
(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

;; By default emacs resizes font with C-x -/+ but it's faster this way.
(global-set-key   (kbd "C--") 'text-scale-decrease)
(global-set-key   (kbd "C-=") 'text-scale-increase)

;; Set font.
(set-face-attribute 'default nil :font "Jetbrains Mono-15")

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

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme."
       (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))


;; Completion
(setq completions-format 'one-column) ;; vertical
(setq completions-max-height 15)
(setq completion-auto-select t) ;; automatically switch to completion window.
(setq completion-auto-help t)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete) ;; TAB will first try to indent the line then acts as 'complete-at-point
(setq completion-styles '(basic partial-completion substring flex))
(setq read-buffer-completion-ignore-case t) ;; same as completeion-ignore-case but for buffers.
(setq read-file-name-completion-ignore-case t) ;; same as completeion-ignore-case but for files.
(setq completion-show-help nil) ;; Don't show help message in *Completions* buffer
(setq completions-detailed t) ;; display completions with details added as prefix/suffix.
(setq completions-group t)
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab) ;; On first TAB show completion window and on second TAB switch to it.
(setq completions-header-format nil) ;; 

(fido-vertical-mode +1)

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

(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'php-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

;; xref is emacs infrastructure that provides functionality to jump to definition, references, ...
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-?") 'xref-find-references)
(global-set-key (kbd "M-/") 'xref-find-references)


