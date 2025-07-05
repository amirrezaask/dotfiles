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

(defun ensure-package (package) "Ensures a package is installed through package.el"
       (unless (package-installed-p package) (package-install package)))

(defun ensure-package-vc (package repo) (unless (package-installed-p package) (package-vc-install package repo)))

(setq-default ring-bell-function 'ignore)

(setq mac-command-modifier 'meta)

;; Setting up variables to help with multi OS codes.
(setq is-windows (eq system-type 'windows-nt)
      is-linux (eq system-type 'gnu/linux)
      is-macos (eq system-type 'darwin))


(menu-bar-mode -1)

(scroll-bar-mode -1)

(tool-bar-mode -1)

;; Distracting
(blink-cursor-mode -1)

(when load-file-name ;; since windows is a bit funky I prefer to store this file path in a variable to be used when C-x i
  (setq INIT-FILE load-file-name)
  (setq amirreza-emacs-directory (file-name-directory INIT-FILE))
  (setq custom-file (expand-file-name "custom.el" amirreza-emacs-directory)))

(defun EDIT () "Edit this file." (interactive) (find-file INIT-FILE))

(global-set-key (kbd "C-x i") 'EDIT) ;; Edit this file.

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; In macos set title bar color automatically everytime background color of emacs changes.
(when is-macos
  (ensure-package 'ns-auto-titlebar)
  (ns-auto-titlebar-mode +1))


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

;; Load all themes without asking for permission.
(setq custom-safe-themes t)

(ensure-package-vc 'amirrezathemes  '(:url "https://github.com/amirrezaask/amirrezathemes"))
(add-to-list 'custom-theme-load-path (expand-file-name "amirrezathemes" (expand-file-name "elpa" user-emacs-directory)))

(ensure-package 'ef-themes)

(ensure-package 'doom-themes)

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(setq ef-bio-palette-overrides ;; better color background for ef-bio
      '((bg-main "#052525")))

(load-theme 'doom-palenight t)

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

(ensure-package 'string-inflection)

;; Multiple cursor support in Emacs.
;; TODO: Add keys or alias for matching word under cursor.
(ensure-package 'multiple-cursors)
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)

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

;; 2025 cut/copy/paste
(global-set-key (kbd "C-w") 'cut) ;; modern cut
(global-set-key (kbd "C-z") 'undo) ;; undo
(global-set-key (kbd "M-w") 'copy) ;; modern copy
(global-set-key (kbd "C-/") 'comment-line) ;; Comment
(global-set-key (kbd "C-<return>") 'save-buffer)

(global-set-key (kbd "C-;") 'goto-line) ;;

(global-set-key (kbd "C-SPC") 'set-mark-command) ;; Visual selection

(global-set-key (kbd "M-RET") 'indent-buffer) ;; Format buffer

(global-set-key   (kbd "M-n") 'jump-down)
(global-set-key   (kbd "M-p") 'jump-up)

(global-set-key   (kbd "M-q") 'quoted-insert)

(global-set-key (kbd "M-s")  'consult-line)


;; Project.el is emacs builtin package to work with projects.
;; by default It uses C-x p acs prefix.
;; It has functionality to search in project but it's slow, so I use a custom function for that.
(defun project-grep (&optional EDIT)
  (interactive "P")
  (let ((default-directory (find-project-root-or-default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))
(define-key project-prefix-map (kbd "C-x p g") 'project-grep)

(setq project-switch-commands
      '((project-find-file "Find file")
	(project-find-dir "Find directory")
	(project-grep "Grep")
	(project-eshell "Eshell")
	(magit-project-status "Magit")))


;; kill compilation process before starting another
(setq compilation-always-kill t)

;; save all buffers on `compile'
(setq compilation-ask-about-save nil)

;; scroll to first error in compile buffer.
(setq compilation-scroll-output 'first-error)

(global-set-key (kbd "M-m")   'compile-project)
(global-set-key (kbd "C-M-s") 'grep-project)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "k") 'kill-compilation)
  (define-key compilation-mode-map (kbd "G") (lambda () (interactive) (recompile t))))

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "k") 'kill-compilation)
  (define-key grep-mode-map (kbd "G") (lambda () (interactive) (recompile t))))


;; search/replace
(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))
(global-set-key (kbd "M-r") 'replace-regexp)


;; macros, i don't use but let's have better keys
(global-set-key   (kbd "M-[")  'kmacro-start-macro)
(global-set-key   (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key   (kbd "M-\\") 'kmacro-end-and-call-macro)


(global-set-key   (kbd "C--") 'text-scale-decrease)
(global-set-key   (kbd "C-=") 'text-scale-increase)

(defvar font-size)
(defvar font-family)

(setq font-families (font-family-list))
(defun load-font (font size) "Set font" (interactive (list (completing-read "Font: " font-families) (read-number "Size: ")))
       (setq font-family font)
       (setq font-size size)
       (set-face-attribute 'default nil :font (format "%s-%d" font size)))

(defun set-font-size (size) (interactive (list (read-number "Size: ")))
       (setq font-size size)
       (load-font current-font-family font-size))

(load-font "Hack" 15)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-0") 'delete-window-and-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-balance-and-switch)
(global-set-key (kbd "C-3") 'split-window-right-balance-and-switch)

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


(global-set-key (kbd "C-j") 'completion-at-point)

;; corfu will help us with autocomplete inline.
(ensure-package 'corfu)
(setq corfu-auto t)
(setq corfu-preselect 'prompt)
(global-corfu-mode +1)

(ensure-package 'orderless)
(setq completion-styles '(orderless basic))

;; vertico greatly enhances emacs minibuffer completion facilities but stays compatible with emacs *completion-read-function*.
(ensure-package 'vertico)
(setq vertico-count 15)
(vertico-mode +1)

(setq completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(ensure-package 'consult)

(ensure-package 'embark)
(ensure-package 'embark-consult)

(ensure-package 'consult-eglot)

(ensure-package 'marginalia)
(marginalia-mode +1)

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-;") 'embark-export))


;; xref is emacs infrastructure that provides functionality to jump to definition, references, ...
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-?") 'xref-find-references)
(global-set-key (kbd "M-/") 'xref-find-references)

;; eglot is Emacs built-in LSP client.
(ensure-package 'eglot)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   'eglot-organize-imports-format)
  (define-key eglot-mode-map (kbd "C-c C-c") 'eglot-code-actions)
  )
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

(dolist (mode '(go rust php)) ;; Enable LSP automatically.
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'eglot-ensure))

(with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))) ;; PHP language server intelephense

(defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))

(defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))

;; installing support for languages, hopefuly emacs will start shipping all needed tressitter parsers soon and I can remove these.
(ensure-package 'json-mode)
(ensure-package 'yaml-mode)
(ensure-package 'go-mode)
(ensure-package 'rust-mode)
(ensure-package 'php-mode )
