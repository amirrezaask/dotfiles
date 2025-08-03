(line-number-mode +1)
(column-number-mode +1)

(defun home (path) (expand-file-name path (getenv "HOME")))
(add-to-list 'exec-path (home ".local/bin"))
(add-to-list 'exec-path (home "go/bin"))
(add-to-list 'exec-path (home ".cargo/bin"))
(add-to-list 'exec-path (home "bin"))
(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(if (eq system-type 'windows-nt) (setenv "PATH" (string-join exec-path ";")) (setenv "PATH" (string-join exec-path ":"))) ;; set emacs process PATH

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa"    . "https://melpa.org/packages/")))

(setq packages '(corfu wgrep))

(defun install-optional-packages ()
  (interactive)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)
(setq recenter-positions '(middle))
(setq kill-whole-line t)
(setq package-install-upgrade-built-in t)
(setq debug-on-error nil)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t)
(setq native-comp-async-report-warnings-errors nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq-default ring-bell-function 'ignore)
(setq ns-pop-up-frames nil)
(setq mac-command-modifier 'meta)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(load-theme 'tango-dark)

(setq
 echo-keystrokes 0.02
 enable-recursive-minibuffers t
 completions-format 'one-column
 completions-max-height 15
 completion-ignore-case t
 tab-always-indent 'complete
 completion-styles '(basic partial-completion substring flex)
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 completion-show-help nil
 completions-detailed t
 completions-group t
 completion-auto-help 'visible
 completion-auto-select 'second-tab
 completions-header-format nil
 )


(keymap-set minibuffer-local-map "C-p" #'minibuffer-previous-completion)
(keymap-set minibuffer-local-map "C-n" #'minibuffer-next-completion)

(when (package-installed-p 'corfu)
  (setq corfu-auto t)
  (global-corfu-mode +1))

(pixel-scroll-precision-mode +1)
(toggle-truncate-lines -1)
(global-so-long-mode +1)
(set-default-coding-systems 'utf-8)
(global-auto-revert-mode +1)
(delete-selection-mode +1)

(global-set-key (kbd "C-/")        'comment-line)
(global-set-key (kbd "C-<return>") 'save-buffer)
(global-set-key (kbd "C-;")        'goto-line)
(global-set-key (kbd "C-SPC")      'set-mark-command)
(global-set-key (kbd "M-RET")      'indent-buffer)
(global-set-key (kbd "M-q")        'quoted-insert)
(global-set-key (kbd "M-r")        'replace-regexp)
(global-set-key (kbd "C--")        'text-scale-decrease)
(global-set-key (kbd "C-=")        'text-scale-increase)

(setq split-width-threshold 160 split-height-threshold nil)

(advice-add 'split-window-right :after 'balance-windows)
(advice-add 'split-window-below :after 'balance-windows)
(advice-add 'delete-window      :after 'balance-windows)


(with-eval-after-load 'replace (define-key query-replace-map (kbd "<return>") 'act))

(defun indent-buffer () "Indent an entire buffer using the default intenting scheme."
       (interactive)
       (save-excursion
         (delete-trailing-whitespace)
         (indent-region (point-min) (point-max) nil)
         (untabify (point-min) (point-max))))

(defun jump-up ()
  (interactive)
  (next-line (* -1 (/ (window-height) 2))) (recenter-top-bottom))

(defun jump-down ()
  (interactive)
  (next-line (/ (window-height) 2)) (recenter-top-bottom))

(keymap-set global-map "M-n" 'jump-down)
(keymap-set global-map "M-p" 'jump-up)

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

(defun project-compilation-buffer-name (MODE)
  (format "*compilation-%s*" (project-root (project-current))))

(setq project-compilation-buffer-name-function 'project-compilation-buffer-name)

(defun project-grep (&optional EDIT)
  (interactive "P")
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (grep (format "rg --no-heading --color=\"never\" %s" (read-string "Grep: ")))))

(defun project-ansi-term ()
  (interactive)
  (let ((default-directory (if (project-current) (project-root (project-current)) default-directory)))
    (ansi-term "/bin/zsh" (format "ansi-term-%s" (project-root (project-current))))))

(add-to-list 'project-switch-commands '(project-grep "Grep"))

(keymap-set project-prefix-map "g" 'project-grep)
(keymap-set project-prefix-map "s" 'project-async-shell-command)
(keymap-set project-prefix-map "t" 'project-ansi-term)

(global-set-key (kbd "C-x p g") 'project-grep)
(global-set-key (kbd "C-x p t") 'project-ansi-term)
(global-set-key (kbd "C-x p s") 'project-async-shell-command)

(when (package-installed-p 'wgrep)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e"))

(global-set-key (kbd "C-x i") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))

(with-eval-after-load 'grep
  (keymap-set grep-mode-map "k" 'kill-compilation)
  (keymap-set grep-mode-map "G" (lambda () (interactive) (recompile t))))

(setq
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error
 )

(with-eval-after-load 'compile
  (keymap-set compilation-mode-map "k" 'kill-compilation)
  (keymap-set compilation-mode-map "G" (lambda () (interactive) (recompile t))))

(global-set-key (kbd "M->") 'xref-find-references)

(global-set-key (kbd "M-[")  'kmacro-start-macro)
(global-set-key (kbd "M-]")  'kmacro-end-or-call-macro)
(global-set-key (kbd "M-\\") 'kmacro-end-and-call-macro)

(setq eldoc-echo-area-use-multiline-p nil)
(setq eglot-ignored-server-capabilities '(
                                          :documentHighlightProvider
                                          :documentOnTypeFormattingProvider
                                          :documentLinkProvider
                                          :colorProvider
                                          :foldingRangeProvider
                                          :executeCommandProvider
                                          :inlayHintProvider
                                          ))
(setq eglot-stay-out-of '(project flymake))
(setq eglot-sync-connect nil)
(setq eglot-events-buffer-size 0)

(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'php-mode-hook #'eglot-ensure)


(with-eval-after-load 'eglot
  (defun eglot-organize-imports () (interactive) (eglot-code-actions nil nil "source.organizeImports" t))
  (defun eglot-organize-imports-format () (interactive) (eglot-format) (eglot-organize-imports))
  
  (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
  (keymap-set eglot-mode-map "M-i" 'eglot-find-implementation)
  (keymap-set eglot-mode-map "M-RET" 'eglot-organize-imports-format)
  (keymap-set eglot-mode-map "C-c C-r" 'eglot-rename)
  (keymap-set eglot-mode-map "C-c C-c" 'eglot-code-actions)
  )

