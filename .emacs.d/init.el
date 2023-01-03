(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")
;; (setq amirreza/font "Source Code Pro")
;; (setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "OperatorMono Nerd Font Light")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")
(setq amirreza/font "Iosevka")
(setq amirreza/font-size "18")
(setq amirreza/theme 'gruber-darker)
(setq amirreza/transparent 100)


(setq vc-follow-symlinks t)
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation
(setq make-backup-files nil) ;; Disable backup files ~file
(setq auto-save-default nil) ;; Disable auto save files
(setq inhibit-startup-screen t) ;; No startup splash screen
(setq use-dialog-box nil) ;; Do not use UI for questions
(setq ring-bell-function 'ignore) ;; Do not beep please.

(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar



;; Copy PATH from default shell
(use-package exec-path-from-shell :config
  (exec-path-from-shell-initialize))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.4)


(defun amirreza/find-file ()
  (interactive)
  (if (vc-backend (buffer-file-name))
      (project-find-file)
    (call-interactively 'find-file)
    ))

(defun amirreza/edit-emacs ()
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory)))


(defun amirreza/getenv (name default)
  "Get env if not defined use default"
  (let ((value (getenv name)))
    (if value
	value
      default
    )))

(use-package general) ;; For a beautiful keymap macro
(use-package hydra) ;; Make your keybindings stick

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package gcmh
  :init
  (gcmh-mode 1)
  )

(defmacro amirreza/defhydra (name body heads)
  `(eval (append '(defhydra ,name ,body) ,heads)))


(general-def :keymaps 'override "C-c e e" 'amirreza/edit-emacs)

(use-package ace-window
  :general
  (:keymaps 'override "C-x o" 'ace-window))


(use-package bufler
  :general
  (:keymaps 'override "C-x C-b" 'bufler)
  )

(use-package winner
  :init
  (winner-mode 1)
  )

(use-package dired :straight nil
  :init
    (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
	dired-hide-details-hide-symlink-targets nil
	;; don't prompt to revert, just do it
	dired-auto-revert-buffer #'dired-buffer-stale-p
	;; Always copy/delete recursively
	dired-recursive-copies  'always
	dired-recursive-deletes 'top
	large-file-warning-threshold nil
	;; Ask whether destination dirs should get created when copying/removing files.
	dired-create-destination-dirs 'ask
	;; Screens are larger nowadays, we can afford slightly larger thumbnails
	image-dired-thumb-size 150)

  :general
  (:keymaps 'dired-mode-map
	    "C-c C-e" 'wdired-change-to-wdired-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(setq
 IS-MAC (string-equal system-type "darwin")
 IS-LINUX (string-equal system-type "linux")
 IS-WINDOWS (string-equal system-type "windows"))

(use-package dired-x
  :straight nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
	dired-omit-files
	(concat dired-omit-files
		"\\|^\\.DS_Store\\'"
		"\\|^\\.project\\(?:ile\\)?\\'"
		"\\|^\\.\\(?:svn\\|git\\)\\'"
		"\\|^\\.ccls-cache\\'"
		"\\|\\(?:\\.js\\)?\\.meta\\'"
		"\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
		       (IS-LINUX "xdg-open")
		       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
	  `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
	    ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
	    ("\\.\\(?:xcf\\)\\'" ,cmd)
	    ("\\.csv\\'" ,cmd)
	    ("\\.tex\\'" ,cmd)
	    ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
	    ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
	    ("\\.html?\\'" ,cmd)
	    ("\\.md\\'" ,cmd)))))


(use-package dired-git-info
  :general
  (:keymaps 'dired-mode-map
	    "C-c m g" 'dired-git-info))

(setq mediaplayer (cond
	   (IS-MAC "/Applications/VLC.app/Contents/MacOS/VLC")
	   (IS-LINUX "vlc")
	   ))


(setq pdfviewer (cond
		 (IS-MAC "open")
		 ))

(setq imageviewer (cond
		   (IS-MAC "open")
		   ))

(use-package openwith
  :init
  (openwith-mode)
  :config
  (setq openwith-associations
	(list
	  (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
		mediaplayer
		'(file))
	  (list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
		  imageviewer
		  '(file))
	  (list (openwith-make-extension-regexp
		'("pdf"))
		pdfviewer
		'(file)))))

(use-package helpful
  :general
  (:keymaps 'global-map
  [remap describe-key] 'helpful-key
  [remap describe-function] 'helpful-callable
  [remap describe-variable] 'helpful-variable))

(use-package all-the-icons)
(use-package ef-themes)
(use-package doom-themes)
(use-package gruber-darker-theme)

;; Add custom themes path to themes load path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

(setq amirreza/--current-theme nil)

(defun amirreza/switch-theme ()
  (interactive)
  (let ((theme (intern (completing-read "Theme: " (mapcar #'symbol-name
							  (custom-available-themes))))))
    (amirreza/load-theme theme)))

(defun amirreza/load-theme (theme)
  (when (not (eq amirreza/--current-theme nil))
    (disable-theme amirreza/--current-theme))
  (setq amirreza/--current-theme theme)
  (load-theme amirreza/--current-theme t))

(amirreza/load-theme amirreza/theme)
(general-def :keymaps 'override "C-c t t" 'amirreza/switch-theme)

(use-package doom-modeline
  :if nil
  :init
  (setq doom-modeline-height 35)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-mode 1))

;; Font settings
(defun amirreza/display-benq ()
  (interactive)
  (setq amirreza/font-size "23")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; My font setup for my laptop setup
(defun amirreza/display-mac ()
  (interactive)
  (setq amirreza/font-size "15")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Interactively ask for font size
(defun amirreza/set-font-size (size)
  (interactive "sSize: ")
  (setq amirreza/font-size size)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Reload font settings
(defun amirreza/reload-font ()
  (interactive)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(amirreza/reload-font)

(setq-default cursor-type 'box) ;; instead of box use a horizontal line.
(set-cursor-color 'red)
(blink-cursor-mode -1) ;; no blinking cursor.
(global-hl-line-mode)

(set-frame-parameter (selected-frame) 'alpha (list amirreza/transparent amirreza/transparent))
(add-to-list 'default-frame-alist (append '(alpha) (list amirreza/transparent amirreza/transparent)))

(use-package corfu
  :straight
  (corfu :type git :host github :repo "emacs-straight/corfu" :files ("*" "extensions/*.el" (:exclude ".git")))

  :init
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  :config
  (global-corfu-mode)
  (corfu-history-mode 1)
  (corfu-echo-mode 1)
  (corfu-popupinfo-mode 1))

(use-package corfu-terminal
  :config
  (corfu-terminal-mode))

(use-package corfu-prescient
  :after prescient
  :config
  (corfu-prescient-mode))

(use-package emacs
  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package vertico
  :init
  (setq vertico-count 15)
  (setq vertico-cycle t)

  :config
  (vertico-mode))

(use-package consult
  :init
  (setq consult-async-min-input 1))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))))


(use-package vertico-prescient
  :after prescient
  :config
  (vertico-prescient-mode))

;; Icons in minibuffer completion
(use-package all-the-icons-completion
  :if nil
  :init
  (all-the-icons-completion-mode))

;; TODO: Maybe a context like completion
;; for example in org mode have a key to open minibuffer with just org mode functions

(use-package olivetti
  :init
  (setq olivetti-body-width 100))

;; Search and replace beautifuly
(use-package wgrep)
;; Ripgrep
(use-package rg)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package delsel
  :straight nil
  :config
  (delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
  )

(use-package paren
  :straight nil
  :init
  (setq show-paren-delay 0) ;; highlight matching parens instantly.
  :config
  (show-paren-mode 1) ;; Highlight matching parens
  )

(use-package display-line-numbers
  :straight nil
  :init
  (setq display-line-numbers-type 'relative) ;; relative line numbers
  :config
  (global-display-line-numbers-mode 1) ;; enable line numbers globaly
  )

(defun amirreza/up-center ()
  (interactive)
  (previous-line (/ (window-height) 2))
  (recenter-top-bottom))

(defun amirreza/down-center ()
  (interactive)
  (next-line (/ (window-height) 2))
  (recenter-top-bottom))

;; Best movement ever ?????
(setq recenter-positions '(middle))

(general-def :keymaps 'global-map
  "M-p" 'amirreza/up-center
  "M-n" 'amirreza/down-center
  )

(use-package expand-region
  :general
  (:keymaps 'global-map
	    "C-=" 'er/expand-region
	    "C--" 'er/contract-region
	    ))

;; really important key if you use emacs in terminal
(use-package simple
  :straight nil
  :general
  (:keymaps 'override "C-q" 'set-mark-command))

(use-package org
  :straight nil
  :init
  (setq org-use-property-inheritance t)
  (setq org-startup-folded t) ;; Start org mode all headers collapsed
  (setq org-src-window-setup 'current-window)
  (setq org-src-tab-acts-natively nil)
  (defun amirreza/org-code-block ()
    (interactive)
    (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC"
		    (completing-read "Language: "
				     '("emacs-lisp"
				       "go"
				       "rust"
				       "python"
				       "lua"
				       "bash"
				       "sh"
				       "fish"
				       "java"
				       )))))

  (defun amirreza/org-disable-tangle ()
    (interactive)
    (insert ":PROPERTIES:
:header-args:    :tangle no
:END:"
	    ))

  (defhydra amirreza/org-mode-hydra (:exit t)
    ("l" org-toggle-link-display "Toggle Link Display")
    ("b" amirreza/org-code-block "Insert a Code Block")
    ("n" amirreza/org-disable-tangle "Disable Tangle PROPERTIES")
    ("e" org-export-dispatch "Export")
    ("o" org-open-at-point "Open At Point")
    ("h" (lambda () (interactive) (org-export-as 'html)) "Org Export To HTML")
    ("t"  org-todo "Open At Point")
    )
  :general
  (:keymaps 'org-mode-map
	    "C-c m" 'amirreza/org-mode-hydra/body)
  (:keymaps 'org-src-mode-map
	    "C-c C-c" #'org-edit-src-exit
	    )
  (:states 'normal :keymaps 'org-mode-map "SPC m" 'amirreza/org-mode-hydra/body)
  )

(use-package ox-reveal)
(use-package ob-go)
(use-package ob-rust)
(use-package ob-php)
(use-package htmlize)
(use-package evil-org
  :if (boundp 'IS-EVIL) ;; Only if evil mode is enabled
  :hook (org-mode . evil-org-mode))

(use-package git-gutter
  :init
  (global-git-gutter-mode))

(use-package magit
  :general
  (:keymaps 'global-map "C-x g" 'magit)
  (:states 'normal "SPC g" 'magit)
  )

(use-package project
  :straight nil
  :general
  (:keymaps 'override "C-x p" 'amirreza/project-hydra/body)
  :init
  (defhydra amirreza/project-hydra (:exit t)
    ("f" project-find-file "Find File")
    ("p" project-switch-project "Switch To Project")
    ("b" project-buffers "Find Buffer In Project")
    ("c" project-compile "Compile Project")
  ))

(setq amirreza/programming-hydra-heads '())

(use-package flymake
  :straight nil
  :init
  (add-to-list 'amirreza/programming-hydra-heads '("n" flymake-goto-next-error "Goto Next Error"))
  (add-to-list 'amirreza/programming-hydra-heads '("p" flymake-goto-previous-error "Goto Previous Error"))
  (add-to-list 'amirreza/programming-hydra-heads '("e" consult-flymake "List of errors")))

(use-package xref
  :straight nil
  :general
    (:keymaps 'global-map
	      "M-." 'xref-find-definitions ;; Goto definitions
	      "M-," 'xref-go-back ;; hop back where you where before jump
	      "M-r" 'xref-find-references ;; Goto references
  ))

(use-package eldoc
  :straight nil
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer nil)
  (add-to-list 'amirreza/programming-hydra-heads '("." amirreza/eldoc-toggle-buffer "Toggle Eldoc for point"))
  :general
  (:keymaps 'global-map
		"C-h ." 'amirreza/eldoc-toggle-buffer ;; Toggle eldoc buffer
		"M-0" 'amirreza/eldoc-toggle-buffer ;; Toggle eldoc buffer
		)

  :config
  (setq amirreza/--eldoc-window-open 'close)

  (defun amirreza/eldoc-toggle-buffer ()
    "Toggle eldoc buffer."
    (interactive)
    (if (eq 'open amirreza/--eldoc-window-open)
	(progn
	  (message "closing...")
	  (dolist (w (window-list))
	    (when (string-match-p "\\*eldoc.*" (buffer-name (window-buffer w)))
	      (quit-window nil w)
	      ))
	  (setq amirreza/--eldoc-window-open 'close))
      (progn
	(message "opening...")
	(eldoc-doc-buffer t)
	(setq amirreza/--eldoc-window-open 'open))
      ))
  (global-eldoc-mode))

(use-package eglot
  :straight nil
  :hook
  ((go-mode rust-mode python-mode php-mode) . 'eglot-ensure)
  :init
  (add-to-list 'amirreza/programming-hydra-heads '("d" eldoc "Document THING at POINT"))
  (add-to-list 'amirreza/programming-hydra-heads '("D" xref-find-definitions "Goto Definitions"))
  (add-to-list 'amirreza/programming-hydra-heads '("r" xref-find-references "Find References"))
  (add-to-list 'amirreza/programming-hydra-heads '("i" eglot-find-implementation "Find Implementations"))
  (add-to-list 'amirreza/programming-hydra-heads '("s" consult-eglot-symbols "Workspace Symbols"))
  (add-to-list 'amirreza/programming-hydra-heads '("R" eglot-rename "Rename"))
  (add-to-list 'amirreza/programming-hydra-heads '("f" eglot-format "Format")))

  (use-package consult-eglot)

;; If a language has no specific keys other that programming one like rust they can map this.
(amirreza/defhydra amirreza/programming-hydra (:exit t)
		   amirreza/programming-hydra-heads)

(general-def 
  :keymaps 'prog-mode-map "C-c m" 'amirreza/programming-hydra/body)


(general-def
  :keymaps 'prog-mode-map
  :states 'normal
  "SPC m" 'amirreza/programming-hydra/body)

(use-package go-mode
  :init
  (amirreza/defhydra amirreza/go-hydra
		     (:exit t)
		     (append amirreza/programming-hydra-heads '(("a" go-tag-add "Add Struct Tag"))))
  :general
  (:keymaps 'go-mode-map
	    "C-c m" 'amirreza/go-hydra/body)
  (:keymaps 'go-mode-map
  :states 'normal
	    "SPC m" 'amirreza/go-hydra/body)
  )


(use-package go-tag)

(use-package rust-mode)

(use-package clojure-mode) ;; LISP on JVM
(use-package cider :after clojure-mode) ;; Clojure repl integrated into Emacs

(use-package zig-mode) ;; Zig

(use-package apache-mode) ;; Apache config syntax
(use-package systemd) ;; Systemd config syntax
(use-package nginx-mode) ;; Nginx config syntax
(use-package docker-compose-mode) ;; Docker-compose syntax
(use-package dockerfile-mode) ;; Dockerfile syntax
(use-package markdown-mode) ;; Markdown syntax
(use-package yaml-mode) ;; Yaml
(use-package fish-mode) ;; Fish
(use-package csv-mode) ;; CSV
(use-package json-mode
  :init
  (setq amirreza/json-hydra-heads '(
				    ("f" json-pretty-print "Pretty print region")
				    ("F" json-pretty-print-buffer "Pretty print buffer")
				    ))
  (amirreza/defhydra amirreza/json-hydra (:exit t) amirreza/json-hydra-heads)
  :general
  (:keymaps 'json-mode-map
	    "C-c m" 'amirreza/json-hydra/body
	    )
  (:states 'normal :keymaps 'json-mode-map "SPC m" 'amirreza/json-hydra/body)
  )

(use-package consult-jq)

(use-package perspective
  :init

  (setq persp-state-default-file (expand-file-name "sessions" user-emacs-directory))
  (setq persp-mode-prefix-key (kbd "C-c w"))


  (defun amirreza/save-session ()
    (interactive)
    (persp-state-save persp-state-default-file))


  (defun amirreza/load-session ()
    (interactive)
    (persp-state-load persp-state-default-file))
  (persp-mode 1)
  :hook
  (kill-emacs . amirreza/save-session)
  :general
  (:prefix "C-c w" :keymaps 'override "s" 'persp-switch)
  (:prefix "SPC w" :states 'normal :keymaps 'override "s" 'persp-switch)

  )

(when (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'meta))

