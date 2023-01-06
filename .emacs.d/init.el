(setq debug-on-init t)
(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")
;; (setq amirreza/font "Source Code Pro")
(setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "OperatorMono Nerd Font Light")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")
;; (setq amirreza/font "Iosevka")
(setq amirreza/font-size "20")
(setq amirreza/theme 'doom-dracula)

;; If early-init wasn't there.
(setq package-enable-at-startup nil) ;; Disable default package manager package.el
(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

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

(use-package benchmark-init
  :init
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

(use-package hydra
  :init
  (defmacro amirreza/defhydra (name body heads)
    `(eval (append '(defhydra ,name ,body) ,heads))))

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package emacs :straight nil
  :init
  (setq create-lockfiles nil) ;; Don't create .# files as lock.
  (setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation
  (setq make-backup-files nil) ;; Disable backup files ~file
  (setq auto-save-default nil) ;; Disable auto save files
  (setq inhibit-startup-screen t) ;; No startup splash screen
  (setq use-dialog-box nil) ;; Do not use UI for questions
  (setq ring-bell-function 'ignore) ;; Do not beep please.
  (tool-bar-mode 0) ;; disable top toolbar
  (scroll-bar-mode 0) ;; disable scroll bar
  (menu-bar-mode -1)) ;; Disable menu bar

(use-package exec-path-from-shell ;; Copy PATH from default shell
  :init
  (exec-path-from-shell-initialize))

(use-package tramp
  :straight nil
  :commands
  (tramp))

(use-package custom
  :straight nil
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package bufler
  :bind
  ("C-x C-b" . bufler))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.4)

(defun amirreza/find-file ()
  "Smart find file function to do project-files if in Git repo otherwise use default find-file."
  (interactive)
  (if (vc-backend (buffer-file-name))
      (project-find-file)
    (call-interactively 'find-file)))

(defun amirreza/edit-emacs ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "C-c e e") 'amirreza/edit-emacs)

(defun amirreza/getenv (name default)
  "Get env if not defined use default"
  (let ((value (getenv name)))
    (if value
	value
      default)))

(use-package dired
  :straight nil
  :bind
  (:map dired-mode-map
	("C-c C-e" . wdired-change-to-wdired-mode)))

;; Improve help buffers in emacs.
(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable))

;; Add custom themes path to themes load path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))


(use-package ef-themes)
(use-package doom-themes)
(use-package gruber-darker-theme)

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
(global-set-key (kbd "C-c t t") 'amirreza/switch-theme)

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

(setq-default cursor-type 'box)
(global-hl-line-mode)
(blink-cursor-mode -1)

;; Autocompletion configs
(use-package corfu
  :straight
  (corfu :type git :host github :repo "emacs-straight/corfu" :files ("*" "extensions/*.el" (:exclude ".git")))
  :init
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (global-corfu-mode 1)
  (corfu-echo-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package corfu-terminal
  :init
  (corfu-terminal-mode))

(use-package corfu-prescient
  :init
  (corfu-prescient-mode))

;; Vertico minibuffer completion
(use-package vertico
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq vertico-count 15)
  (setq vertico-cycle t)
  (vertico-mode))

(use-package consult
  :init
  (setq consult-async-min-input 1))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))

(use-package delsel  ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
  :straight nil
  :init
  (delete-selection-mode 1))

(use-package paren 
  :straight nil
  :init
  (setq show-paren-delay 0)  ;; highlight matching parens instantly.
  (show-paren-mode 1)) ;; Highlight matching parens

(use-package display-line-numbers
  :straight nil
  :init
  (setq display-line-numbers-type 'relative) ;; relative line numbers
  (global-display-line-numbers-mode 1)) ;; enable line numbers globaly

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

(global-set-key (kbd "M-p") 'amirreza/up-center)
(global-set-key (kbd "M-n") 'amirreza/down-center)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package simple
  :straight nil
  :bind
  ("C-q" . set-mark-command)
  ("C-<return>" . set-mark-command))

;; Org mode stuff

(use-package org
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
:END:"))

  (defhydra amirreza/org-mode-hydra (:exit t)
    ("l" org-toggle-link-display "Toggle Link Display")
    ("b" amirreza/org-code-block "Insert a Code Block")
    ("n" amirreza/org-disable-tangle "Disable Tangle PROPERTIES")
    ("e" org-export-dispatch "Export")
    ("o" org-open-at-point "Open At Point")
    ("h" (lambda () (interactive) (org-export-as 'html)) "Org Export To HTML")
    ("t"  org-todo "Open At Point"))


  :bind
  (:map org-mode-map
	("C-c m" . 'amirreza/org-mode-hydra/body)
	:map org-src-mode-map
	("C-c C-c" . 'org-edit-src-exit)))


(use-package ox-reveal :after org-mode)
(use-package ob-go :after org-mode)
(use-package ob-rust :after org-mdoe)
(use-package ob-php :after org-mode)
(use-package htmlize :after org-mode)

(use-package git-gutter
  :init
  (global-git-gutter-mode))

(use-package magit
  :bind
  ("C-x g" . magit))


(use-package project
  :straight nil
  :init
  (defhydra amirreza/project-hydra (:exit t)
    ("f" project-find-file "Find File")
    ("p" project-switch-project "Switch To Project")
    ("b" project-buffers "Find Buffer In Project")
    ("c" project-compile "Compile Project"))
  :bind
  ("C-x p" . amirreza/project-hydra/body))

(setq amirreza/programming-hydra-heads '())

(use-package wgrep)
(use-package ripgrep)

(use-package flymake
  :straight nil
  :init
  (add-to-list 'amirreza/programming-hydra-heads '("n" flymake-goto-next-error "Goto Next Error"))
  (add-to-list 'amirreza/programming-hydra-heads '("p" flymake-goto-previous-error "Goto Previous Error"))
  (add-to-list 'amirreza/programming-hydra-heads '("e" consult-flymake "List of errors")))

(use-package xref
  :straight nil
  :bind
  (("M-." . xref-find-definitions)
   ("M-," . xref-go-back)
   ("M-r" . xref-find-references)))

(use-package eldoc
  :straight nil
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer nil)
  (add-to-list 'amirreza/programming-hydra-heads '("." amirreza/eldoc-toggle-buffer "Toggle Eldoc for point"))
  (global-eldoc-mode)
  :bind
  (("C-h ." . eldoc)
   ("M-0" . eldoc)))


(use-package eglot
  :hook
  (prog-mode-hook . eglot-ensure)
  :init
  (add-to-list 'amirreza/programming-hydra-heads '("d" eldoc "Document THING at POINT"))
  (add-to-list 'amirreza/programming-hydra-heads '("D" xref-find-definitions "Goto Definitions"))
  (add-to-list 'amirreza/programming-hydra-heads '("r" xref-find-references "Find References"))
  (add-to-list 'amirreza/programming-hydra-heads '("i" eglot-find-implementation "Find Implementations"))
  (add-to-list 'amirreza/programming-hydra-heads '("s" consult-eglot-symbols "Workspace Symbols"))
  (add-to-list 'amirreza/programming-hydra-heads '("R" eglot-rename "Rename"))
  (add-to-list 'amirreza/programming-hydra-heads '("f" eglot-format "Format"))

  )

(use-package prog-mode
  :straight nil
  :init
  (amirreza/defhydra amirreza/programming-hydra (:exit t)
		     amirreza/programming-hydra-heads)
  :bind
  (("C-c m" . amirreza/project-hydra/body)))


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
  :config
  (persp-mode 1)
  :bind
  (("C-x w s" . persp-switch)
   ("C-c w s" . persp-switch)))

(use-package vterm :commands (vterm))

(when (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'meta))

(use-package apache-mode) ;; Apache config syntax
(use-package cmake-mode) ;; CMake
(use-package systemd) ;; Systemd config syntax
(use-package nginx-mode) ;; Nginx config syntax
(use-package docker-compose-mode) ;; Docker-compose syntax
(use-package dockerfile-mode) ;; Dockerfile syntax
(use-package markdown-mode) ;; Markdown syntax
(use-package yaml-mode) ;; Yaml
(use-package fish-mode) ;; Fish
(use-package nix-mode) ;; Nix

(use-package json-snatcher :mode "\\.json\\'") ;; Show path of json value at POINT

(use-package csv-mode :mode "\\.csv\\'") ;; CSV

(use-package js2-mode :mode "\\.js\\'") ;; Javascript
(use-package rjsx-mode :mode "\\.jsx\\'") ;; React JSX syntax
(use-package typescript-mode :mode "\\.ts\\'") ;; Typescript syntax
(use-package nodejs-repl :after js2-mode) ;; Nodejs repl
(use-package tide :after typescript-mode) ;; Typescript IDE

(use-package php-mode :mode "\\.php\\'") ;; PHP
(use-package psysh :after php-mode) ;; PHP repl in Emacs
(use-package composer :after php-mode) ;; Composer

(use-package pip-requirements :after python-mode) ;; requirements.txt
(use-package pipenv :after python-mode) ;; pipenv
(use-package pyimport :after python-mode)
(use-package python-isort :straight (python-isort :type git :host github :repo "wyuenho/emacs-python-isort") :after python-mode)

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (amirreza/defhydra amirreza/go-hydra
		     (:exit t)
		     (append amirreza/programming-hydra-heads '(("a" go-tag-add "Add Struct Tag"))))
  :bind
  (:map go-mode-map
	("C-c m" . amirreza/go-hydra/body)))

(use-package go-tag :after go-mode) ;; Struct tags in Golang
(use-package go-gen-test :after go-mode) ;; Generate test for function
(use-package clojure-mode :mode "\\.clj\\'") ;; LISP on JVM
(use-package cider :after clojure-mode) ;; Clojure repl integration
(use-package lua-mode :mode "\\.lua\\'") ;; Lua
(use-package zig-mode :mode "\\.zig\\'") ;; Zig

(use-package rustic
  :mode "\\.rs\\'"
  :init
  (setq rustic-lsp-client 'eglot)) ;; Rustic default is lsp-mode


(use-package json-mode
  :mode "\\.json\\'"
  :init
  (setq amirreza/json-hydra-heads '(("f" json-pretty-print "Pretty print region")
				    ("F" json-pretty-print-buffer "Pretty print buffer")))

  (amirreza/defhydra amirreza/json-hydra (:exit t) amirreza/json-hydra-heads)
  :bind
  (:map json-mode-map
	("C-c m" . amirreza/json-hydra/body)))
