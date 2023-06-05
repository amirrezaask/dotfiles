s;; (setq debug-on-error t)
;; ==========================================
;; Improve startup time
;; ==========================================
(tool-bar-mode -1) ;; disable top toolbar
(scroll-bar-mode -1) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 300 1024 1024)) ;; Increase Emacs garbage collector threshold to 300 MB
(setq read-process-output-max (* 1024 1024)) ;; Increase Emacs read from process output to 1 MB
;; ==========================================
;; Package manager and installing packages
;; ==========================================
(setq package-enable-at-startup nil)
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
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ==========================================
;; Configurations
;; ==========================================
(use-package gcmh  ;; Better garbage collector behaviour
  :init
  (gcmh-mode 1))

(use-package exec-path-from-shell  ;; When launched in graphical user interface mode use exec-path to copy PATH
  :if (and (display-graphic-p) (not (eq system-type 'windows-nt)))
  :hook (after-init . exec-path-from-shell-initialize))

;; Font settings
(use-package emacs :straight nil
  :init
  (setq amirreza/font-size 16)
  (setq amirreza/font "FiraCode Nerd Font Mono")

  (defun amirreza/increase-font-size ()
    (interactive)
    (setq amirreza/font-size (+ amirreza/font-size 1))
    (set-frame-font (format "%s-%d" amirreza/font amirreza/font-size) nil t))

  (defun amirreza/decrease-font-size ()
    (interactive)
    (setq amirreza/font-size (- amirreza/font-size 1))
    (set-frame-font (format "%s-%d" amirreza/font amirreza/font-size) nil t))

  (global-set-key (kbd "M-=") 'amirreza/increase-font-size) ;; Increase font size
  (global-set-key (kbd "M--") 'amirreza/decrease-font-size) ;; Decrease font size

  (set-frame-font (format "%s-%d" amirreza/font amirreza/font-size) nil t) ;; Setting the Font and size
  )

;; Emacs Improvements and configs
(use-package emacs :straight nil ;; Emacs internal modes configuration and tweaks
  :bind
  (
   ("M-p" . (lambda () (interactive) (previous-line (/ (window-height) 2)) (recenter-top-bottom)))
   ("M-n" . (lambda () (interactive) (next-line (/ (window-height) 2)) (recenter-top-bottom)))
   ("M-0" . 'delete-window) ;; Delete current window
   ("M-2" . 'split-window-below) ;; Split window horizontaly
   ("M-3" . 'split-window-right) ;; Split window verticaly
   ("C-q" . 'set-mark-command) ;; start selecting a region
   ("C-;" . 'goto-line) ;; Goto line
   ("C-<tab>" .  'indent-region) ;; indent region
   ("M-[" . (lambda () (interactive) (shrink-window-horizontally 5)))
   ("M-]" . (lambda () (interactive) (enlarge-window-horizontally 5)))
   )
  :config
  (setq user-full-name "Amirreza Askarpour")
  (setq user-email "raskarpour@gmail.com")
  (global-unset-key (kbd "C-z")) ;; No minimizing
  (delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
  (setq create-lockfiles nil) ;; Don't create .# files as lock.
  (setq make-backup-files nil) ;; Disable backup files ~file
  (setq auto-save-default nil) ;; Disable auto save files
  (setq vc-follow-symlinks t) ;; Follow link files
  (setq inhibit-startup-screen t) ;; No startup splash screen
  (setq use-dialog-box nil) ;; Do not use UI for questions
  (setq scroll-step 2) ;; Smoooooth scrolling
  (setq undo-limit 20000000)
  (setq undo-strong-limit 40000000)
  (global-hl-line-mode -1) ;; highlight my current line
  (mouse-avoidance-mode 'jump) ;; 
  (setq ring-bell-function 'ignore) ;; Do not beep please.
  (setq show-paren-delay 0) ;; highlight matching parens instantly.
  (show-paren-mode 1) ;; Highlight matching parens
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1) ;; line numbers
  (setq recenter-positions '(middle)) ;; when I do <C-l> always center view.
  (setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; don't touch my config for custom variables put them in another file.
  (defalias 'yes-or-no-p 'y-or-n-p) ;; Instead of yes or no use y or n
  (setq echo-keystrokes 0.4) ;; faster echoing of keystrokes in minibuffer.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default cursor-type 'box) ;; set cursor type to be box, I like box better that a single horizontal line, it's more observable.
  (toggle-frame-maximized) ;; Always start in maximized mode
  (blink-cursor-mode -1) ;; Disable cursor blinking
  )

(use-package hydra)

;; Themes
(use-package doom-themes)
(use-package modus-themes)
(use-package ef-themes)
(use-package sweet-theme)
(use-package gruber-darker-theme)
(use-package amirreza-themes :straight (:type git :host github :repo "amirrezaask/themes" :local-repo "amirreza-themes" ) :defer t)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(load-theme 'gruber-darker t)

(use-package expand-region ;; Expand/contract your selection based on language semantics.
  :bind
  (("C-=" . 'er/expand-region) ;; Expand your current selection based on the semantics of syntax.
  ("C--" . 'er/contract-region)) ;; Contract your current selection based on the semantics of syntax.
  )

(use-package multiple-cursors ;; TODO: maybe find better keybindings for this
  :bind
  (("C-S-n" . mc/mark-next-like-this) ;; insert another cursor in next line
  ("C-S-p" . mc/mark-previous-like-this) ;; insert cursor in previous line
  ("C-M-n" . mc/unmark-next-like-this) ;; insert another cursor in next line
  ("C-M-p" . mc/unmark-previous-like-this) ;; insert cursor in previous line
  )
)

(use-package ace-window ;; better window management for Emacs
  :bind
  (("C-x C-o" . ace-window)))

(use-package wgrep)

(use-package magit ;; Best git client ever
  :bind
  (("C-x g" . magit)))

(use-package minions ;; Remove minor modes from modeline.
  :init
  (minions-mode 1))


(use-package vertico ;; Minibuffer completion enhancement
  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq vertico-count 15)
  (setq vertico-cycle t)
  (vertico-mode))

(use-package consult :init ;; Useful minibuffer functions
  (setq consult-async-min-input 1)
  :bind
  (("M-y" . consult-yank-pop) ;; Emacs clipboard manager
   ("C-s" . consult-line)
   ("C-S-s" . consult-ripgrep)
   ))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (:map minibuffer-local-map
  ("C-." . embark-act)         ;; pick some comfortable binding
  ("C-S-e" . embark-export)      ;; Export current items into a buffer, amazing feature.
  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless ;; better matching algorithm
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Autocompletion
(use-package corfu-terminal)
(use-package corfu-prescient)
(use-package corfu
  :after (corfu-terminal corfu-prescient)
  :demand t
  :bind
  (("C-SPC" . completion-at-point)
   ("C-@" . completion-at-point))
  :config
  (setq corfu-auto t) ;; Corfu should start only when I want it to.
  (global-corfu-mode) ;; Globally enable auto complete.
  (unless (display-graphic-p) ;; If in terminal emacs do some compat stuff.
    (corfu-terminal-mode))
  )

;; Language modes
(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  :init
  (setq gofmt-command "goimports"))

(use-package go-tag)
(when (< emacs-major-version 29) (use-package csharp-mode)) ;; Csharp mode is included from Emacs 29
(use-package rust-mode)
(use-package lua-mode)
(use-package zig-mode)
(use-package php-mode)
(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package csv-mode)
(use-package tuareg) ;; Ocaml
(use-package utop
  :init
  (setq utop-command "opam exec -- dune utop . -- -emacs")
  )



;; Org mode
(use-package org :straight nil
  :bind
  (:map org-src-map ("C-c C-c" . org-edit-src-exit))
  :init
  (setq org-use-property-inheritance t  ;; Inherit properties of the parent node in Org Emode.
        org-startup-folded t  ;; Start org mode all headers collapsed.
        org-src-window-setup 'current-window ;; use current window for org src edits.
        org-src-tab-acts-natively nil))

;; Eldoc
(use-package eldoc :straight nil
  :bind
  ("C-'" . eldoc)
  :init
  (setq eldoc-echo-area-use-multiline-p nil) ;; Don't do multiline document in minibuffer it's distracting
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer nil)
  (setq eldoc-idle-delay 0.5))


;; Xref
(use-package xref :straight nil
  :bind
  (("M-," . xref-pop-marker-stack) ;; Jump back
   ("M-r" . xref-find-references) ;; Find references
   ))

;; Eglot
(use-package eglot
  :hook (((go-mode rust-mode tuareg-mode) . eglot-ensure) (eglot-managed-mode . (lambda () ;; This will just disable fucking mouse when eglot is enabled.
                                                                      (put 'eglot-note 'flymake-overlay-control nil)
                                                                      (put 'eglot-warning 'flymake-overlay-control nil)
                                                                      (put 'eglot-error 'flymake-overlay-control nil))))
  :bind
  (:map eglot-mode-map
        ("M-S-r" . eglot-rename) ;; rename LSP
        ("M-i" . eglot-find-implementation) ;; find impl LSP
        ("M-c" . eglot-code-actions)) ;; code actions LSP

        
  :config
  (setq eglot-events-buffer-size 0) ;; Make eglot faster by not logging events
  (setq eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)) ;; Disable hover + document highlights
  (setq eglot-autoshutdown t) ;; Automatic shutdown unused servers.
  )

;; Flymake
(use-package flymake :straight nil ;; Diagnostic system of emacs showing errors and warnings
  :bind
  (("M-j" . flymake-goto-next-error) ("M-k" . flymake-goto-prev-error))
  :config
  (setq flymake-no-changes-timeout 0.5))


;; Butler
(use-package bufler
  :bind
  (("C-x C-b" . bufler)))


;; MacOS stuff
(when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta) ;; make command key in mac act as meta/alt
    (setq mac-option-modifier 'meta))

(use-package dired :straight nil
  :bind
  (:map dired-mode-map
        ("C-c C-e" . wdired-change-to-wdired-mode)))


(use-package rg)
(use-package perspective
  :init
  (setq persp-mode-prefix-key (kbd "C-x w"))
  (persp-mode)
  :bind
  (("C-x w s" . persp-switch)))

(use-package project :straight nil
  :config
  (defun amirreza/find-file ()
    "Use git based finding if inside a git repository"
    (interactive)
    (cond
     ((project-current) (call-interactively 'project-find-file))
     (t (call-interactively 'find-file))))

  (global-set-key (kbd "M-o") 'amirreza/find-file)

  (defun amirreza/compile ()
    (interactive)
    (cond
     ((project-current) (call-interactively 'project-compile))
     (t (call-interactively 'compile))))
  
  :bind
  (("C-c C-c" . 'amirreza/compile) ;; Compile command
   ("M-o" . 'amirreza/find-file)))


