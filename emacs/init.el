;;; init.el --- init file for Emacs                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; (setq debug-on-error)

(setq user-full-name "AmirrezaAskarpour"
      user-mail-address "raskarpour@gmail.com")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronouslyr
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package desktop-environment
  :disabled t
  :straight t
  :config
  (desktop-environment-mode))

(use-package exwm 
  :disabled t
  :straight t
  :config 
  (require 'exwm-config)

  (exwm-config-default) ;; some basic default keybindings

  (setq exwm-workspace-number 4) ;; initial workspaces

  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  (setq exwm-manage-configurations
        '((string= exwm-instance-name "firefox") workspace 1
        (string= exwm-instance-name "rhythmbox") workspace 8)
        )
  )


(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package emacs 
  :config
  (setq use-dialog-box nil) ;; ask quesions in minibuffer
  (setq inhibit-splash-screen 0) ;; disable startup screen
  (setq ring-bell-function 'ignore) ;; don't make a sound
  ;; vertical scrolling
  (setq scroll-step 1)
  (setq scroll-margin 1)
  (setq scroll-conservatively 101)
  (setq scroll-up-aggressively 0.01)
  (setq scroll-down-aggressively 0.01)
  (setq auto-window-vscroll nil)
  (setq fast-but-imprecise-scrolling nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  ;; Horizontal Scroll
  (setq hscroll-step 1)
  (setq hscroll-margin 1)
  (setq backup-directory-alist
        '(("." . "~/.emacs.d/backup/")))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq create-lockfiles nil)
  (setq-default
   indent-tabs-mode nil
   tab-width 4)
  (setq echo-keystrokes 0.1)
  (setq-default fill-column 80)
  (setq-default cursor-type 'bar)
  (setq ring-bell-function t)
  (setq visible-bell t)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5))))

(use-package mule
  :config 
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-add-key-based-replacements "C-c m" "Major mode functions")
  (which-key-add-key-based-replacements "C-c w" "workspace functionality")
  (which-key-add-key-based-replacements "C-c o" "external tools integration")
  (which-key-add-key-based-replacements "C-c e" "Editor functions")
  (which-key-add-key-based-replacements "C-c d" "Devops related functions")
  (setq which-key-enable-extended-define-key t)
  (which-key-mode 1)
  (which-key-setup-minibuffer))

(use-package modus-operandi-theme :straight t :defer t)
(use-package modus-vivendi-theme :straight t :defer t)
(use-package spacemacs-theme :straight t :defer t)
(use-package doom-themes :straight t :defer t)

(use-package custom
  :demand
  :bind (("<f12>" . amirreza/toggle-color-mode))
  :config
  (defvar amirreza/current-mode 'dark "Current color of Emacs.")
  (defvar amirreza/dark-theme 'doom-outrun-electric)
  (defvar amirreza/light-theme 'doom-one-light)

  (defmacro amirreza/--load-theme (&rest theme-opts)
    `(progn (mapc #'disable-theme custom-enabled-themes)
            (load-theme ,@theme-opts)))

  (defun amirreza/load-theme (theme)
    (interactive "sEnter Theme: ")
    (amirreza/--load-theme (intern theme) t))
  (defun amirreza/apply-color (mode)
    "Apply current color mode to Emacs."
    (if (eq amirreza/current-mode 'dark)
        (amirreza/--load-theme amirreza/dark-theme t)
      (amirreza/--load-theme  amirreza/light-theme t)))

  (defun amirreza/toggle-color-mode ()
    "Toggle current mode to the opposite"
    (interactive)
    (if (eq amirreza/current-mode 'dark)
        (setq amirreza/current-mode 'light)
      (setq amirreza/current-mode 'dark))
    (amirreza/apply-color amirreza/current-mode))

  (amirreza/apply-color amirreza/current-mode))

(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(defvar amirreza/font "Jetbrains Mono-9")

(set-face-attribute 'default t :font amirreza/font)

(set-frame-font amirreza/font nil t)

(global-prettify-symbols-mode 1)

(defun amirreza/change-font (font size)
  (interactive "sFont: \nnSize: ")
  (set-face-attribute 'default t :font (format "%s-%d" font size))
  (set-frame-font (format "%s-%d" font size) nil t))

(use-package all-the-icons
  :straight t
  :commands (all-the-icons-octicon
         all-the-icons-faicon
         all-the-icons-fileicon
         all-the-icons-wicon
         all-the-icons-material
         all-the-icons-alltheicon))

(use-package all-the-icons-dired
  :straight t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-modeline :straight t
  :config
  (setq doom-modeline-height 35)
  (doom-modeline-mode 1))


;; show time and date in modeline
(use-package time
  :config
  (setq display-time-format "%H:%M  %Y-%m-%d")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  (display-time-mode))

;; show battery percentage in modeline
(use-package battery
  :config
  (setq battery-mode-line-format " Battery: %b%p%%")
  (setq battery-mode-line-limit 99)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  (display-battery-mode +1))

;; show position in file using a nyan-cat
(use-package nyan-mode :straight t :config (nyan-mode 1))

;; My custom modeline
(use-package emacs 
  :disabled t
  :config
  (setq amirreza/modeline-seperator "   ")
  (setq-default mode-line-format (list
                                  mode-line-front-space
                                  mode-line-misc-info ;; eyebrowse workspace number
                                  amirreza/modeline-seperator
                                  mode-line-modified
                                  amirreza/modeline-seperator
                                  "%m"
                                  amirreza/modeline-seperator
                                  "%b"
                                  amirreza/modeline-seperator
                                  mode-line-modes
                                  amirreza/modeline-seperator
                                  mode-line-position
                                  amirreza/modeline-seperator
                                  '(:eval vc-mode)
                                  mode-line-end-spaces
                                  )))

(use-package dashboard
  :straight t
  :config    
  (setq dashboard-banner-logo-title "Free as in freedom")
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 5)
                     (recents  . 5)))

  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

(use-package exec-path-from-shell
  :defer 2
  :straight t
  :config (exec-path-from-shell-initialize))

(setq display-buffer-alist
      '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-width . 0.40)
           (side . right)
           (slot . 0))
        ("^vterm"
          (display-buffer-in-side-window)
          (window-width . 0.40)
          (side . right)
          (slot . 0)
          )
        ("\\*rg"
          (display-buffer-in-side-window)
          (window-width . 0.40)
          (side . right)
          (slot . 0))))

(use-package eyebrowse :straight t 
             :config (eyebrowse-mode +1)
             :bind (("C-c w d" . eyebrowse-close-window-config)
                    ("C-c w c" . eyebrowse-create-window-config)
                    ("C-c w 0" . eyebrowse-switch-to-window-config-0)
                    ("C-c w 1" . eyebrowse-switch-to-window-config-1)
                    ("C-c w 2" . eyebrowse-switch-to-window-config-2)
                    ("C-c w 3" . eyebrowse-switch-to-window-config-3)
                    ("C-c w 4" . eyebrowse-switch-to-window-config-4)
                    ("C-c w 5" . eyebrowse-switch-to-window-config-5)
                    ("C-c w 6" . eyebrowse-switch-to-window-config-6)
                    ("C-c w 7" . eyebrowse-switch-to-window-config-7)
                    ("C-c w 8" . eyebrowse-switch-to-window-config-8)
                    ("C-c w 9" . eyebrowse-switch-to-window-config-9)))

(use-package winner
  :commands (winner-redo winner-undo))

(use-package ace-window
  :straight t
  :bind (("C-x o" . 'ace-window) ("C-x C-o" . 'ace-window)))

(use-package dired
  :config
  (add-hook 'dired-mode-hook (lambda () 
                               (dired-hide-details-mode 1)))
  :bind
  (:map dired-mode-map
        ("C-c m d" . dired-hide-details-mode)
        ("C-j" . next-line)
        ("C-k" . previous-line)))

(use-package dired-sidebar :straight t
  :bind
  (("<f8>" . dired-sidebar-toggle-sidebar)))

(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package peep-dired
  :straight t
  :after dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")))

(use-package gnus
  :config
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))

  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
   '((nnimap "Gmail"
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port "imaps")
             (nnimap-stream ssl)))))

(use-package counsel
  :straight t
  :demand
  :config
  (defun amirreza/rhythmbox-current-song-name () 
    (interactive)
    (message (counsel-rhythmbox-current-song)))

  (defun amirreza/rhythmbox-play/pause () 
    (interactive)
    (counsel-rhythmbox-playpause-current-song))

  :bind (:prefix "C-c o m"
                 :prefix-map music-player
                 :prefix-docstring "music player lanati"
                 ("l" . counsel-rhythmbox)
                 ("c" . amirreza/rhythmbox-current-song-name)
                 ("p" . amirreza/rhythmbox-play/pause)))

(use-package bongo 
  :disabled t
  :straight t
  :config
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-action-track-icon nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress nil)
  (setq bongo-mark-played-tracks nil)
  (setq bongo-header-line-mode nil)
  (setq bongo-header-line-function nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-default-directory "~/Music")
  (defun amirreza/dired-music-library-hook ()
    (when (string-match-p "Music" default-directory)
      (set (make-local-variable 'bongo-dired-library-mode) 't)))

  :hook
  (dired-mode . amirreza/dired-music-library-hook)
  :bind
  (:map bongo-dired-library-mode-map
        ("<C-return>" . bongo-insert-file)))

(use-package erc 
  :commands erc
  :init
  (setq erc-nick "amirrezaask")
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#emacs" "#5hit"))))

(use-package proced
  :commands proced
  :bind (("C-c o p" . proced)))

(use-package org
  :mode "\\.org\\'"
  :bind (:map org-mode-map
              ("C-c c b" . amirreza/--org-insert-elisp-code-block))
  :config
  (defun amirreza/--org-insert-elisp-code-block ()
    (interactive)
    (insert (format "#+begin_src emacs-lisp\n\n#+end_src"))
    (previous-line)
    (beginning-of-line))
  (setq org-ellipsis "⤵")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-support-shift-select t)
  (setq org-src-window-setup 'current-window)
  (setq org-agenda-files '("~/org/work.org" "~/org/personal.org")))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(use-package toc-org :straight t :hook (org-mode . toc-org-mode))

(use-package htmlize :straight t :defer t)


(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/custom.el"))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun amirreza/edit-configuration ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c e e") 'amirreza/edit-configuration)

(use-package vlf :straight t)

(use-package beacon
  :straight t
  :config (beacon-mode 1))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode +1))
(use-package simple
  :config
  (column-number-mode +1))

(use-package iedit
	     :straight t
	     :bind (("C-c e i" . 'iedit-mode)))

(use-package ibuffer
  :bind (("C-x C-b" . 'ibuffer)))

(use-package ibuffer-projectile 
  :straight t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package multiple-cursors
  :straight t
  :bind (("C->" . 'mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-c C-<" . 'mc/mark-all-like-this)
	 ("C-M-," . 'mc/edit-lines)))

(use-package dumb-jump
  :straight t
  :bind
  (("C-M-j" . 'dumb-jump-go)
   ("C-M-p" . 'dumb-jump-back))
  :config
  (dumb-jump-mode 1))

(use-package hl-todo
  :straight t
  :hook ((prog-mode) . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO"       warning bold)
	  ("FIXME"      error bold)
	  ("HACK"       font-lock-constant-face bold)
	  ("REVIEW"     font-lock-keyword-face bold)
	  ("NOTE"       success bold)
	  ("DEPRECATED" font-lock-doc-face bold))))

(use-package simple
  :config
  (setq kill-ring-max 15))

(use-package expand-region
  :straight t
  :bind (("C-=" . 'er/expand-region)
	 ("C--" . 'er/contract-region)))

(use-package sudo-edit
     :straight t
     :bind ("C-c e s e" . sudo-edit)
     :commands (sudo-edit))

(use-package highlight-indent-guides
  :straight t
  :hook ((yaml-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces))

(use-package dumb-jump
  :straight t
  :bind
  (("C-M-j" . 'dumb-jump-go)
   ("C-M-p" . 'dumb-jump-back))
  :config
  (setq dumb-jump-selector 'ivy)
  (dumb-jump-mode 1))

(use-package isearch
  :demand
  :config
  (setq isearch-highlight t)
  (setq isearch-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  :bind 
  (("C-s" . isearch-forward-regexp) ;; map default C-s to regex search
   ("C-r" . isearch-backward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-M-r" . isearch-backward)))

(use-package replace
  :demand t)


(use-package fzf
  :straight t
  :bind
  (("C-c e f f" . fzf-directory)))

(use-package rg
 :straight t
 :bind (("C-c e r g" . rg)))

(use-package rainbow-delimiters :straight t :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-blocks :straight t :defer t)

(use-package flx :straight t)
(use-package ivy
  :straight t
  :bind
  (("C-x b" . 'ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("RET" . 'ivy-alt-done))
  :config
  (setq ivy-height 15)
  ;; loopish cycling through list
  (setq ivy-wrap t)
  ;; don't show recents in minibuffer
  (setq ivy-use-virtual-buffers nil)
  ;; ...but if that ever changes, show their full path
  (setq ivy-virtual-abbreviate 'full)
  ;; don't quit minibuffer on delete-error
  (setq ivy-on-del-error-function #'ignore)

  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line)
  (ivy-mode +1))

(use-package all-the-icons-ivy :straight t :config (all-the-icons-ivy-setup))

(use-package swiper

  :straight t
  :commands (swiper)
  :init (global-set-key (kbd "C-s") 'swiper))

(use-package counsel

  :straight t
  :commands (counsel-M-x counsel-find-file ivy-switch-buffer)
  :config
  (setq ivy-re-builders-alist
   '((t . ivy--regex-fuzzy)))
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-h b" . 'counsel-descbinds)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("C-h a" . 'counsel-apropos)
   ( "M-y" . 'counsel-yank-pop)))

(use-package icomplete
  :disabled t
  :demand ;loading of icomplete is not deferred since we are using `:bind'.
  :config
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-show-matches-on-no-input t) ;; show completions from start of entering the minibuffer
  (setq icomplete-separator " | ") ;; seperator of candidates
  (setq icomplete-hide-common-prefix nil) ;;
  (setq icomplete-with-completion-tables t) ;; do completion on anything that has a completion table
  (setq icomplete-in-buffer nil) ; we dont want icomplete to work in buffers, we have company for that
  (defun amirreza/show-kill-ring ()
    (interactive)
    (insert (completing-read "Choose: " kill-ring )))

  (if (> emacs-major-version 27)
      (fido-mode +1)
    (icomplete-mode +1))

  :bind (("M-y" . amirreza/show-kill-ring)
         :map icomplete-minibuffer-map
         ("C-f" . icomplete-forward-completions)
         ("C-b" . icomplete-backward-completions)
         ("C-n" . icomplete-forward-completions)
         ("C-p" . icomplete-backward-completions)
         ("<right>" . icomplete-forward-completions)
         ("<left>" . icomplete-backward-completions)
         ("<up>" . icomplete-backward-completions)
         ("<RET>" . icomplete-force-complete-and-exit)
         ("<down>" . icomplete-forward-completions)))

(use-package icomplete-vertical
  :disabled t
  :straight t
  :demand
  :config
  (icomplete-vertical-set-separator "\n----------\n")
  (icomplete-vertical-mode 1)
  :bind
  (:map icomplete-minibuffer-map
        ("C-t" . icomplete-vertical-toggle)))

(use-package ido
  :disabled t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :disabled t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :disabled t
  :config
  (ido-ubiquitous-mode 1))

(use-package helm :straight t
  :disabled t
  :config
  (setq helm-mode-fuzzy-match t))

(use-package helm-descbinds :straight t
  :disabled t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-describe-modes :straight t
  :disabled t
  :bind (("C-h m" . helm-describe-modes)))

(use-package helm-make :straight t
  :disabled t
  :bind (("<f5> m" . helm-make)))

(use-package company
  :demand
  :straight t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-o" . company-other-backend)
              ("<tab>" . company-complete-common-or-cycle)
              ("RET" . company-complete-selection))
  :config
  (setq company-minimum-prefix-lenght 1)
  (setq company-tooltip-limit 30)
  (setq company-idle-delay 0.0)
  (setq company-echo-delay 0.1)
  (setq company-backends '(company-capf company-dabbrev company-files company-dabbrev-code))
  (defmacro amirreza/with-backends (mode backends) 
    "Register a buffer local variable with given BACKENDS for given MODE. For registering backends for various modes use this"
    (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
      (message "amirreza/with-backends called with %s %s %s" mode backends mode-hook)
      `(add-hook (quote ,mode-hook) (lambda ()
                                      (setq-local company-backends (quote ,backends))))))
  (global-company-mode t))

(use-package vterm
  :straight t
  :commands vterm
  :bind (("C-c o t" . vterm)))

(use-package lsp-mode 
    :straight t
    :commands (lsp lsp-deferred)
    :hook 
    ((python-mode
      go-mode) . lsp)
    :config
    (setq lsp-auto-guess-root t)
    :commands (lsp))


(use-package dap-mode :straight t)

(use-package helm-lsp :disabled t :straight t :commands helm-lsp-workspace-symbol)

(use-package lsp-ui :straight t :disabled t :commands lsp-ui-mode :hook (lsp-mode . lsp-ui-mode))

(use-package magit
  :straight t
  :commands (magit-status)
  :bind
  (("C-x g" . 'magit-status)))

(use-package diff-hl
  :straight t
  :config (global-diff-hl-mode 1))

(use-package
  gitconfig-mode
  :straight t
  :mode "/\\.gitconfig\\'")

(use-package gitignore-mode
  :straight t
  :mode "/\\.gitignore\\'")

(use-package gitattributes-mode
  :straight t
  :mode "/\\.gitattributes\\'")

(use-package git-messenger
  :straight t
  :bind
  (("C-c e g b" . git-messenger:popup-message))
  :config
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

(use-package eldoc
  :hook (prog-mode . eldoc-mode))

(use-package projectile
       :bind
       (("C-x p" . 'projectile-command-map)
        ("C-c p" . 'projectile-add-known-project))
       :config
       (setq projectile-completion-system 'ivy)
       (projectile-mode 1))

(use-package yasnippet :straight t 
  :config (yas-global-mode 1))

(use-package yasnippet-snippets :straight t)

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (defun amirreza/python-insert-docstring ()
    (interactive)
    (insert "'''\n'''")
    (previous-line))
  (amirreza/with-backends python-mode (company-capf))
  :bind
  (:map python-mode-map 
    ("C-c m p d" . amirreza/python-insert-docstring)))

(use-package lsp-python-ms :straight t)

(use-package pipenv
	     :straight t
	     :defer t)

(use-package py-autopep8
  :straight t
  :hook python-mode
  :config
  (py-autopep8-enable-on-save))

(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :init
  (defun amirreza/go-snippets ()
    (amirreza/defsnippet "fmain" "" "func main() {" \n "}")
    (amirreza/defsnippet "pkgm" "Package: " "package " str \n)
    (amirreza/defsnippet "pl" "" "fmt.Println(\"" _ "\")") ;; _ is the cursor position after the expansion
    (amirreza/defsnippet "pf" "" "fmt.Printf(\"" _ "\")")
    (amirreza/defsnippet "ifer" "" "if err != nil {" \n _ \n "}")
    (amirreza/defsnippet "if" "" "if " _ "{" \n "}"))

  (add-hook 'go-mode-hook (lambda () (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))))
  ;; (add-hook 'go-mode-hook 'amirreza/go-snippets) disabled due to switch to yasnippet
  :config
  (add-hook 'go-mode-hook (lambda () 
                            (interactive)
                            (setq-local prettify-symbols-alist '(("func" . 955)))))
  (add-hook 'go-mode-hook (lambda () (interactive)
                            (add-hook 'before-save-hook 'lsp-format-buffer t t)
                            (add-hook 'before-save-hook 'lsp-organize-imports t t))))

(use-package go-add-tags :straight t :defer t :bind (:map go-mode-map ("C-c m s t" . go-add-tags)))

(use-package gotest :defer t :straight t 
  :bind 
  (:map go-mode-map 
        ("C-c m t f" . go-test-current-file)
        ("C-c m t t" . go-test-current-test)))

(use-package scheme
  :config
  (setq scheme-program-name "guile"))

(use-package elisp-mode
  :config
  (setq-local prettify-symbols-alist '(("fn" . 955)))
  (defun --amirreza/emacs-lisp-repeat (str count)
    "Create dashes with given COUNT."
    (let ((dashes ""))
  (dotimes (iterator count dashes)
    (setq dashes (concat dashes str)))))

  (defun --amirreza/emacs-lisp-wrap-text-in-spaces (text)
    (let* ((len (length text))
       (spaces-length-side (/ (- 80 len) 2))
       (spaces-side (--amirreza/emacs-lisp-repeat " " spaces-length-side)))
  (format "%s%s%s" spaces-side text spaces-side)))

  (defun amirreza/emacs-lisp-insert-comment-line (text)
    "Insert a comment line with given TEXT."
    (interactive "sComment: ")
    (let* ((text-wrapped (--amirreza/emacs-lisp-wrap-text-in-spaces text))
       (dashes (--amirreza/emacs-lisp-repeat "=" 80)))
  (insert (format "\n;;%s\n;;%s\n;;%s" dashes text-wrapped dashes))))
  :bind
  (:map emacs-lisp-mode-map
    ("C-c m d" . 'amirreza/emacs-lisp-insert-comment-line)))

(use-package clojure-mode :straight t
  :config
  (setq-local prettify-symbols-alist '(("fn" . 955) ; λ
                                        ("->" . 8594))))

(use-package cider 
  :straight t
  :commands (cider cider-jack-in cider-jack-in-cljs))

(use-package lisp-mode :mode "\\.cl\\'")

(use-package sly :straight t)

(use-package haskell-mode :straight t :mode "\\.hs\\'")

(use-package lsp-haskell :straight t :hook haskell-mode)

(use-package web-mode :straight t :mode ("\\.html\\'" "\\.css\\'"))

(use-package php-mode :straight t :mode "\\.php\\'")

(use-package php-runtime :straight t :defer t)

(use-package composer :straight t :hook php-mode)

(use-package phpunit :straight t
  :commands (php-current-test php-current-class php-current-project)
  :bind (:map php-mode-map 
              ("C-c m t t" . php-current-test)
              ("C-c m t c" . php-current-class)
              ("C-c m t p" . php-current-project)))

(use-package rust-mode :straight t :mode "\\.rs\\'")

(use-package crontab-mode :defer t :straight t)

(use-package apache-mode :straight t
  :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'" "access\\.conf\\'"))

(use-package systemd :straight t
  :mode ("\\.service\\'" "\\.timer\\'"))

(use-package nginx-mode :straight 
  :mode ("/etc/nginx/conf.d/.*" "/etc/nginx/.*\\.conf\\'"))

(use-package tramp
      :commands (tramp)
      :bind
      (("C-c d t" . tramp))
      :config
      (setq tramp-default-method "ssh"))

(use-package docker-compose-mode
  :straight t
  :mode "docker-compose\\.yml")

(use-package docker :straight t 
  :bind
  ("C-c d d" . docker))

(use-package kubel :straight t :commands (kubel) :bind (("C-c d k" . kubel)))

(use-package redis :straight t)

(use-package pacmacs :straight t :defer t)


(defvar amirreza/emacs-init-time-elapsed (- (float-time) amirreza/emacs-init-timestamp))
