;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Amirreza Askarpour"
      user-mail-address "raskarpour@gmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 21 :weight 'semi-light))

(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(defun amirreza/up-center ()
  (interactive)
  (forward-line -20)
  (recenter-top-bottom))

(defun amirreza/down-center ()
  (interactive)
  (forward-line 20)
  (recenter-top-bottom))

(setq evil-escape-unordered-key-sequence t)
;; Best movement ever ?????
(setq recenter-positions '(middle))

(map! :e "M-p" 'amirreza/up-center)
(map! :e "M-n" 'amirreza/down-center)

(map! :n "C-d" 'amirreza/down-center)
(map! :n "C-u" 'amirreza/up-center)

;; LSP keybindings
(map! :map lsp-mode-map :n "g r" 'xref-find-references)
(map! :map lsp-mode-map :n "g i" 'lsp-find-implementation)
(map! :map lsp-mode-map :n "g d" 'xref-find-definitions)

;; Magit
(map! :leader :desc "Magit status" :n "g s" #'magit)

;; My sexy keymap from my vim setup.
(map! :n "RET" #'evil-ex-nohighlight)

(map! :n "C-j" #'evil-window-down)
(map! :n "C-k" #'evil-window-up)
(map! :n "C-h" #'evil-window-left)
(map! :n "C-l" #'evil-window-right)

(setq
 evil-split-window-below t
 evil-vsplit-window-right t
 )

;; Workspaces
(map! :leader :n "w s" 'persp-switch)
