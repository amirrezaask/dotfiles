;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Amirreza Askarpour"
      user-mail-address "raskarpour@gmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 21 :weight 'semi-light))

(setq doom-theme 'doom-one)
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

;; Best movement ever ?????
(setq recenter-positions '(middle))

(global-set-key (kbd "M-p") (lambda () (interactive) (amirreza/up-center)))
(global-set-key (kbd "M-n") (lambda () (interactive) (amirreza/down-center)))

(with-eval-after-load 'evil
  (evil-define-key 'normal global-map (kbd "C-d") 'amirreza/down-center)
  (evil-define-key 'normal global-map (kbd "C-u") 'amirreza/up-center))

(setq evil-escape-unordered-key-sequence t)

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
