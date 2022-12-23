;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Amirreza Askarpour"
      user-mail-address "raskarpour@gmail.com")

;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 21 :weight 'semi-light))

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
(evil-define-key 'normal lsp-mode-map (kbd "gr") 'xref-find-references)
(evil-define-key 'normal lsp-mode-map (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal lsp-mode-map (kbd "gd") 'xref-find-definitions)

;; Magit
(map! :mode 'normal :leader :desc "Magit status" "g s" #'magit)

;; My sexy keymap from my vim setup.
(map! :mode 'normal "RET" #'evil-ex-nohighlight)
