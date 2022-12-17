;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Amirreza Askarpour"
      user-mail-address "raskarpour@gmail.com")

;; (setq doom-font (font-spec :family "Fira Code" :size 20 :weight 'semi-light)
     ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 20 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq evil-escape-unordered-key-sequence t)

(evil-global-set-key 'normal (kbd "<C-d>") (lambda
					     ()
					     (interactive)
					     (evil-scroll-down)
					     (evil-scroll-line-to-center)
					     ))

(evil-global-set-key 'normal (kbd "<C-u>") (lambda
                                            ()
                                            (interactive)
                                            (evil-scroll-up)
                                            (evil-scroll-line-to-center)
                                            ))


(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")
