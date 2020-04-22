;; Configurations for when I launch emacs from terminal
;; I use alias e='emacs -nw -l <thisfile>'

(menu-bar-mode -1)
(ido-mode 1)
(ido-everywhere 1)
(setq vc-follow-symlinks t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)
