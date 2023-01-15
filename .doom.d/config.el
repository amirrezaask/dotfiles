;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Amirreza Ask"
      user-mail-address "raskarpour@gmail.com")

(setq doom-font (font-spec :family "Fira Code" :size 20))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")

(defun amirreza/up-center ()
  "Move half a page up and also center the cursor."
  (interactive)
  (forward-line (- (/ (window-height) 2)))
  (recenter-top-bottom))

(defun amirreza/down-center ()
  "Move half a page down and also center the cursor."
  (interactive)
  (forward-line (/ (window-height) 2))
  (recenter-top-bottom))

(defun amirreza/transparent (percent)
  (interactive "nPercent: ")
  (set-frame-parameter (selected-frame) 'alpha `(,percent ,percent)))

(setq recenter-positions '(middle))

(global-set-key (kbd "M-p") 'amirreza/up-center)
(global-set-key (kbd "M-n") 'amirreza/down-center)

(setq evil-escape-unordered-key-sequence t)

(map! :n "C-l" 'evil-window-right)
(map! :n "C-h" 'evil-window-left)
(map! :n "C-k" 'evil-window-up)
(map! :n "C-j" 'evil-window-down)
(map! :n "C-u" 'amirreza/up-center)
(map! :n "C-d" 'amirreza/down-center)
