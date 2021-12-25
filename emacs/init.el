;; Debug Mode
;; (setq debug-on-error t)

(defun amirreza-package-manager-init ()
  "Initialize Straight.el package manager."
  ;; Initialize Package manager
  (defvar bootstrap-version)
  (setq straight-base-dir "~/.local/emacs/")
  (let ((bootstrap-file
         (expand-file-name "~/.local/emacs/straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))

(amirreza-package-manager-init)

(use-package doom-themes :straight t)

(load-theme 'doom-dracula t)
(use-package which-key
  :straight t

  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1)
  (which-key-setup-minibuffer))


(defun amirreza/change-font (font)
  (setq default-frame-alist `((font . ,font))))

(defvar amirreza/font "GoRegular-11")
(amirreza/change-font amirreza/font)

(define-key global-map (kbd "C--") (lambda () (interactive) (text-scale-adjust -1)))
(define-key global-map (kbd "C-=") (lambda () (interactive) (text-scale-adjust +1)))





