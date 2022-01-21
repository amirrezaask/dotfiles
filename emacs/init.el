;; Debug Mode
(setq debug-on-error t)

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

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
