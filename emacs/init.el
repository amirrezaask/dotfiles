(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar amirreza/config-file (expand-file-name "README.org" user-emacs-directory))

(defun amirreza/reload ()
  "Reload user configuration file"
  (interactive)
  (org-babel-load-file amirreza/config-file))


(org-babel-load-file amirreza/config-file)
