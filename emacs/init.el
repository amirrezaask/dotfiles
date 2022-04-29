(defvar amirreza/config-file (expand-file-name "README.org" user-emacs-directory))

(defvar amirreza/packages-file (expand-file-name "packages.el" user-emacs-directory))

(defun amirreza/reload ()
  "Reload user configuration file"
  (interactive)
  (org-babel-load-file amirreza/config-file))

(defun amirreza/sync ()
  "Sync packages with packages in packages.el"
  (interactive)
  (load-file amirreza/packages-file))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize emacs package manager
(package-initialize)

;; helper to install missing packages
(defun amirreza/packages-install (packages)
    (mapc (lambda (pkg)
        (unless (package-installed-p pkg)
            (package-install pkg)))
    packages
    ))

(package-install 'use-package)

(amirreza/sync) ;; Sync packages

(amirreza/reload) ;; Load user configuration


