(defvar amirreza/config-file (expand-file-name "README.org" user-emacs-directory))

(defun amirreza/reload ()
  "Reload user configuration file"
  (interactive)
  (org-babel-load-file amirreza/config-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Initialize emacs package manager
(package-initialize)

(package-install 'use-package)

(amirreza/reload) ;; Load user configuration


