;; Debug Mode
(setq debug-on-error t)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install 'use-package)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
