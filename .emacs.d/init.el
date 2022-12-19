;; Top level user configurations
(load-file (expand-file-name "user.el" user-emacs-directory))

;; lisp/*.el contains configurations for packages both emacs-core and third-party
;; amirreza-<package name>.el
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; site-lisp is like a lab for my experiments and maybe potential packages
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


(require 'amirreza-package-manager)
(require 'amirreza-buffer)
(require 'amirreza-company)
(require 'amirreza-cursor)
(require 'amirreza-dired)
(require 'amirreza-editor)
(require 'amirreza-eglot)
(require 'amirreza-emacs)
(require 'amirreza-font)
(require 'amirreza-git)
(require 'amirreza-help)
(require 'amirreza-languages)
(require 'amirreza-minibuffer)
(require 'amirreza-modeline)
(require 'amirreza-org)
(require 'amirreza-prescient)
(require 'amirreza-theme)
(require 'amirreza-treesitter)
(require 'amirreza-windows)
(require 'amirreza-workspaces)
