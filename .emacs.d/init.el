(setq debug-on-error t)
;; Top level user configurations
(load-file (expand-file-name "user.el" user-emacs-directory))

;; lisp/*.el contains configurations for packages both emacs-core and third-party
;; amirreza-<package name>.el
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; site-lisp is like a lab for my experiments and maybe potential packages
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Package manager
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(defmacro elpa-package
    (package &rest body)
  "Check if PACKAGE is installed and evaluate BODY."
  `(progn
     (straight-use-package (quote,package))
     (progn ,@body)
     ))

(defmacro emacs-package
    (package &rest body)
  `(progn
     ,@body))

(require 'amirreza-emacs)
(require 'amirreza-buffer)
(require 'amirreza-company)
(require 'amirreza-cursor)
(require 'amirreza-dired)
(require 'amirreza-editor)
(require 'amirreza-eglot)
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
