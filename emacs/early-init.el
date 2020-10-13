;; load corelib library
(add-to-list 'load-path "~/.emacs.d/corelib")
(require 'corelib)

;; Optimize Emacs internals, tangle literate configuration if needed.
(corelib/faster-start)

;; Initialize Package manager
(corelib/init-package-manager)
(delete-file "~/.emacs.d/README.el")
(org-babel-load-file "~/.emacs.d/README.org")

;; Init file is generated gets loaded.
