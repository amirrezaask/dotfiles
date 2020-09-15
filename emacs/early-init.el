;; load corelib library
(add-to-list 'load-path "~/.emacs.d/corelib")
(require 'corelib)

;; Optimize Emacs internals, tangle literate configuration if needed.
(corelib/faster-start)

;; Initialize Package manager
(corelib/init-package-manager)

;; Use literate configuration mode
(corelib/use-literate-config "~/.emacs.d/README.org" "~/.emacs.d/init.el")

;; Init file is generated gets loaded.
