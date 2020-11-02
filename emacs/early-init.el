;; load corelib library
(add-to-list 'load-path "~/.emacs.d/corelib")
(require 'corelib)

;; Optimize Emacs internals, tangle literate configuration if needed.
(corelib/faster-start)

;; Initialize Package manager
(corelib/init-package-manager)
(delete-file "~/.emacs.d/README.el")
