;; load corelib library
(load-file "~/.emacs.d/corelib/corelib.el")

;; Optimize Emacs internals, tangle literate configuration if needed.
(corelib/faster-start)

;; Initialize Package manager
(corelib/init-package-manager)

;; Init file is generated gets loaded.
