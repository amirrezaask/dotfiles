;; load corelib library
(add-to-list 'load-path "~/.emacs.d/core")
(require 'core)

;; Optimize Emacs internals, tangle literate configuration if needed.
(core/faster-start)

