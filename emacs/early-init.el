;; load corelib library
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'core/core)

;; Optimize Emacs internals, tangle literate configuration if needed.
(core/faster-start)

