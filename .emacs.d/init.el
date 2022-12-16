(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package-manager)
(require 'ui)
(require 'basics)

(require 'minibuffer-vertico)
;; (require 'minibuffer-ivy)

(require 'completion-company)
;; (require 'completion-corfu)

(require 'project-manager)

(require 'file-manager)

(require 'navigation)

(require 'git)

(require 'misc)

(require 'langs/go)

(require 'langs/rust)

;; (require 'lsp-eglot)

(require 'lsp-lspmode)
