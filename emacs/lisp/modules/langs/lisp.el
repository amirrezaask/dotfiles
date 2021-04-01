(pkg! paredit :straight t
      :hook ((clojure-mode emacs-lisp-mode) . paredit-mode))
(pkg! parinfer :straight t  :hook ((clojure-mode emacs-lisp-mode) . parinfer-mode))

(pkg! lisp-mode :mode "\\.cl\\'")

 (pkg! sly :straight t :mode "\\.cl\\'")
