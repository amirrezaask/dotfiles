#!/usr/local/bin/emacs --script
(require 'org)

(delete-file "~/.emacs.d/README.el")
(delete-file "~/.emacs.d/d/README.elc")

(org-babel-load-file "~/.emacs.d/README.org")
(byte-compile-file "~/.emacs.d/README.el")
(byte-compile-file "~/.emacs.d/init.el")
