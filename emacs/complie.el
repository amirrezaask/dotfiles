#!/usr/local/bin/emacs --script
(require 'org)

(org-babel-load-file "~/.emacs.d/README.org")
(byte-compile-file "~/.emacs.d/README.el")
(byte-compile-file "~/.emacs.d/init.el")
