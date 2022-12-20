;;; amirreza-emacs.el --- My configurations for Emacs internals  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Amirreza Askarpour

;; Author: Amirreza Askarpour <amirreza@amirrezas-MacBook-Pro.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:

;; Emacs has an internal garbage collector with a similar role of other programming languages garbage collector. It also acts similar,
;; it would pause execution, basically stopping emacs for short time, sweep all memory owned by emacs process and find elisp objects that are not used anymore and free the memory owned by these objects.
;; This process of pausing emacs and sweeping happens when memory used by emacs has reached to a threshold, so when that threshold is too low, these pauses happen more frequently which means that more pauses
;; and less responsive experience, so what we are going to do at first is to increase this GC threshold to make pauses happen less frequently.
(setq gc-cons-threshold (* 100 1024 1024))

;; Many packages like LSP clients will try and spawn a subprocess to do some work, Emacs should read from those processes output and process the result, increasing maximum read means less sys calls to read from, subprocess output and basically more speed.
(setq read-process-output-max (* 1024 1024))

(setq create-lockfiles nil) ;; Don't create .# files as lock.

(setq make-backup-files nil) ;; Disable backup files ~file
(setq auto-save-default nil) ;; Disable auto save files
(setq inhibit-startup-screen t) ;; No startup splash screen
(setq use-dialog-box nil) ;; Do not use UI for questions

(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar

(defun amirreza/up-center ()
  (interactive)
  (previous-line 20)
  (recenter-top-bottom))

(defun amirreza/down-center ()
  (interactive)
  (next-line 20)
  (recenter-top-bottom))

;; Best movement ever ?????
(setq recenter-positions '(middle))
(if-not-evil 
    (global-set-key (kbd "M-p") (lambda () (interactive) (amirreza/up-center)))
    (global-set-key (kbd "M-n") (lambda () (interactive) (amirreza/down-center))))

;; Copy PATH from default shell
(elpa-package exec-path-from-shell
	      (exec-path-from-shell-initialize))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun amirreza/find-file ()
  (interactive)
  (if (vc-backend (buffer-file-name))
      (project-find-file)
    (call-interactively 'find-file)
    ))

(if-evil
 (nmap-leader
  "SPC" 'amirreza/find-file
  "f f" 'find-file
  "p f" 'project-find-file
  "p p" 'project-switch-project
  "p g" 'project-find-regexp
  )

 (nmap
   "C-j" 'evil-window-bottom
   "C-k" 'evil-window-up
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   )
 )

(provide 'amirreza-emacs)
;;; amirreza-emacs.el ends here
