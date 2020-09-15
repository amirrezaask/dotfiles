;;; corelib.el --- CoreLib main script               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
;; Keywords: 

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

;; 

;;; Code:


(defvar corelib/emacs-init-timestamp (float-time) "Holds Emacs initialization time.")

(defun corelib/init-package-manager ()
  "Initialize Straight.el package manager."
  ;; Initialize Package manager
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))


(defun corelib/tangle-literate-config (path output)
  "Tangle given literate config."
  (require 'org)
  (org-babel-tangle-file path output "emacs-lisp"))

(defun corelib/use-literate-config (input output)
  (unless (file-exists-p output)
    (corelib/tangle-literate-config input output))
  (add-hook 'kill-emacs-hook (lambda () (corelib/tangle-literate-config input output))))

(defun corelib/faster-start ()
  ;; Defer Garbage collection
  (setq gc-cons-threshold most-positive-fixnum)

  ;; Restore garbage collection after initialization
  (add-hook 'after-init-hook (lambda ()
                               (require 'corelib-gcmh)
                               (gcmh-mode 1)))
  ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)


  (defvar file-name-handler-alist-bak file-name-handler-alist "file name handler backup.")
  
  (setq file-name-handler-alist nil)

  (add-hook 'after-init-hook (lambda () (setq file-name-handler-alist file-name-handler-alist-bak)))

  (setq package-enable-at-startup nil)

  (tool-bar-mode 0) ;; disable tool-bar

  (scroll-bar-mode 0) ;; disable scroll-bar

  (menu-bar-mode 0) ;; disable menu-bar

  (setq vc-follow-symlinks t)

  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  
)

(provide 'corelib)
;;; corelib.el ends here
