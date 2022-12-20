;;; amirreza-eglot.el --- Eglot                      -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(elpa-package eglot

  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer nil)

  (global-eldoc-mode)

  (defun amirreza/eglot-hook ()
    (eglot-ensure)
    (put 'eglot-note 'flymake-overlay-control nil)
    (put 'eglot-warning 'flymake-overlay-control nil)
    (put 'eglot-error 'flymake-overlay-control nil)

    (define-key eglot-mode-map (kbd "C-c d") 'eldoc)
    (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "M-r") 'xref-find-references)
    (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c c") 'eglot-code-actions)
    (if-evil
     (nmap "gd" 'xref-find-definitions)
     (nmap "gr" 'xref-find-references)
     (nmap "gi" 'eglot-find-implementation)
     (nmap "gf" 'eglot-format)
     (nmap "S-c" 'eglot-code-actions)
     )
    )

  (add-hook 'go-mode-hook 'amirreza/eglot-hook)
  (add-hook 'rust-mode-hook 'amirreza/eglot-hook)
  (add-hook 'python-mode-hook 'amirreza/eglot-hook)
  (add-hook 'php-mode-hook 'amirreza/eglot-hook))


(provide 'amirreza-eglot)
;;; amirreza-eglot.el ends here
