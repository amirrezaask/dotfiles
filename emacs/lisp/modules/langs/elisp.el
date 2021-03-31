;;; elisp.el --- Elisp module                        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AmirrezaAskarpour

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

(pkg! elisp-mode
      :hook
      (emacs-lisp-mode-hook . amirreza/elisp-hook)
      :config
      (defun amirreza/elisp-hook ()
        (setq-local prettify-symbols-alist '(("fn" . 955)))
        (defun --amirreza/emacs-lisp-repeat (str count)
         "Create dashes with given COUNT."
         (let ((dashes ""))
           (dotimes (iterator count dashes)
             (setq dashes (concat dashes str)))))

       (defun --amirreza/emacs-lisp-wrap-text-in-spaces (text)
         (let* ((len (length text))
                (spaces-length-side (/ (- 80 len) 2))
                (spaces-side (--amirreza/emacs-lisp-repeat " " spaces-length-side)))
           (format "%s%s%s" spaces-side text spaces-side)))

       (defun amirreza/emacs-lisp-insert-comment-line (text)
         "Insert a comment line with given TEXT."
         (interactive "sComment: ")
         (let* ((text-wrapped (--amirreza/emacs-lisp-wrap-text-in-spaces text))
                (dashes (--amirreza/emacs-lisp-repeat "=" 80))))
         (insert (format "\n;;%s\n;;%s\n;;%s" dashes text-wrapped dashes))))
     :bind
     (:map emacs-lisp-mode-map
           ("C-c m c" . 'amirreza/emacs-lisp-insert-comment-line)))



(provide 'modules/langs/elisp)
;;; elisp.el ends here
