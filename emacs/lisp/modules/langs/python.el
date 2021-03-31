;;; python.el --- python module                      -*- lexical-binding: t; -*-

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

(pkg! python-mode
      :mode "\\.py\\'"
      :hook
      (python-mode . amirreza/lsp)
      :config
      (defun amirreza/python-insert-docstring ()
        (interactive)
        (insert "'''\n'''")
        (previous-line))
     :bind
     (:map python-mode-map 
           ("C-c m d" . amirreza/python-insert-docstring)))
(pkg! pipenv
      :straight t
      :after python-mode)
(pkg! lsp-python-ms :straight t :after python-mode)

(pkg! py-autopep8
      :straight t
      :hook python-mode
      :config
      (py-autopep8-enable-on-save))

(provide 'modules/langs/python)
;;; python.el ends here
