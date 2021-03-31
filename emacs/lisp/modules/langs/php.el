;;; php.el --- PHP module                            -*- lexical-binding: t; -*-

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

(pkg! php-mode :straight t 
      :mode "\\.php\\'")

(pkg! php-runtime :straight t :after php-mode)

(pkg! composer :straight t :after php-mode)

(pkg! phpunit :straight t
      :after php-mode
      :config 
      (define-key php-mode-map (kbd "C-c m t t") 'phpunit-current-test)
      (define-key php-mode-map (kbd "C-c m t c")  'phpunit-current-class)
      (define-key php-mode-map (kbd "C-c m t p")  'phpunit-current-project))

(provide 'modules/langs/php)
;;; php.el ends here
