;;; evil.el --- evil module                          -*- lexical-binding: t; -*-

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
(pkg! evil
      :straight t
      :init
      (setq evil-want-keybinding nil)
      :bind
      (:map evil-normal-state-map
            ("g c" . comment-line)
            ("SPC f f" . counsel-find-file)
            ("SPC ." . counsel-M-x)
            ("SPC h d f" . describe-function)
            ("SPC h d v" . describe-variable)
            ("SPC h d k" . describe-key))
      :config
      (evil-mode 1))

(pkg! evil-escape :straight t :config (setq-default evil-escape-key-sequence "jk") (setq evil-escape-unordered-key-sequence t) (evil-escape-mode 1))
(pkg! evil-collection :straight t :config (evil-collection-init))

(pkg! evil-surround
      :straight t
      :config
      (global-evil-surround-mode 1))
            


(provide 'modules/evil)
;;; evil.el ends here
