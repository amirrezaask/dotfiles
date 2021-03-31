;;; windows.el --- Windows module                    -*- lexical-binding: t; -*-

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


(setq display-buffer-alist
      '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-width . 0.40)
         (side . right)
         (slot . 0))
        ("^vterm"
         (display-buffer-in-side-window)
         (window-width . 0.40)
         (side . right)
         (slot . 0))
        ("\*eshell.*"
         (display-buffer-in-side-window)
         (window-width . 0.40)
         (side . right)
         (slot . 0))
        ("\\*rg"
         (display-buffer-in-side-window)
         (window-width . 0.50)
         (side . right)
         (slot . 0))))

(pkg! eyebrowse 
      :straight t
      :demand
      :commands (eyebrowse-close-window-config
                 eyebrowse-create-window-config
                 eyebrowse-switch-to-window-config-0
                 eyebrowse-switch-to-window-config-1
                 eyebrowse-switch-to-window-config-2
                 eyebrowse-switch-to-window-config-3
                 eyebrowse-switch-to-window-config-4
                 eyebrowse-switch-to-window-config-5
                 eyebrowse-switch-to-window-config-6
                 eyebrowse-switch-to-window-config-7
                 eyebrowse-switch-to-window-config-8
                 eyebrowse-switch-to-window-config-9)
      :config (eyebrowse-mode +1) 
      :bind (("C-c w 0" . eyebrowse-switch-to-window-config-0)
             ("C-c w 1" . eyebrowse-switch-to-window-config-1)
             ("C-c w 2" . eyebrowse-switch-to-window-config-2)
             ("C-c w 3" . eyebrowse-switch-to-window-config-3)
             ("C-c w 4" . eyebrowse-switch-to-window-config-4)
             ("C-c w 5" . eyebrowse-switch-to-window-config-5)
             ("C-c w 6" . eyebrowse-switch-to-window-config-6)
             ("C-c w 7" . eyebrowse-switch-to-window-config-7)
             ("C-c w 8" . eyebrowse-switch-to-window-config-8)
             ("C-c w 9" . eyebrowse-switch-to-window-config-9)
             ("C-c w n" . eyebrowse-create-window-config)
             ("C-c w c" . eyebrowse-close-window-config)))

(pkg! winner
      :config
      (winner-mode 1)
      :commands (winner-redo winner-undo)
      :bind (("C->" . winner-redo)
             ("C-<" . winner-undo)))

(pkg! ace-window
      :straight t
      :commands (ace-window)
      :bind (("C-x o" . 'ace-window)
             ("C-x C-o" . 'ace-window)))
(provide 'modules/windows)
;;; windows.el ends here
