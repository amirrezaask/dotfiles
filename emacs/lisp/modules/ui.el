;;; ui.el --- UI module                              -*- lexical-binding: t; -*-

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
(pkg! modus-operandi-theme :straight t :defer t)
(pkg! modus-vivendi-theme :straight t :defer t)
(pkg! doom-themes :straight t :defer t)

(setq ring-bell-function t)
(setq visible-bell t)

(defvar amirreza/current-mode 'dark "Current color mode of Emacs.")
(defvar amirreza/dark-theme 'doom-one)
(defvar amirreza/light-theme 'modus-operandi)

(defmacro amirreza/--load-theme (&rest theme-opts)
  `(progn (mapc #'disable-theme custom-enabled-themes)
          (load-theme ,@theme-opts)))

(defun amirreza/load-theme ()
  (interactive)
  (let ((theme (intern  (completing-read "Theme: " (mapcar #'symbol-name
                                                           (custom-available-themes))))))

    (amirreza/--load-theme theme t)))

(defun amirreza/apply-color (mode)
  "Apply current color mode to Emacs."
  (if (eq amirreza/current-mode 'dark)
      (amirreza/--load-theme amirreza/dark-theme t)
    (amirreza/--load-theme  amirreza/light-theme t)))

(defun amirreza/toggle-color-mode ()
  "Toggle current mode to the opposite"
  (interactive)
  (if (eq amirreza/current-mode 'dark)
      (setq amirreza/current-mode 'light)
    (setq amirreza/current-mode 'dark))
  (amirreza/apply-color amirreza/current-mode))
(amirreza/apply-color amirreza/current-mode)

(setq-default ring-bell-function 'ignore)
(setq-default cursor-type 'bar)

(blink-cursor-mode 1)
(global-hl-line-mode +1)

(if-enabled? amirreza/modeline 
             (setq mode-line-percent-position '(-3 "%p"))

             (defface amirreza/buffer-face
               '(
                 (((background dark))  :foreground "IndianRed1" :weight bold)
                 (((background light)) :foreground "blue violet" :weight bold))
               
               "Face for buffer name.")

             (defface amirreza/date-face
               '(
                 (((background dark)) :foreground "yellow" :weight bold)
                 (((background light)) :foreground "tomato" :weight bold))
               
               "Face for global variables.")


             (defface amirreza/vcs-face
               '(
                 (((background dark)) :foreground "cyan" :weight bold)
                 (((background light)) :foreground "olive drab" :weight bold))
               
               "Face for global variables.")

             (defface amirreza/mode-face
               '(
                 (((background dark)) :foreground "spring green" :weight bold)
                 (((background light)) :foreground "royal blue" :weight bold))
               
               "Face for global variables.")

             (defface amirreza/pos-face
               '(
                 (((background dark)) :foreground "light slate blue" :weight bold)
                 (((background light)) :foreground "firebrick" :weight bold))
               
               "Face for global variables.")

             (defface amirreza/workspace-face
               '(
                 (((background dark)) :foreground "orange" :weight bold)
                 (((background light)) :foreground "violet red" :weight bold))
               
               "Face for global variables.")


             (setq display-time-string-forms
                   '((propertize
                      (concat 24-hours ":" minutes " " day "/" month "/" year)
                      'face 'marco-date)))

             (setq-default mode-line-format
                           (list
                            "["
                            '(:eval
                              (let ((workspace-number (format "%d" (eyebrowse--get 'current-slot))))
                                (if (= (length workspace-number) 0)
                                    ""
                                  (propertize workspace-number 'face 'amirreza/workspace-face))))

                            "]"
                            "  "
                            "[" '(:eval (propertize "%b" 'face 'amirreza/buffer-face)) "]"
                            " "
                            "[" '(:eval (propertize "%m" 'face 'amirreza/mode-face)) "]"
                            " "
                            "[" '(:eval (propertize "%l,%c" 'face 'amirreza/pos-face)) "]"
                            " "

                            "[" '(:eval (when-let (vc vc-mode)
                                          (list " "
                                                (propertize (substring vc 5)
                                                            'face 'amirreza/vcs-face)
                                                " "))) "]"
                            " "
                            "[" '(:eval (propertize display-time-string 'face 'amirreza/date-face)) "] ")))

(if-enabled? doom/modeline 
             (pkg! doom-modeline :straight t :config (setq doom-modeline-height 35) (doom-modeline-mode 1)))

(defun amirreza/change-font (font)
  (setq default-frame-alist `((font . ,font))))

(defvar amirreza/font "Hermit-10")
(amirreza/change-font amirreza/font)
(provide 'modules/ui)
;;; ui.el ends here
