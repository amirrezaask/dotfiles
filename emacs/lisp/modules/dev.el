;;; dev.el --- Dev module                            -*- lexical-binding: t; -*-

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
(pkg! company
      :straight t
      :hook (prog-mode . company-mode)
      :bind (:map company-active-map
                  ("C-n" . company-select-next)
                  ("C-p" . company-select-previous)
                  ("C-o" . company-other-backend)
                  ("<tab>" . company-complete-common-or-cycle)
                  ("RET" . company-complete-selection))
      :config
      (setq company-minimum-prefix-lenght 1)
      (setq company-tooltip-limit 30)
      (setq company-idle-delay 0.0)
      (setq company-echo-delay 0.1)
      (setq company-show-numbers t)
      (setq company-backends '(company-capf company-dabbrev company-files company-dabbrev-code)))

(pkg! imenu
      :bind ("M-i" . imenu))
(pkg! abbrev :init (setq save-abbrevs 'silently) :commands (expand-abbrev))
(pkg! skeleton
      :commands (amirreza/defsnippet)
      :config
      (defmacro amirreza/defsnippet (mode abbrv &rest skeleton-expansions)
        "Snippets are wrapper around skeleton and abbrevs."
        (let ((command-name (intern (format "amirreza/snippet-%s-%s" mode abbrv))))
          `(progn
             (define-skeleton ,command-name ""
               ,@skeleton-expansions)
             (define-abbrev local-abbrev-table ,abbrv "" (quote ,command-name))))))

(pkg! ggtags :straight t) ;; TODO

(pkg! counsel-gtags :straight t
      :bind (("s-." . 'counsel-gtags-dwim)))

(pkg! dumb-jump
      :disabled t
      :straight t
      :hook
      (xref-backend-functions . #'dumb-jump-xref-activate))


(pkg! lsp-mode :straight t
      :init
      (setq lsp-file-watch-threshold 10000)
      (setq lsp-auto-guess-root t)
      (setq lsp-keymap-prefix "C-c l")
      (setq lsp-before-save-edit t)
      (defun amirreza/lsp ()
        (lsp))
      
      :hook (
             (lsp-mode . lsp-enable-which-key-integration)
             (before-save . lsp-format-buffer)))

(pkg! eglot :disabled t :straight t :init (defun amirreza/lsp () (eglot-ensure)))

(pkg! dap-mode :straight t
      :defer t
      :disabled t
      :config
      (dap-ui-mode 1)
      ;; enables mouse hover support
      (dap-tooltip-mode 1)
      ;; use tooltips for mouse hover
      ;; if it is not enabled `dap-mode' will use the minibuffer.
      (tooltip-mode 1)
      ;; displays floating panel with debug buttons
      ;; requies emacs 26+
      (dap-ui-controls-mode 1))


(pkg! eldoc
      :config (global-eldoc-mode 1))

(pkg! autoinsert
      :hook (prog-mode . auto-insert-mode))

(provide 'modules/dev)
;;; dev.el ends here
