;;; init.el --- My Emacs initial script              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Amirreza Askarpour

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

;; 1. install packages
;; 2. Setting configuration values and defining helper functions
;; 3. Keybindings

(setq package-enable-at-startup nil) ;; Disable default package manager package.el
(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Installing Package manager                            ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Installing Packages                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All packages I install
(setq amirreza/packages
 '(
   ;; Basic stuff
   hydra
   helpful
   gcmh
   expand-region

   ;; Window and buffer management
   ace-window
   bufler

   ;; Grep and Search
   wgrep
   ripgrep

   ;; Org mode stuff
   ox-reveal
   ob-go
   ob-rust
   ob-php
   htmlize

   ;; Git Stuff
   git-gutter
   magit

   ;; Themes
   ef-themes
   solarized-theme
   doom-themes
   gruber-darker-theme

   ;; Autocompletion menu
   (corfu :type git :host github :repo "emacs-straight/corfu" :files ("*" "extensions/*.el" (:exclude ".git")))
   corfu-terminal
   corfu-prescient

   ;; Minibuffer completion
   vertico
   vertico-prescient
   consult
   marginalia
   orderless

   go-mode ;; Go major mode
   go-tag ;; Struct tags in Golang
   go-gen-test ;; Generate test for function

   rustic ;; LLVM with some haskell on it

   clojure-mode ;; LISP on JVM
   cider ;; Clojure repl integration

   lua-mode ;; Lua

   zig-mode ;; Zig

   pip-requirements ;; requirements.txt
   pipenv ;; pipenv
   pyimport
   (python-isort :type git :host github :repo "wyuenho/emacs-python-isort")

   js2-mode ;; Javascript
   rjsx-mode ;; React JSX syntax
   typescript-mode ;; Typescript syntax
   nodejs-repl ;; Nodejs repl
   tide ;; Typescript IDE

   php-mode ;; PHP
   psysh ;; PHP repl in Emacs
   composer ;; Composer

   apache-mode ;; Apache config syntax
   cmake-mode ;; CMake
   systemd ;; Systemd config syntax
   nginx-mode;; Nginx config syntax
   docker-compose-mode ;; Docker-compose syntax
   dockerfile-mode ;; Dockerfile syntax
   markdown-mode ;; Markdown syntax
   yaml-mode ;; Yaml
   fish-mode ;; Fish
   nix-mode ;; Nix

   json-mode ;; JSON
   json-snatcher ;; Show path of json value at POINT

   csv-mode ;; CSV

   perspective ;; Workspace management
   ))
(mapc (lambda (pkg)
	(straight-use-package pkg)) amirreza/packages)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Customization Variables                               ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq debug-on-init t) ;; debug emacs itself
(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")

(setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "Source Code Pro")
;; (setq amirreza/font "OperatorMono Nerd Font Light")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")
;; (setq amirreza/font "Iosevka")
(setq amirreza/font-size "18")

(setq amirreza/theme 'jblow) ;; default theme

(gcmh-mode 1) ;; Smartly manage Emacs garbage collector pauses (better perf).

(setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation

(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq make-backup-files nil) ;; Disable backup files ~file
(setq auto-save-default nil) ;; Disable auto save files

(setq inhibit-startup-screen t) ;; No startup splash screen

(setq use-dialog-box nil) ;; Do not use UI for questions
(setq ring-bell-function 'ignore) ;; Do not beep please.

(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar

(exec-path-from-shell-initialize) ;; Copy PATH from default shell

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; don't touch my config for custom variables put them in another file.

(defalias 'yes-or-no-p 'y-or-n-p) ;; Instead of yes or no use y or n

(setq echo-keystrokes 0.4) ;; faster echoing of keystrokes in minibuffer.

(defun amirreza/edit-emacs ()
  "Edit emacs configuration."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "C-c e e") 'amirreza/edit-emacs)

(defun amirreza/getenv (name default)
  "Get env if not defined use default"
  (let ((value (getenv name)))
    (if value
	value
      default)))

(defmacro amirreza/defhydra (name body heads)
  `(eval (append '(defhydra ,name ,body) ,heads)))


(global-set-key (kbd "C-x o") 'ace-window) ;; window switch
(global-set-key (kbd "C-x C-b") 'bufler) ;; buffer management

(global-unset-key (kbd "C-z")) ;; No minimizing

(add-hook 'dired-mode-hook (lambda () ;; Make a dired buffer writable and changes will affect files structure in disk.
			     (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)))

;; Improve help buffers in emacs.
(global-set-key [remap describe-key] 'helpful-key)
(global-set-key [remap describe-function] 'helpful-callable)
(global-set-key [remap describe-variable] 'helpful-variable)

;; Add custom themes path to themes load path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

;; disable other themes before loading new one.
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))

(load-theme amirreza/theme t)
(global-set-key (kbd "C-c t t") 'load-theme)

;; Font settings
(defun amirreza/display-benq ()
  (interactive)
  (setq amirreza/font-size "23")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; My font setup for my laptop setup
(defun amirreza/display-macbook-personal ()
  (interactive)
  (setq amirreza/font-size "15")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Interactively ask for font size
(defun set-font-size (size)
  (interactive "sSize: ")
  (setq amirreza/font-size size)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Reload font settings
(defun reload-font ()
  (interactive)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(reload-font) ;; Load font settings

(setq-default cursor-type 'box) ;; set cursor type to be box, I like box better that a single horizontal line, it's more observable.
(global-hl-line-mode 1) ;; highlight current line that cursor is one, again for observabality.
(blink-cursor-mode -1) ;; Cusror blinking is distracting

;; Autocompletion configs
(setq corfu-auto t) ;; make corfu start automatically aka AutoComplete or Intelisense for VSCode guys.
(setq corfu-auto-delay 0.1) ;; Less delay to show the autocompletion menu.
(global-corfu-mode) ;; Globally enable auto complete.
(corfu-history-mode 1)
(corfu-echo-mode 1)
(corfu-popupinfo-mode 1)
(corfu-terminal-mode)
(corfu-prescient-mode)

;; Minibuffer completion
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
(setq vertico-count 15)
(setq vertico-cycle t)
(vertico-mode)
(vertico-prescient-mode)
(setq consult-async-min-input 1)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
(setq show-paren-delay 0) ;; highlight matching parens instantly.
(show-paren-mode 1) ;; Highlight matching parens
(setq display-line-numbers-type 'relative) ;; relative line numbers
(global-display-line-numbers-mode 1) ;; enable line numbers globaly

(defun amirreza/up-center ()
  "Move half a page up and also center the cursor."
  (interactive)
  (previous-line (/ (window-height) 2))
  (recenter-top-bottom))

(defun amirreza/down-center ()
  "Move half a page down and also center the cursor."
  (interactive)
  (next-line (/ (window-height) 2))
  (recenter-top-bottom))

;; Best movement ever ?????
(setq recenter-positions '(middle))

(global-set-key (kbd "M-p") 'amirreza/up-center)
(global-set-key (kbd "M-n") 'amirreza/down-center)

(global-set-key (kbd "C-=") 'er/expand-region) ;; Expand your current selection based on the semantics of syntax.
(global-set-key (kbd "C--") 'er/contract-region) ;; Contract your current selection based on the semantics of syntax.

(global-set-key (kbd "C-q") 'set-mark-command) ;; start selecting
(global-unset-key (kbd "C-SPC"))


(setq org-use-property-inheritance t) ;; Inherit properties of the parent node in Org Emode.
(setq org-startup-folded t) ;; Start org mode all headers collapsed
(setq org-src-window-setup 'current-window) ;; use current window in org src edit.
(setq org-src-tab-acts-natively nil)


(defhydra amirreza/org-mode-hydra (:exit t)
  ("l" org-toggle-link-display "Toggle Link Display")
  ("e" org-export-dispatch "Export")
  ("o" org-open-at-point "Open At Point")
  ("h" (lambda () (interactive) (org-export-as 'html)) "Org Export To HTML")
  ("t"  org-todo "Open At Point"))

(add-hook 'org-mode-hook (lambda ()
			     (define-key org-mode-map (kbd "C-c m") 'amirreza/org-mode-hydra/body)
			     (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit))) ;; more consistent with magit and other modes.


(global-git-gutter-mode) ;; Show git changes along side line numbers.

(global-set-key (kbd "C-x g") 'magit) ;; Best Git client ever


(defhydra amirreza/project-hydra (:exit t) ;; Emacs can understands Projects
  ("f" project-find-file "Find File")
  ("p" project-switch-project "Switch To Project")
  ("b" project-buffers "Find Buffer In Project")
  ("c" project-compile "Compile Project"))

(global-set-key (kbd "C-x p") 'amirreza/project-hydra/body)

(setq amirreza/programming-hydra-heads '())

(add-to-list 'amirreza/programming-hydra-heads '("n" flymake-goto-next-error "Goto Next Error"))
(add-to-list 'amirreza/programming-hydra-heads '("p" flymake-goto-prev-error "Goto Previous Error"))
(add-to-list 'amirreza/programming-hydra-heads '("e" consult-flymake "List of errors"))

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-r") 'xref-find-references)

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer nil)

(add-to-list 'amirreza/programming-hydra-heads '("." amirreza/eldoc-toggle-buffer "Toggle Eldoc for point"))

(global-set-key (kbd "C-h .") 'eldoc)
(global-set-key (kbd "M-0") 'eldoc)

(global-eldoc-mode)
;; Language with LSP
(add-hook 'prog-mode-hook #'eglot-ensure)

(add-to-list 'amirreza/programming-hydra-heads '("d" eldoc "Document THING at POINT"))
(add-to-list 'amirreza/programming-hydra-heads '("D" xref-find-definitions "Goto Definitions"))
(add-to-list 'amirreza/programming-hydra-heads '("r" xref-find-references "Find References"))
(add-to-list 'amirreza/programming-hydra-heads '("i" eglot-find-implementation "Find Implementations"))
(add-to-list 'amirreza/programming-hydra-heads '("s" consult-eglot-symbols "Workspace Symbols"))
(add-to-list 'amirreza/programming-hydra-heads '("R" eglot-rename "Rename"))
(add-to-list 'amirreza/programming-hydra-heads '("f" eglot-format "Format"))

(amirreza/defhydra amirreza/programming-hydra (:exit t)
		   amirreza/programming-hydra-heads)

(add-hook 'prog-mode-hook (lambda ()
			    (define-key prog-mode-map (kbd "C-c m") 'amirreza/programming-hydra/body)))



(setq rustic-lsp-client 'eglot) ;; Rustic default is lsp-mode

(amirreza/defhydra amirreza/go-hydra
		   (:exit t)
		   (append amirreza/programming-hydra-heads '(("a" go-tag-add "Add Struct Tag"))))
  
(add-hook 'go-mode-hook (lambda ()
			  (define-key go-mode-map (kbd "C-c m") 'amirreza/go-hydra/body)))

(setq amirreza/json-hydra-heads '(
				  ("f" json-pretty-print "Pretty print region")
				  ("F" json-pretty-print-buffer "Pretty print buffer")
				  ))
(amirreza/defhydra amirreza/json-hydra (:exit t) amirreza/json-hydra-heads)

(add-hook 'json-mode-hook (lambda ()
			    (define-key (kbd "C-c m") 'amirreza/json-hydra/body)))


(setq persp-state-default-file (expand-file-name "sessions" user-emacs-directory))
(setq persp-mode-prefix-key (kbd "C-c w"))

(defun amirreza/save-session ()
  (interactive)
  (persp-state-save persp-state-default-file))

(defun amirreza/load-session ()
  (interactive)
  (persp-state-load persp-state-default-file))

(persp-mode 1)

(global-set-key (kbd "C-c w s") 'persp-switch)

(when (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'meta))


(provide 'init)
;;; init.el ends here


