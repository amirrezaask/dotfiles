;; (setq debug-on-init t) ;; debug emacs itself

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Early-init.el                                         ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-enable-at-startup nil) ;; Disable default package manager package.el
(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 100 1024 1024)) ;; Increase Emacs garbage collector threshold to 100 MB
(setq read-process-output-max (* 1024 1024)) ;; Increase Emacs read from process output to 100 MB

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
   ;; Core
   hydra
   helpful
   gcmh
   exec-path-from-shell
   which-key

   ;; Editing
   multiple-cursors
   expand-region
   parinfer-rust-mode
   keycast
   avy
   
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
   cyberpunk-theme
   gruvbox-theme
   exotica-theme

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

   racket-mode ;; Racket LISP

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

   vterm
   multi-vterm

   perspective ;; Workspace management
   ))

(mapc (lambda (pkg)
	(straight-use-package pkg)) amirreza/packages)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Customize core stuff                                  ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")

(setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "Go Mono")
;; (setq amirreza/font "Source Code Pro")
;; (setq amirreza/font "OperatorMono Nerd Font Light")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")
;; (setq amirreza/font "Iosevka")
(setq amirreza/font-size "18")

(setq amirreza/theme 'modus-vivendi) ;; default theme

(gcmh-mode 1) ;; Smartly manage Emacs garbage collector pauses (better perf).

(setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation

(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq make-backup-files nil) ;; Disable backup files ~file
(setq auto-save-default nil) ;; Disable auto save files

(setq vc-follow-symlinks t)

(setq inhibit-startup-screen t) ;; No startup splash screen

(setq use-dialog-box nil) ;; Do not use UI for questions
(setq ring-bell-function 'ignore) ;; Do not beep please.

(exec-path-from-shell-initialize) ;; Copy PATH from default shell

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; don't touch my config for custom variables put them in another file.

(defalias 'yes-or-no-p 'y-or-n-p) ;; Instead of yes or no use y or n

(setq echo-keystrokes 0.4) ;; faster echoing of keystrokes in minibuffer.
(which-key-mode)

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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Help me daddy                                         ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [remap describe-key] 'helpful-key)
(global-set-key [remap describe-function] 'helpful-callable)
(global-set-key [remap describe-variable] 'helpful-variable)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Font & Theme                                          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add custom themes path to themes load path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

;; disable other themes before loading new one.
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Cursor configs                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default cursor-type 'box) ;; set cursor type to be box, I like box better that a single horizontal line, it's more observable.
(global-hl-line-mode 1) ;; highlight current line that cursor is one, again for observabality.
(blink-cursor-mode -1) ;; Cusror blinking is distracting

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Auto Completion                                       ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq corfu-auto t) ;; make corfu start automatically aka AutoComplete or Intelisense for VSCode guys.
(setq corfu-auto-delay 0.1) ;; Less delay to show the autocompletion menu.
(global-corfu-mode) ;; Globally enable auto complete.
(corfu-history-mode 1) ;; Sort completion based on history.
(corfu-echo-mode 1) ;; Show candidate documentation in echo area.
(corfu-popupinfo-mode 1) 
(unless (display-graphic-p) ;; If in terminal emacs do some compat stuff.
  (corfu-terminal-mode))
(corfu-prescient-mode)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Minibuffer Completion                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Editor Experience                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
(setq show-paren-delay 0) ;; highlight matching parens instantly.
(show-paren-mode 1) ;; Highlight matching parens
(setq display-line-numbers-type 'relative) ;; relative line numbers
(global-display-line-numbers-mode 1) ;; enable line numbers globaly

(add-hook 'prog-mode-hook 'hs-minor-mode) ;; Enable HideShow minor mode for all programming mdoes.
(global-set-key (kbd "C-'") 'hs-toggle-hiding) ;; toggle Fold

(global-set-key (kbd "C->") 'mc/mark-next-like-this) ;; Insert cursor next
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this) ;; Insert cursor prev

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

(global-set-key (kbd "C-;") 'avy-goto-line)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Org Mode                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Git                                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-git-gutter-mode) ;; Show git changes along side line numbers.

(global-set-key (kbd "C-x g") 'magit) ;; Best Git client ever

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Project                                               ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defhydra amirreza/project-hydra (:exit t) ;; Emacs can understands Projects
  ("f" project-find-file "Find File")
  ("p" project-switch-project "Switch To Project")
  ("b" project-buffers "Find Buffer In Project")
  ("c" project-compile "Compile Project"))

(global-set-key (kbd "C-x p") 'amirreza/project-hydra/body)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Programming                                           ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This will eventualy have all actions I want as sort of my context menu in programming modes.
(setq amirreza/programming-hydra-heads '())

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Programming (Diagnostics)                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'amirreza/programming-hydra-heads '("n" flymake-goto-next-error "Goto Next Error"))
(add-to-list 'amirreza/programming-hydra-heads '("p" flymake-goto-prev-error "Goto Previous Error"))
(add-to-list 'amirreza/programming-hydra-heads '("e" consult-flymake "List of errors"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Programming (Goto)                                          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-.") 'xref-find-definitions) ;; Jump to definitions
(global-set-key (kbd "M-,") 'xref-go-back) ;; Jump back
(global-set-key (kbd "M-r") 'xref-find-references) ;; Find references

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Programming (Documentation)                                          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq eldoc-echo-area-use-multiline-p nil) ;; Don't do multiline document in minibuffer it's distracting
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer nil)
(add-to-list 'amirreza/programming-hydra-heads '("." amirreza/eldoc-toggle-buffer "Toggle Eldoc for point"))
(global-set-key (kbd "C-h .") 'eldoc)
(global-set-key (kbd "M-0") 'eldoc)
(global-eldoc-mode) ;; All modes should have emacs documentatino system enabled.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Programming (LSP aka eglot)                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (>= emacs-major-version 29) ;; If emacs 28 or lower install eglot.
  (straight-use-package 'eglot))

(add-hook 'prog-mode-hook #'eglot-ensure) ;; Try to use LSP for all programming modes.



(add-to-list 'amirreza/programming-hydra-heads '("d" eldoc "Document THING at POINT"))
(add-to-list 'amirreza/programming-hydra-heads '("D" xref-find-definitions "Goto Definitions"))
(add-to-list 'amirreza/programming-hydra-heads '("r" xref-find-references "Find References"))
(add-to-list 'amirreza/programming-hydra-heads '("i" eglot-find-implementation "Find Implementations"))
(add-to-list 'amirreza/programming-hydra-heads '("s" consult-eglot-symbols "Workspace Symbols"))
(add-to-list 'amirreza/programming-hydra-heads '("R" eglot-rename "Rename"))
(add-to-list 'amirreza/programming-hydra-heads '("f" eglot-format "Format"))

(setq rustic-lsp-client 'eglot) ;; Rustic default is lsp-mode

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Programming                                         ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(amirreza/defhydra amirreza/programming-hydra (:exit t)
		   amirreza/programming-hydra-heads)

(add-hook 'prog-mode-hook (lambda ()
			    (define-key prog-mode-map (kbd "C-SPC") 'amirreza/programming-hydra/body)
			    (define-key prog-mode-map (kbd "C-c m") 'amirreza/programming-hydra/body)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       Go                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(amirreza/defhydra amirreza/go-hydra
		   (:exit t)
		   (append amirreza/programming-hydra-heads '(("a" go-tag-add "Add Struct Tag"))))
  
(add-hook 'go-mode-hook (lambda ()
			  (define-key go-mode-map (kbd "C-SPC") 'amirreza/go-hydra/body)
			  (define-key go-mode-map (kbd "C-c m") 'amirreza/go-hydra/body)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       JSON                                            ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq amirreza/json-hydra-heads '(
				  ("f" json-pretty-print "Pretty print region")
				  ("F" json-pretty-print-buffer "Pretty print buffer")))
(amirreza/defhydra amirreza/json-hydra (:exit t) amirreza/json-hydra-heads)

(add-hook 'json-mode-hook (lambda ()
			    (define-key (kbd "C-SPC") 'amirreza/json-hydra/body)
			    (define-key (kbd "C-c m") 'amirreza/json-hydra/body)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Workspaces                                            ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 MacOS Settings                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta) ;; make command key in mac act as meta/alt
    (setq mac-option-modifier 'meta))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Eshell & VTerm                                         ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spawn-eshell-with-name (name)
  "Spawn a new eshell instance with given NAME."
  (interactive "sName: ")
  (let ((eshell-buffer-name (format "*eshell-%s*" name)))
    (eshell)))


(defun eshell--complete-history-prompt ()
  "Prompt with completion for history elements from `eshell-history-ring`."
  (interactive)
  (if-let ((hist (ring-elements eshell-history-ring)))
      (insert (completing-read "Input from history: "
                       hist nil t nil
                       'prot-eshell--complete-history-prompt-history))
    (user-error "There is no Eshell history")))

(add-hook 'eshell-mode-hook (lambda ()
			      (define-key eshell-mode-map (kbd "C-r") 'eshell--complete-history-prompt)))

(defun spawn-eshell ()
  "Spawn a new eshell instance with the name of current buffer or jump if there is one existing"
  (interactive)
  (let ((eshell-buffer-name (format "*eshell-%s-buffer*" (buffer-name (current-buffer)))))
    (eshell)))

(defhydra amirreza/spawn-hydra (:exit t)
  ("v" multi-vterm "Spawn VTerm")
  ("e" spawn-eshell-with-name "Spawn Eshell"))

(global-set-key (kbd "C-c s e") 'amirreza/spawn-hydra/body)
(global-set-key (kbd "C-S-s") 'spawn-eshell)

(provide 'init)
;;; init.el ends here
