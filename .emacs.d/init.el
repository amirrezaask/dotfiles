;; (setq debug-on-init t)
(setq user-full-name "Amirreza Askarpour")
(setq user-email "raskarpour@gmail.com")

;; (setq amirreza/font "Source Code Pro")
;; (setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "OperatorMono Nerd Font Light")
(setq amirreza/font "JetBrainsMono Nerd Font Mono")
;; (setq amirreza/font "Iosevka")

(setq amirreza/font-size "20")
(setq amirreza/theme 'doom-dracula)

;; If early-init wasn't there.
(setq package-enable-at-startup nil) ;; Disable default package manager package.el
(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))


(setq vc-follow-symlinks t)
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap straight.el
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

;; All packages I install
(setq amirreza/packages
 '(
   ;; Basic stuff
   hydra
   helpful
   gcmh

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

   ;; Language major modes
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

   vterm

   perspective ;; Workspace management
   ))

(mapc (lambda (pkg)
	(straight-use-package pkg)) amirreza/packages)

(gcmh-mode 1)
(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq native-comp-async-report-warnings-errors 'silent) ;; Silent Emacs 28 native compilation
(setq make-backup-files nil) ;; Disable backup files ~file
(setq auto-save-default nil) ;; Disable auto save files
(setq inhibit-startup-screen t) ;; No startup splash screen
(setq use-dialog-box nil) ;; Do not use UI for questions
(setq ring-bell-function 'ignore) ;; Do not beep please.

(tool-bar-mode 0) ;; disable top toolbar
(scroll-bar-mode 0) ;; disable scroll bar
(menu-bar-mode -1) ;; Disable menu bar


(straight-use-package 'exec-path-from-shell)

;; Copy PATH from default shell
(exec-path-from-shell-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.4)


(defun amirreza/find-file ()
  "Smart find file function to do project-files if in Git repo otherwise use default find-file."
  (interactive)
  (if (vc-backend (buffer-file-name))
      (project-find-file)
    (call-interactively 'find-file)))

(defun amirreza/edit-emacs ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


(defun amirreza/getenv (name default)
  "Get env if not defined use default"
  (let ((value (getenv name)))
    (if value
	value
      default)))

(defmacro amirreza/defhydra (name body heads)
  `(eval (append '(defhydra ,name ,body) ,heads)))

(global-set-key (kbd "C-c e e") 'amirreza/edit-emacs)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-b") 'bufler)

(add-hook 'dired-mode-hook (lambda ()
			     (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)))

(global-set-key [remap describe-key] 'helpful-key)
(global-set-key [remap describe-function] 'helpful-callable)
(global-set-key [remap describe-variable] 'helpful-variable)

;; Add custom themes path to themes load path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

(setq amirreza/--current-theme nil)

(defun amirreza/switch-theme ()
  (interactive)
  (let ((theme (intern (completing-read "Theme: " (mapcar #'symbol-name
							  (custom-available-themes))))))
    (amirreza/load-theme theme)))

(defun amirreza/load-theme (theme)
  (when (not (eq amirreza/--current-theme nil))
    (disable-theme amirreza/--current-theme))
  (setq amirreza/--current-theme theme)
  (load-theme amirreza/--current-theme t))

(amirreza/load-theme amirreza/theme)
(global-set-key (kbd "C-c t t") 'amirreza/switch-theme)

;; Font settings
(defun amirreza/display-benq ()
  (interactive)
  (setq amirreza/font-size "23")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; My font setup for my laptop setup
(defun amirreza/display-mac ()
  (interactive)
  (setq amirreza/font-size "15")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Interactively ask for font size
(defun amirreza/set-font-size (size)
  (interactive "sSize: ")
  (setq amirreza/font-size size)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

;; Reload font settings
(defun amirreza/reload-font ()
  (interactive)
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(amirreza/reload-font)

(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

;; Autocompletion configs
(setq corfu-auto t)
(setq corfu-auto-delay 0.1)
(global-corfu-mode)
(corfu-history-mode 1)
(corfu-echo-mode 1)
(corfu-popupinfo-mode 1)
(corfu-terminal-mode)
(corfu-prescient-mode)

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

(setq vertico-count 15)
(setq vertico-cycle t)
(vertico-mode)

(setq consult-async-min-input 1)
(marginalia-mode)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(vertico-prescient-mode)

(delete-selection-mode 1) ;; When a region of text is selected and then something is typed remove text and replace with what has been typed.
(setq show-paren-delay 0) ;; highlight matching parens instantly.
(show-paren-mode 1) ;; Highlight matching parens
(setq display-line-numbers-type 'relative) ;; relative line numbers
(global-display-line-numbers-mode 1) ;; enable line numbers globaly

(defun amirreza/up-center ()
  (interactive)
  (previous-line (/ (window-height) 2))
  (recenter-top-bottom))

(defun amirreza/down-center ()
  (interactive)
  (next-line (/ (window-height) 2))
  (recenter-top-bottom))

;; Best movement ever ?????
(setq recenter-positions '(middle))

(global-set-key (kbd "M-p") 'amirreza/up-center)
(global-set-key (kbd "M-n") 'amirreza/down-center)

(straight-use-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(global-set-key (kbd "C-q") 'set-mark-command)

(setq org-use-property-inheritance t)
(setq org-startup-folded t) ;; Start org mode all headers collapsed
(setq org-src-window-setup 'current-window)
(setq org-src-tab-acts-natively nil)

(defun amirreza/org-code-block ()
  (interactive)
  (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC"
		  (completing-read "Language: "
				   '("emacs-lisp"
				     "go"
				     "rust"
				     "python"
				     "lua"
				     "bash"
				     "sh"
				     "fish"
				     "java"
				     )))))

(defun amirreza/org-disable-tangle ()
  (interactive)
  (insert ":PROPERTIES:
:header-args:    :tangle no
:END:"))

(defhydra amirreza/org-mode-hydra (:exit t)
  ("l" org-toggle-link-display "Toggle Link Display")
  ("b" amirreza/org-code-block "Insert a Code Block")
  ("n" amirreza/org-disable-tangle "Disable Tangle PROPERTIES")
  ("e" org-export-dispatch "Export")
  ("o" org-open-at-point "Open At Point")
  ("h" (lambda () (interactive) (org-export-as 'html)) "Org Export To HTML")
  ("t"  org-todo "Open At Point"))


(add-hook 'org-mode-hook (lambda ()
			     (define-key org-mode-map (kbd "C-c m") 'amirreza/org-mode-hydra/body)
			     (define-key org-mode-map (kbd "C-x m") 'amirreza/org-mode-hydra/body)
			     (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)))



(global-git-gutter-mode)

(global-set-key (kbd "C-x g") 'magit)

(defhydra amirreza/project-hydra (:exit t)
  ("f" project-find-file "Find File")
  ("p" project-switch-project "Switch To Project")
  ("b" project-buffers "Find Buffer In Project")
  ("c" project-compile "Compile Project"))

(global-set-key (kbd "C-x p") 'amirreza/project-hydra/body)

(setq amirreza/programming-hydra-heads '())

(add-to-list 'amirreza/programming-hydra-heads '("n" flymake-goto-next-error "Goto Next Error"))
(add-to-list 'amirreza/programming-hydra-heads '("p" flymake-goto-previous-error "Goto Previous Error"))
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

(setq amirreza/--eldoc-window-open 'close)

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
			    (define-key prog-mode-map (kbd "C-c m") 'amirreza/programming-hydra/body)
			    (define-key prog-mode-map (kbd "C-x m") 'amirreza/programming-hydra/body)))


(setq rustic-lsp-client 'eglot) ;; Rustic default is lsp-mode

(amirreza/defhydra amirreza/go-hydra
		   (:exit t)
		   (append amirreza/programming-hydra-heads '(("a" go-tag-add "Add Struct Tag"))))
  
(add-hook 'go-mode-hook (lambda ()
			  (define-key go-mode-map (kbd "C-x m") 'amirreza/go-hydra/body)
			  (define-key go-mode-map (kbd "C-c m") 'amirreza/go-hydra/body)))

(setq amirreza/json-hydra-heads '(
				  ("f" json-pretty-print "Pretty print region")
				  ("F" json-pretty-print-buffer "Pretty print buffer")
				  ))
(amirreza/defhydra amirreza/json-hydra (:exit t) amirreza/json-hydra-heads)

(add-hook 'json-mode-hook (lambda ()
			    (define-key (kbd "C-x m") 'amirreza/json-hydra/body)
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
