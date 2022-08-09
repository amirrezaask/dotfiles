;; loki-mode.el - very basic loki mode

(require 'cl)
(require 'rx)
(require 'js)
(require 'compile)

(defconst loki-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst loki-builtins
  '("cast" "it" "type_info" "size_of"))

(defconst loki-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "new" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "SOA"))

(defconst loki-constants
  '("null" "true" "false"))

(defconst loki-typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "bool"))

(defun loki-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun loki-keywords-rx (keywords)
  "build keyword regexp"
  (loki-wrap-word-rx (regexp-opt keywords t)))

(defconst loki-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst loki-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst loki-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst loki-font-lock-defaults
  `(
    ;; Keywords
    (,(loki-keywords-rx loki-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(loki-keywords-rx loki-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(loki-keywords-rx loki-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(loki-wrap-word-rx loki-number-rx) . font-lock-constant-face)

    ;; Types
    (,(loki-keywords-rx loki-typenames) 1 font-lock-type-face)
    (,loki-hat-type-rx 1 font-lock-type-face)
    (,loki-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst loki--defun-rx "\(.*\).*\{")

(defmacro loki-paren-level ()
  `(car (syntax-ppss)))

(defun loki-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at loki--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun loki-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (loki-paren-level)))
    (while (and
            (not (loki-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (loki-paren-level))
      (while (>= (loki-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (loki-line-is-defun)
      (beginning-of-line)))

(defun loki-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (loki-paren-level)))
    (when (> orig-level 0)
      (loki-beginning-of-defun)
      (end-of-line)
      (setq orig-level (loki-paren-level))
      (skip-chars-forward "^}")
      (while (>= (loki-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'loki-parent-mode
 (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; imenu hookup
(add-hook 'loki-mode-hook
      (lambda ()
        (setq imenu-generic-expression
          '(
            ("type" "^\\(.*:*.*\\) : " 1)
	    ("function" "^\\(.*\\) :: " 1)
	    ("struct" "^\\(.*\\) *:: *\\(struct\\)\\(.*\\){" 1)
	    )
        )
      )
)

;; NOTE: taken from the scala-indent package and modified for Loki.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun loki--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun loki--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'loki--indent-on-parentheses)
  )

;;;###autoload
(define-derived-mode loki-mode loki-parent-mode "Loki"
 :syntax-table loki-mode-syntax-table
 :group 'loki
 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 (setq-local comment-start "/*")
 (setq-local comment-end "*/")
 (setq-local indent-line-function 'js-indent-line)
 (setq-local font-lock-defaults '(loki-font-lock-defaults))
 (setq-local beginning-of-defun-function 'loki-beginning-of-defun)
 (setq-local end-of-defun-function 'loki-end-of-defun)

 ;; add indent functionality to some characters
 (loki--add-self-insert-hooks)

 (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.loki\\'" . loki-mode))

(defconst loki--error-regexp
  "^\\([^ :]+\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(loki ,loki--error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'loki compilation-error-regexp-alist)

(provide 'loki-mode)
