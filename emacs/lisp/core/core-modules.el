(defvar amirreza/modules-path "~/.emacs.d/lisp/modules")

(defun enabled? (mod)
  (not (null (member mod modules!))))

(defmacro if-enabled? (mod &rest body)
  `(when (enabled? (quote ,mod))
     ,@body))

(defun enable! (&rest modules)
  (setq modules! (append modules modules!)))


(defun load-module (name)
  (cond
    ((symbolp name) (load-file (expand-file-name (concat (symbol-name name) ".el") amirreza/modules-path)))
    ((listp name) (mapc
                   (lambda (attr)
                     (load-file (expand-file-name (concat (symbol-name attr) ".el") (concat amirreza/modules-path "/" (symbol-name (car name))))))
                   (cdr name)))
   ))

(defun load-modules ()
  (mapcar (lambda (mod) (load-module mod)) modules!))


(provide 'core/core-modules)
