(defun enabled? (mod)
  (not (null (member mod amirreza/modules))))

(defmacro if-enabled? (mod &rest body)
  `(when (enabled? (quote ,mod))
     ,@body))

(defun enable! (&rest modules)
  (setq amirreza/modules (append modules amirreza/modules)))

(provide 'core-modules)
