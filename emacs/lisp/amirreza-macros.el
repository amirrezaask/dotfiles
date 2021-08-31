(defun amirreza/plist-get-or-default (plist key default)
  (cond
   ((null (plist-get plist key)) (plist-get plist key))
   (t default)))

(defmacro keys! (&rest keys)
  "Bind FN to KEY in MAP as
  (keys! (:map MAP :key KEY :fn FN))
  "
  `(mapc (lambda (kf)
           (let ((keymap (amirreza/plist-get-or-default kf :map global-map))
                 (key (kbd (plist-get kf :key)))
                 (fn (plist-get kf :fn)))
             (define-key keymap key fn))) (quote ,keys)))

(defmacro hooks! (&rest hooks)
  "Add FN to HOOK as
  (hooks! (:hook HOOK :fn FN))
  "
  `(mapc (lambda (hookM)
           (let ((h (plist-get hookM :hook))
                 (f (plist-get hookM :fn)))
             (add-hook h f
                       (quote ,hooks))))))

(defmacro pkg! (pkgname &rest body)
  "(pkg! consult (message \"consult loaded\"))"
  `(progn
     (straight-use-package (quote ,pkgname))
     ,@body))

(provide 'amirreza-macros)
