(setq packages '( ((mac aarch64 rust-analyzer) . 1) ) )


(cdr (assoc '(mac aarch64 rust-analyzer) packages))

(defun installer-os ()
  'mac
  )

(defun installer-arch ()
  'aarch64
  )

(defun installer-add-installer (for callback)
  ()
  )

(defun installer-install (package-name)
  (cdr (assoc (list (installer-os) (installer-arch) package-name) packages))
  )


(installer-install 'rust-analyzer)



(provide 'installer)
