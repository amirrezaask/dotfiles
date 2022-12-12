(defun amirreza/up-center ()
  (interactive)
  (previous-line 20)
  (recenter-top-bottom)
  )

(defun amirreza/down-center ()
  (interactive)
  (next-line 20)
  (recenter-top-bottom)

  )

;; Best movement ever ?????
(setq recenter-positions '(middle))
(global-set-key (kbd "M-p") (lambda () (interactive) (amirreza/up-center)))
(global-set-key (kbd "M-n") (lambda () (interactive) (amirreza/down-center)))


(provide 'navigation)
