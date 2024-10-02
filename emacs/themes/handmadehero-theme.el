(deftheme handmadehero)

(custom-theme-set-faces
 'handmadehero
 `(default                          ((t (:foreground "burlywood2" :background "#161616"))))
 `(hl-line                          ((t (:background "midnight blue"))))
 `(vertico-current                  ((t (:background "midnight blue"))))
 `(region                           ((t (:background "medium blue"))))
 `(cursor                           ((t (:background "#40FF40"))))
 `(font-lock-keyword-face           ((t (:foreground "DarkGoldenrod2"))))
 `(font-lock-type-face              ((t (:foreground "burlywood3"))))
 `(font-lock-constant-face          ((t (:foreground "olive drab"))))
 `(font-lock-variable-name-face     ((t (:foreground "burlywood3"))))
 `(font-lock-builtin-face           ((t (:foreground "gray80"))))
 `(font-lock-string-face            ((t (:foreground "olive drab"))))
 `(font-lock-comment-face           ((t (:foreground "gray50"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "gray50"))))
 `(font-lock-doc-face               ((t (:foreground "gray50"))))
 `(font-lock-function-name-face     ((t (:foreground "burlywood2"))))
 `(font-lock-doc-string-face        ((t (:foreground "gray50"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))


(provide 'handmadehero-theme)
