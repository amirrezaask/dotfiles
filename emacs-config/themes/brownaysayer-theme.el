(deftheme brownaysayer)
(custom-theme-set-faces
 'brownaysayer
 `(default                          ((t (:foreground "#debe95" :background "#161616"))))
 `(hl-line                          ((t (:background "#252525"))))
 `(vertico-current                  ((t (:inherit hl-line))))
 `(region                           ((t (:background  "medium blue"))))
 `(cursor                           ((t (:background "lightgreen"))))
 `(font-lock-keyword-face           ((t (:foreground "#d4d4d4"))))
 `(font-lock-type-face              ((t (:foreground "#8cde94"))))
 `(font-lock-constant-face          ((t (:foreground "#7ad0c6"))))
 `(font-lock-variable-name-face     ((t (:foreground "#c8d4ec"))))
 `(font-lock-builtin-face           ((t (:foreground "white"))))
 `(font-lock-string-face            ((t (:foreground "gray70"))))
 `(font-lock-comment-face           ((t (:foreground "#3fdf1f"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#3fdf1f"))))
 `(font-lock-doc-face               ((t (:foreground "#3fdf1f"))))
 `(font-lock-function-name-face     ((t (:foreground "white"))))
 `(font-lock-doc-string-face        ((t (:foreground "#3fdf1f"))))
 `(font-lock-warning-face           ((t (:foreground "yellow"))))
 `(font-lock-note-face              ((t (:foreground "khaki2" ))))
 `(mode-line                        ((t (:foreground "black" :background "#d3b58d"))))
 `(mode-line-inactive               ((t (:background "gray20" :foreground "#ffffff"))))
 `(show-paren-match                 ((t (:background "mediumseagreen")))))


(provide 'brownaysayer)
