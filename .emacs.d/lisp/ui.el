(setq amirreza/font "FiraCode Nerd Font Mono")
;; (setq amirreza/font "OperatorMono Nerd Font")
;; (setq amirreza/font "JetBrainsMono Nerd Font Mono")

(setq amirreza/font-size "18")

(defun amirreza/home-monitor ()
  (interactive)
  (setq amirreza/font-size "23")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(defun amirreza/laptop ()
  (interactive)
  (setq amirreza/font-size "19")
  (set-frame-font (concat amirreza/font " " amirreza/font-size) nil t))

(use-package ef-themes)
(use-package doom-themes)
(use-package gruber-darker-theme)


;; (setq amirreza/dark-theme 'doom-one)
(setq amirreza/dark-theme 'doom-dracula)
(setq amirreza/light-theme 'doom-one-light)

;; (setq amirreza/dark-theme 'modus-vivendi)
;; (setq amirreza/light-theme 'modus-operandi)

(setq amirreza/--color-mode 'dark)

(defun amirreza/load-theme ()
  (if (eq amirreza/--color-mode 'dark)
      (load-theme amirreza/dark-theme t)
    (load-theme amirreza/light-theme t)
      )
  )

(defun amirreza/toggle-color ()
  (interactive)
  (if (eq amirreza/--color-mode 'dark)
      (setq amirreza/--color-mode 'light)
    (setq amirreza/--color-mode 'dark)
    )
  (amirreza/load-theme)
  )

(global-set-key (kbd "<f12>") 'amirreza/toggle-color)

(amirreza/load-theme)

;; (amirreza/laptop)
(amirreza/home-monitor)

(provide 'ui)
