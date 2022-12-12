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

;; (setq amirreza/theme 'doom-outrun-electric)
;; (setq amirreza/theme 'doom-one)
;; (setq amirreza/theme 'doom-xcode)

(setq amirreza/dark-theme 'modus-vivendi)
(setq amirreza/light-theme 'modus-operandi)

(setq amirreza/--color-mode 'dark)

(defun amirreza/load-theme ()
  (if (eq amirreza/--color-mode 'dark)
      (load-theme amirreza/dark-theme)
    (load-theme amirreza/light-theme)
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
