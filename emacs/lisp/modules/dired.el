;;; dired.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AmirrezaAskarpour

;; Author: AmirrezaAskarpour <raskarpour@gmail.com>
;; Keywords:


(pkg! dired
      :commands (dired dired-jump)
      :hook (dired-hook . (lambda () (interactive) (dired-hide-details-mode 1)))
      :bind (("C-x C-j" . dired-jump)
             :map dired-mode-map
             ("q" . kill-this-buffer)))
(pkg! dired-x
      :config
      (setq cmd "xdg-open")
      (setq dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "vlc")
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

(pkg! dired-sidebar :straight t
      :bind
      (("<f8>" . dired-sidebar-toggle-sidebar)))

(pkg! dired-subtree
      :straight t
      :bind (:map dired-mode-map
                  ("<tab>" . dired-subtree-toggle)))


(provide 'modules/dired)
