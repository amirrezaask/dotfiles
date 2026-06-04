;;; early-init.el — package bootstrap only (loaded before init.el)

(setq package-enable-at-startup nil)

;; Defer GC until after startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 16 1024 1024))))

(setq inhibit-compacting-font-caches t)
(setq use-short-answers t)
(setq use-dialog-box nil)
