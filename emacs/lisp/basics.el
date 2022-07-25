(setq backup-by-copying t) ;; Always copy files for backup.
(setq version-control t) ;; Use version numbers for backup.
(setq delete-old-versions t) ;; Delete old backup of files.
(setq kept-new-versions 6) ;; Number of newest versions to keep.
(setq kept-old-versions 2) ;; Number of old versions to keep.
(setq create-lockfiles nil) ;; Don't create .# files as lock.
(setq backup-directory-alist ;; all backups should go here (PATTERN . LOCATION)
      '(("." . "~/.config/emacs/backup/")))

(setq-default indent-tabs-mode nil ;; Don't insert tabs for indentation.
                tab-width 4) ;; Width of the TAB character in display.


(defalias 'yes-or-no-p 'y-or-n-p) ;; Show y or n instead of yes or no for question prompts.

(setq echo-keystrokes 0.1) ;; Show keystrokes in minibuffer faster than default.

(setq use-dialog-box nil) ;; Don't use any kind of GUI dialog box.

(setq inhibit-splash-screen 0) ;; Disable Emacs start screen.

(setq ring-bell-function 'ignore) ;; No bell ringing.

(set-terminal-coding-system 'utf-8) ;; default emacs encodings
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default fill-column 80) ;; column number which emacs start to line wrap.

(setq scroll-step 5) ;; When point moves out of screen, number of lines to scroll
(setq scroll-margin 5) ;; Scroll margin lines, when point arrives at these margins scroll the display.
(setq scroll-conservatively 101) ;; Number of lines to scroll to bring point back into view.
(setq scroll-up-aggressively 0.11) ;; When scrolling how much to move the view.
(setq scroll-down-aggressively 0.01) ;; Same as above.
(setq auto-window-vscroll nil) ;; Disable changing window-vscroll automatically.
(setq fast-but-imprecise-scrolling nil) ;; Disable fast scroll since it does not feel good.
(setq mouse-wheel-scroll-amount '(5
                                  ((shift) . 10)))
(setq mouse-wheel-progressive-speed t)

;; Horizontal Scroll
(setq hscroll-step 1) ;; Number of columns to scroll when point is to close to edge.
(setq hscroll-margin 1) ;; How many columns away from edge to start scrolling.

(setq custom-file "~/.config/emacs/custom.el") ;; Don't tamper with init.el for custom variables and use given file.

(column-number-mode +1) ;; Show column number in modeline.

(display-battery-mode 1) ;; Show battery in modeline.

(display-time-mode 1) ;; Show time in modeline.

(global-display-line-numbers-mode 1) ;; Ensure line numbers globally.

(setq kill-ring-max 15) ;; Capacity of kill-ring.

(show-paren-mode 1) ;; Highlight matching parens

(setq show-paren-delay 0) ;; highlight matching parens instantly.

(when (> emacs-major-version 26) (global-tab-line-mode -1)) ;; Disable tab line in Emacs 27+.

(blink-cursor-mode 1) ;; Cursor blinks.


(provide 'basics)
