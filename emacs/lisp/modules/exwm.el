(if-enabled? exwm
  (pkg! exwm
    :straight t
    :config
    (require 'exwm)
    (require 'exwm-config)
    (require 'exwm-systemtray)

    (defun amirreza/exwm-lock ()
      "lock using 'slock'"
      (interactive)
      (start-process "" nil "/usr/bin/slock"))

    (defun amirreza/application-launcher (command)
      "Acts as a dmenu replacement."
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))

    (defun amirreza/exwm-current-workspace ()
      "Show index of current workspace."
      (interactive)
      exwm-workspace-current-index)

    (setq exwm-workspace-number 10)
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-class-name))))

    (add-hook 'exwm-update-title-hook
              (lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-title))))
    (setq exwm-input-global-keys
          `(
            ;; Bind "s-r" to exit char-mode and fullscreen mode.
            ([?\s-r] . exwm-reset)
            ([?\s-g] . keyboard-quit)
            ;; Bind "s-w" to switch workspace interactively.
            ([?\s-w] . exwm-workspace-switch)

            ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ;; Bind "s-&" to launch applications ('M-&' also works if the output
            ;; buffer does not bother you).
            ([?\s-d] . amirreza/application-launcher)
            ;; Bind "s-l" to "slock", a simple X display locker.
            ([?\s-l] . amirreza/exwm-lock)
            (,(kbd "<XF86AudioRaiseVolume>") . (lambda (
                                                             (interactive)
                                                             (start-process-shell-command "RaiseVolume" nil "pactl set-sink-volume @DEFAULT_SINK@ +10%"))))

            (,(kbd "<XF86AudioLowerVolume>") . (lambda (
                                                             (interactive)
                                                             (start-process-shell-command "DownVolume" nil "pactl set-sink-volume @DEFAULT_SINK@ -10%"))))

            (,(kbd "<XF86AudioMute>") . (lambda (
                                                      (interactive)
                                                      (start-process-shell-command "MuteVolume" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle"))))

            (,(kbd "<XF86AudioMicMute>") . (lambda (
                                                         (interactive)
                                                         (start-process-shell-command "MuteMicVolume" nil "pactl set-source-mute @DEFAULT_SOURCE@ toggle"))))))


            


    (setq exwm-input-simulation-keys
          '(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])
            ;; cut/paste.
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ;; search
            ([?\C-s] . [?\C-f])))


    (require 'exwm-randr)

    ;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1"
    ;;                                             1 "HDMI-1"
    ;;                                             2 "HDMI-1"
    ;;                                             3 "HDMI-1"
    ;;                                             4 "HDMI-1"
    ;;                                             5 "HDMI-1"
    ;;                                             6 "eDP-1"
    ;;                                             7 "HDMI-1"
    ;;                                             8 "HDMI-1"
    ;;                                             9 "HDMI-1"))
    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
                (start-process-shell-command
                 "xrandr" nil "xrandr --output HDMI-1 --above eDP-1 --mode 1920x1080")))

    (global-set-key (kbd "<XF86AudioRaiseVolume>") (lambda ())
                                                   (interactive)
                                                   (start-process-shell-command "RaiseVolume" nil "pactl set-sink-volume @DEFAULT_SINK@ +10%"))

    (global-set-key (kbd "<XF86AudioLowerVolume>") (lambda ())
                                                   (interactive)
                                                   (start-process-shell-command "DownVolume" nil "pactl set-sink-volume @DEFAULT_SINK@ -10%"))

    (global-set-key (kbd "<XF86AudioMute>") (lambda ()
                                                   (interactive)
                                                   (start-process-shell-command "MuteVolume" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle")))

    (global-set-key (kbd "<XF86AudioMicMute>") (lambda ()
                                                   (interactive)
                                                   (start-process-shell-command "MuteMicVolume" nil "pactl set-source-mute @DEFAULT_SOURCE@ toggle")))
    (exwm-randr-enable)
    (start-process-shell-command "Set keyboard layout" nil "setxkbmap -layout 'us,ir' -option 'grp:shifts_toggle' -option 'ctrl:nocaps'")
    (exwm-systemtray-enable)
    (exwm-enable)))
