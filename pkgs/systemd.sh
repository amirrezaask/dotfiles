#!/usr/bin/env bash

sudo tee /etc/systemd/logind.conf > /dev/null <<'EOF'
[Login]
HandlePowerKey=poweroff
HandleSuspendKey=ignore
HandleHibernateKey=ignore
HandleLidSwitch=suspend
HandleLidSwitchExternalPower=suspend
HandleLidSwitchDocked=suspend
KillUserProcesses=no
IdleAction=ignore
EOF


sudo tee /etc/systemd/system/lock-before-suspend.service> /dev/null <<'EOF'
[Unit]
Description=Lock screen before suspend
Before=sleep.target
StopWhenUnneeded=true

[Service]
Type=oneshot
ExecStart=swaylock
RemainAfterExit=yes

[Install]
WantedBy=sleep.target
EOF

sudo systemctl enable lock-before-suspend.service
sudo systemctl daemon-reload
