#!/usr/bin/env bash

APP_NAME=$1
APP_URL=$2

DESKTOP_FILE="$HOME/.local/share/applications/$APP_NAME.desktop"

cat >"$DESKTOP_FILE" <<EOF
[Desktop Entry]
Version=1.0
Name=$APP_NAME
Comment=$APP_NAME
Exec=google-chrome --new-window --ozone-platform=wayland --app="$APP_URL" --name="$APP_NAME" --class="$APP_NAME"
Terminal=false
Type=Application
StartupNotify=true
EOF


