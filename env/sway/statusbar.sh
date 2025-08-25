#!/bin/bash

# Print header for the protocol
echo '{"version":1}'
echo '['
echo '[],'

while true; do
    # Get keyboard layout using swaymsg
    layout=$(swaymsg -t get_inputs | jq -r '.[] | select(.type=="keyboard") | .xkb_active_layout_name // "Unknown" | select(. != null)' | head -n1)
    if [ -z "$layout" ]; then
        layout="Unknown"
    fi

    # Get WiFi SSID using nmcli (NetworkManager CLI)
    wifi=$(nmcli -t -f active,ssid dev wifi | grep '^yes' | cut -d: -f2)
    if [ -z "$wifi" ]; then
        wifi="No WiFi"
    fi

    # Get battery status (assumes a single battery, e.g., BAT0)
    battery=$(cat /sys/class/power_supply/BAT0/capacity 2>/dev/null || echo "N/A")
    if [ "$battery" != "N/A" ]; then
        status=$(cat /sys/class/power_supply/BAT0/status 2>/dev/null)
        if [ "$status" = "Charging" ]; then
            battery="$battery% (+)"
        else
            battery="$battery%"
        fi
    fi

    # Get volume level (try pamixer, then amixer, then pactl)
    if command -v pamixer >/dev/null 2>&1; then
        volume=$(pamixer --get-volume 2>/dev/null)
        # Check mute status explicitly (pamixer --get-mute returns true if muted)
        if [ "$(pamixer --get-mute 2>/dev/null)" = "true" ]; then
            volume="Muted"
        else
            volume="$volume%"
        fi
    elif command -v amixer >/dev/null 2>&1; then
        # Look for the first valid volume control (Master, Speaker, or PCM)
        volume=$(amixer sget Master,0 2>/dev/null || amixer sget Speaker,0 2>/dev/null || amixer sget PCM,0 2>/dev/null | grep -o "[0-9]*%" | head -n1)
        if [ -n "$volume" ] && { amixer sget Master,0 2>/dev/null || amixer sget Speaker,0 2>/dev/null || amixer sget PCM,0 2>/dev/null; } | grep -q "\[off\]"; then
            volume="Muted"
        fi
    elif command -v pactl >/dev/null 2>&1; then
        # Use pactl for PulseAudio/PipeWire
        volume=$(pactl get-sink-volume @DEFAULT_SINK@ 2>/dev/null | grep -o '[0-9]*%' | head -n1)
        if pactl get-sink-mute @DEFAULT_SINK@ 2>/dev/null | grep -q "Mute: yes"; then
            volume="Muted"
        fi
    else
        volume="N/A"
    fi

    # Date and time
    datetime=$(date '+%Y-%m-%d %H:%M:%S')

    # Compose JSON array for swaybar
    echo "["
    echo "{\"full_text\":\"Layout: $layout\", \"name\":\"layout\"},"
    echo "{\"full_text\":\"WiFi: $wifi\", \"name\":\"wifi\"},"
    echo "{\"full_text\":\"Battery: $battery\", \"name\":\"battery\"},"
    echo "{\"full_text\":\"Volume: $volume\", \"name\":\"volume\"},"
    echo "{\"full_text\":\"$datetime\", \"name\":\"datetime\"}"
    echo "],"

    sleep 0.5
done
