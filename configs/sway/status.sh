while true; do
	wifi="WIFI :: $(nmcli -t -f active,ssid dev wifi | grep '^yes' | cut -d: -f2)"
	wifi=${wifi:-"No WiFi"}

	bat=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null | head -n1)
	bat="BAT :: ${bat:-N/A}%"

	bt=$(bluetoothctl show | grep "Powered:" | awk '{print $2}')
	if [ "$bt" = "yes" ]; then
		bt="BT:: On"
	else
		bt="BT :: Off"
	fi

	ram=$(free -h | awk '/Mem:/ {print $3 "/" $2}')
	ram="RAM :: $ram"

	date=$(date '+%Y-%m-%d %H:%M')

	echo "$wifi | $bat | $bt | $ram | $date"
	sleep 2
done
