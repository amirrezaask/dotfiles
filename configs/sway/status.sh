seperator="|"
while true; do
	wifi="WIFI: $(nmcli -t -f active,ssid dev wifi | grep '^yes' | cut -d: -f2)"
	wifi=${wifi:-"No WiFi"}

	bat=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null | head -n1)
	bat="BAT: ${bat:-N/A}%"

	bt=$(bluetoothctl show | grep "Powered:" | awk '{print $2}')
	if [ "$bt" = "yes" ]; then
		bt="BT: On"
	else
		bt="BT: Off"
	fi

	ram=$(free -h | awk '/Mem:/ {print $3 "/" $2}')
	ram="RAM: $ram"

    cpu=$(top -bn1 | grep "Cpu(s)" | awk '{print $2 + $4}')  # user + system
    cpu="CPU: ${cpu}%"

	date=$(date '+%Y-%m-%d %H:%M')

	echo "$wifi $seperator $bat $seperator $bt $seperator $ram $seperator  $cpu $seperator $date"
	sleep 2
done
