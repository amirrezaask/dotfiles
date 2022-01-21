#!/usr/bin/bash

Monitors=$(xrandr | grep -o "^.* connected" | sed "s/ connected//")
clock() {
        DATETIME=$(date "+%a %b %d, %T")

        echo -n "$DATETIME"
}

battery() {
        BATPERC=$(acpi --battery | cut -d, -f2)
        echo "$BATPERC"
}
Sound(){
	NOTMUTED=$( amixer sget Master | grep "\[on\]" )
	if [[ ! -z $NOTMUTED ]] ; then
		VOL=$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master) | sed 's/%//g')
		if [ $VOL -ge 85 ] ; then
			echo -e "\uf028 ${VOL}%"
		elif [ $VOL -ge 50 ] ; then
			echo -e "\uf027 ${VOL}%"
		else
			echo -e "\uf026 ${VOL}%"
		fi
	else
		echo -e "\uf026 M"
	fi
}


rhythmbox_song_name() {
    echo "$(rhythmbox-client --no-start --print-playing-format %tt)"
}

get_desktops_for() {
    echo "$(bspc query --monitor ${1} -D --names)"
}

# left() {
# }

center() {
    output=""
    output="${output} %{c} $(rhythmbox_song_name)"
    echo $output
}

right() {
    output=""
    output="${output} %{r} $(Sound) | $(clock) | $(battery)"
    echo $output
}
for_all_monitors(){
    final_output=""
    tmp=0
    for m in $(echo "$Monitors"); do
        output="%{l} $(get_desktops_for ${m}) $(center) $(right)"
        final_output="%{S${tmp}} ${output} ${final_output}" 
        let tmp=$tmp+1
    done
    echo $final_output
}

while true; do
    output=$(for_all_monitors);
    echo $output
    sleep 5 
done
