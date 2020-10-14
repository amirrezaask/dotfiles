#!/usr/bin/bash

clock() {
        DATETIME=$(date "+%a %b %d, %T")

        echo -n "$DATETIME"
}

battery() {
        BATPERC=$(acpi --battery | cut -d, -f2)
        echo "$BATPERC"
}

volume() {
    amixer get Master | sed -n 'N;s/^.*\[\([0-9]\+%\).*$/\1/p'
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
    output="${output} %{c}$(rhythmbox_song_name)"
    echo $output
}

right() {
    output=""
    output="${output} %{r}$(clock) $(battery)"
    echo $output
}
for_all_monitors(){
    Monitors=$(xrandr | grep -o "^.* connected" | sed "s/ connected//")
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
