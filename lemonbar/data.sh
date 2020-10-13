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

while true; do
        output=""
        output="${output} $(right) $(center)"
        echo $output
        sleep 1
done
