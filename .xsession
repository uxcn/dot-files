#!/bin/zsh -l

if test -e ~/.Xmodmap; then

    xmodmap ~/.Xmodmap

fi


# background
hsetroot -solid \#262626

# compositor
compton -b

# start xscreensaver
xscreensaver &

# start synergy server
# synergys

# start pulseaudio
start-pulseaudio-x11

pactl load-module module-dbus-protocol &>/dev/null # FIXME
pactl load-module module-equalizer-sink &>/dev/null # FIXME

xmonad

# stop synergy server
# pkill synergys

# stop mpd
mpd --kill

# stop pulseaudio
pulseaudio --kill

# stop xscreensaver
xscreensaver-command -exit
