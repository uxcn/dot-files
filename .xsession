#!/bin/sh

# background
/usr/bin/xsri --color=\#373737

# compositor
compton -b

# start xscreensaver
xscreensaver -no-splash &

#start urxvtd
/usr/bin/urxvtd -q -f -o

# start synergy client
#/usr/bin/synergys

# start pulseaudio
/usr/bin/start-pulseaudio-x11 --start --log-target=syslog

/usr/bin/xmonad

# stop mpd
/usr/bin/mpd --kill

# stop pulseaudio
/usr/bin/pulseaudio --kill

# stop xscreensaver
/usr/bin/xscreensaver-command -exit
