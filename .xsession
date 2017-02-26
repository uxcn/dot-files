#!/bin/zsh -li

if test -e /dev/dri/card0 &&
   test -e /dev/dri/card1 &&
   test -e /dev/dri/card2; then

    /usr/bin/xrandr --setprovideroutputsource 1 0
    /usr/bin/xrandr --output DVI-1-0 --auto --output DP2 --auto --left-of DVI-1-0 --output LVDS1 --auto --left-of DP2

else

    /usr/bin/xrandr --output LVDS1 --auto

fi


# background
hsetroot -solid \#373737

# compositor
compton -b

# start xscreensaver
xscreensaver -no-splash &

# start urxvtd
urxvtd -q -f -o

# start synergy server
synergys

# start pulseaudio
start-pulseaudio-x11 --start --log-target=syslog

xmonad

# stop synergy server
pkill synergys

# stop mpd
mpd --kill

# stop pulseaudio
pulseaudio --kill

# stop xscreensaver
xscreensaver-command -exit
