#!/bin/sh
/usr/bin/xsri --color=\#1b1b1b

# start synergy client
/usr/bin/sudo /usr/bin/killall synergys
/usr/bin/sleep 1
/usr/bin/synergys 

/usr/bin/pulseaudio --start --log-target=syslog
/usr/bin/mpd ~/.mpd/mpd.conf 2>/dev/null

exec /usr/bin/xmonad
#exec /usr/bin/twm
