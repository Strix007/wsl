#!/bin/bash
ICON=$HOME/i3lock/lock.png
TMPBG=/tmp/lock-state.png
scrot /tmp/lock-state.png
convert $TMPBG -scale 10% -scale 1000% $TMPBG
# convert $TMPBG $ICON -gravity Center -composite -matte $TMPBG
STATUS=$(playerctl status)
playerctl -a pause
if [ "$STATUS" == "Paused" ]; then
    i3lock -u -i $TMPBG
else
    i3lock -u -i $TMPBG -n; playerctl play
fi
rm /tmp/lock-state.png
