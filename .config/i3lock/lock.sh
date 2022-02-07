#!/bin/bash
ICON=/home/arbab/.config/i3lock/lock.png
TMPBG=/tmp/lock-state.png
scrot /tmp/lock-state.png
convert $TMPBG -scale 10% -scale 1000% $TMPBG
convert $TMPBG $ICON -gravity center -composite -matte $TMPBG
playerctl -a pause
if [ "$(playerctl status)" == "Paused" ]; then
		i3lock -u -i $TMPBG
    elif [ "$(playerctl status)" == "Playing" ]; then
		i3lock -u -i $TMPBG -n; playerctl play
	else 
		i3lock -u -i $TMPBG -n; playerctl play
fi
rm /tmp/lock-state.png