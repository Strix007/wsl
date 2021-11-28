#!/bin/bash

ICON=/home/arbab/.config/i3lock/lock.png
TMPBG=/tmp/lock-state.png
scrot /tmp/lock-state.png
convert $TMPBG -scale 10% -scale 1000% $TMPBG
convert $TMPBG $ICON -gravity center -composite -matte $TMPBG
playerctl -a pause
i3lock -u -i $TMPBG -n; playerctl play
rm /tmp/lock-state.png