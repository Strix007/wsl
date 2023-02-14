#!/bin/bash

killall -q polybar; # Terminate Already Running Bar Instances
FALLBACKWM="xmonad" # Set fallback window manager

# Launch

sleep 30 # Give time for the WM to set everything up
if [ -z "$DESKTOP_SESSION" ] # Check for some reason if $DESKTOP_SESSION is empty and if so, launch polybar with a fallback window manager
then
    polybar mainbar-$(echo $FALLBACKWM) --config=/home/arbab/.config/polybar/config.ini &
    echo "Polybar For $(echo $FALLBACKWM) Has Been Launched"
else
    polybar mainbar-$(echo $DESKTOP_SESSION) --config=/home/arbab/.config/polybar/config.ini &
    echo "Polybar For $(echo $DESKTOP_SESSION) Has Been Launched"
fi


