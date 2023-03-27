#!/bin/bash

killall -q polybar; # Terminate Already Running Bar Instances
FALLBACKWM="xmonad" # Set fallback window manager

# Launch

if [ -z "$DESKTOP_SESSION" ] # Check for some reason if $DESKTOP_SESSION is empty and if so, launch polybar with a fallback window manager
then
    polybar mainbar-$FALLBACKWM --config=$HOME/.config/polybar/config.ini &
    echo "Polybar For echo $FALLBACKWM Has Been Launched"
else
    polybar mainbar-$DESKTOP_SESSION --config=$HOME/.config/polybar/config.ini &
    echo "Polybar For $DESKTOP_SESSION Has Been Launched"
fi


