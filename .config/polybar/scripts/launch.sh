#!/bin/bash

killall -q polybar;                                  # Terminate Already Running Bar Instances

# Launch

polybar mainbar-$(echo $DESKTOP_SESSION) --config=/home/arbab/.config/polybar/config.ini &
echo "Polybar For $(echo $DESKTOP_SESSION) Has Been Launched"
