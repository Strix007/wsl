#!/usr/bin/env sh

killall -q polybar                                  # Terminate Already Running Bar Instances
while pgrep -x polybar >/dev/null; do sleep 1; done # Wait Until The Processes Have Been Shutdown

# Launch

polybar mainbar-$(wmctrl -m | grep Name: | awk '{print $2}') --config=/home/arbab/.config/polybar/config.ini &
echo "Polybar For $(wmctrl -m | grep Name: | awk '{print $2}') Has Been Launched"