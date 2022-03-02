#!/usr/bin/env sh

killall -q polybar                                  # Terminate Already Running Bar Instances
while pgrep -x polybar >/dev/null; do sleep 1; done # Wait Until The Processes Have Been Shutdown

# Launch

polybar mainbar-$(echo $DESKTOP_SESSION) --config=/home/arbab/.config/polybar/config.ini &
echo "Polybar For $(echo $DESKTOP_SESSION) Has Been Launched"
