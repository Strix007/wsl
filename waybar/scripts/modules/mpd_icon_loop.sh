#!/bin/bash

STATUS=$(mpc status)
if [[ -z $STATUS ]]; then
        ICON=""
elif [[ $STATUS == *paused* ]]; then
        ICON=""
elif [[ $STATUS == *playing* ]] ; then
    MPD_STATUS_REPEAT=$(mpc status | sed -n "3 p" | awk '{print $3}')
    MPD_STATUS_SINGLE=$(mpc status | sed -n "3 p" | awk '{print $7}')
    if [[ $MPD_STATUS_REPEAT == "off" || $MPD_STATUS_SINGLE == "off" ]]; then
        ICON="󰑖"
        CLASS="loop-off"
    elif [[ $MPD_STATUS_REPEAT == "on" || $MPD_STATUS_SINGLE == "on" ]]; then
        ICON="󰑖"
        CLASS="loop-on"
    fi
else
    ICON=""
fi
JSON_STRING=$( jq -n \
                  --arg a "$ICON" \
                  --arg b "$CLASS" \
                  '{text: $a, class: $b}' )
echo $JSON_STRING
