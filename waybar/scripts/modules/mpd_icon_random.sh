#!/bin/bash

STATUS=$(mpc status)
if [[ -z $STATUS ]]; then
        ICON=""
elif [[ $STATUS == *paused* ]]; then
        ICON=""
elif [[ $STATUS == *playing* ]] ; then
    MPD_STATUS_RANDOM=$(mpc status | sed -n "3 p" | awk '{print $5}')
    if [[ $MPD_STATUS_RANDOM == "off" ]]; then
        ICON=""
        CLASS="random-off"
    elif [[ $MPD_STATUS_RANDOM == "on" ]]; then
        ICON=""
        CLASS="random-on"
    fi
else
    ICON=""
fi
JSON_STRING=$( jq -n \
                  --arg a "$ICON" \
                  --arg b "$CLASS" \
                  '{text: $a, class: $b}' )
echo $JSON_STRING
