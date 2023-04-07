#!/bin/bash

STATUS=$(dunstctl is-paused)

if [[ $STATUS == "false" ]]; then
    ICON=""
elif [[ $STATUS == "true" ]]; then
    ICON=""
fi

JSON_STRING=$( jq -n \
                  --arg a "$ICON" \
                  '{text: $a}' )
echo $JSON_STRING
