#!/bin/bash
STATUS=$(mpc status)
if [[ -z $STATUS ]]; then
    TITLE=""
    ALBUM=""
elif [[ $STATUS == *paused* ]]; then
    TITLE=$(mpc current)
    ALBUM=$(mpc -f "%album%" | sed -n '1 p')
elif [[ $STATUS == *playing* ]]; then
    TITLE=$(mpc current)
    ALBUM=$(mpc -f "%album%" | sed -n '1 p')
else
    TITLE=""
    ALBUM=""
fi
tooltip="$TITLE - $ALBUM"
JSON_STRING=$( jq -n \
                  --arg a "$TITLE" \
                  --arg b "$tooltip" \
                  '{text: $a, tooltip: $b}' )
echo $JSON_STRING
