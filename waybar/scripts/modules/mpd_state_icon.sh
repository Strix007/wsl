
#!/bin/bash
STATUS=$(mpc status)
if [[ -z $STATUS ]]; then
    ICON=""
elif [[ $STATUS == *paused* ]]; then
    ICON="| 󰎊"
elif [[ $STATUS == *playing* ]]; then
    ICON="| 󰎈"
else
    ICON=""
fi
JSON_STRING=$( jq -n \
                  --arg a "$ICON" \
                  '{text: $a}' )
echo $JSON_STRING
