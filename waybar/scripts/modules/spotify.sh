#!/bin/sh

# Function for metadata
getmeta (){
    playerctl --player=spotify metadata --format "{{ $1 }}"
}
# Function for track duration
getduration (){
    playerctl metadata --format "{{ duration($1) }}"
}
ARTIST=$(getmeta artist)
TITLE=$(getmeta title)
ALBUM=$(getmeta album)
LENGTH=$(getduration mpris:length)
POSITION=$(getduration position)
DURATION="$POSITION/$LENGTH"

if ! pgrep -x spotify >/dev/null; then
    echo ""; exit
fi
RESULT="$ARTIST - $TITLE"
TOOLTIP="$DURATION"

JSON_STRING=$( jq -n \
                  --arg a "$RESULT" \
                  --arg b "$TOOLTIP" \
                  '{text: $a, tooltip: $b}' )
echo $JSON_STRING
