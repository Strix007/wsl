#!/bin/bash
getstorage_used() {
    df -H $1 | awk '{print $3}' | tail -n 1 | sed 's/G//' | cut -f1 -d"."
}
getstorage_free() {
    df -H $1 | awk '{print $4}' | tail -n 1 | sed 's/G//' | cut -f1 -d"."
}

USED_MEDIA=$(getstorage_used /media)
FREE_MEDIA=$(getstorage_free /media)
FORMAT="${USED_MEDIA}GB"
tooltip="Free: ${FREE_MEDIA}GB"

JSON_STRING=$( jq -n \
                  --arg a "$FORMAT" \
                  --arg b "$tooltip" \
                  '{text: $a, tooltip: $b}' )

echo $JSON_STRING
