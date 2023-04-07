#!/bin/bash
getstorage_used() {
    df -H $1 | awk '{print $3}' | tail -n 1 | sed 's/G//' | cut -f1 -d"."
}
getstorage_free() {
    df -H $1 | awk '{print $4}' | tail -n 1 | sed 's/G//' | cut -f1 -d"."
}

USED_ROOT=$(getstorage_used /)
USED_HOME=$(getstorage_used /home)
USED_NVME=$(($USED_ROOT+ $USED_HOME))
FREE_ROOT=$(getstorage_free /)
FREE_HOME=$(getstorage_free /home)
FREE_NVME=$(($FREE_ROOT+ $FREE_HOME))
FORMAT="${USED_NVME}GB"
tooltip="Free: ${FREE_NVME}GB"

JSON_STRING=$( jq -n \
                  --arg a "$FORMAT" \
                  --arg b "$tooltip" \
                  '{text: $a, tooltip: $b}' )

echo $JSON_STRING
