#!/bin/bash

while true
do
    PROGRESS="$(progress 2>/dev/null | sed -n "2 p" | awk '{print $1}')"
    if [[ -z $PROGRESS ]]; then
        echo ""
    else
        echo " $PROGRESS"
    fi
done
