#!/bin/bash

mySpinners=("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")

while true
do
    PROGRESS="$(progress 2>/dev/null | sed -n "2 p" | awk '{print $1}')"
    if [[ -z "$PROGRESS" ]]; then
        echo ""
    else
        for i in "${mySpinners[@]}"
        do
            sleep 0.1
            echo "$i"
        done
    fi
done
