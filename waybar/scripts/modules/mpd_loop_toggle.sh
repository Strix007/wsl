#!/bin/bash
MPD_STATUS_REPEAT=$(mpc status | sed -n "3 p" | awk '{print $3}')
MPD_STATUS_SINGLE=$(mpc status | sed -n "3 p" | awk '{print $7}')
if [[ $MPD_STATUS_REPEAT == "off" || $MPD_STATUS_SINGLE == "off" ]]; then
    mpc -q single on && mpc -q repeat on
elif [[ $MPD_STATUS_REPEAT == "on" || $MPD_STATUS_SINGLE == "on" ]]; then
    mpc -q single off && mpc -q repeat off
fi
