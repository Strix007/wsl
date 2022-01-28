#!/bin/bash

if pgrep -x "spotify" > /dev/null; then
  echo 'Spotify Is Running'
else
  spotify &
fi

