#!/bin/bash

if pgrep -x "alacritty" > /dev/null; then
  echo 'Alacritty Is Running'
else
 alacritty &
fi


