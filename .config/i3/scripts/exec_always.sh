#!/bin/bash

# Compositing With Picom (Fork: Joanaburg)

picom --experimental-backend &

# Polybar Launch Script

$HOME/.config/polybar/scripts/launch.sh

# Implement Dwindle

python $HOME/.config/i3/scripts/Tile_Dwindle.py
