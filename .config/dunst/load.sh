#!/bin/bash

# https://github.com/thanhvule0310/dotfiles/tree/main/.config/dunst
pkill dunst
dunst -config ~/.config/dunst/dunstrc &
notify-send -a Notification -u low "Hi, Arbab " -i "/home/arbab/.config/dunst/icons/welcome.svg"
