#!/bin/bash

# Playerctl Daemon For Behavior Based On Player In Order Of Their Last Activity

playerctld daemon &

# Wallpaper Uitlity (Currently: Nitrogen)

nitrogen --restore &

# Rclone

rclone mount --daemon Drive_arbabashruff: ~/Mount/arbabashruff@gmail.com/ &

# Xfce Power Manager

xfce4-power-manager &

# Session Utility

lxsession &

# Pulseaudio Volume Manager In SysTray

volumeicon &

# WLAN SysTray Utility Using NetworkManager (Currently: NM-Applet)

nm-applet &

# SysTray App For KdeConnect

kdeconnect-indicator &

# Dunst Startup Script

/home/arbab/.config/dunst/load.sh
