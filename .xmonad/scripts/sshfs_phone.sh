#!/bin/bash
# For this to work, you need to install sshfs on your system, install openssh on your android and start ssh using sshd. Find out your ip by ifconfig and add it to /etc/hosts as "your_ip phone" then you need to create a folder to mount your device on, mine is $HOME/Mount/ssh_MotoG77. Finally, make a filed named .env in your $HOME directory and store your password there as a variable named SSHFS_PHONE_PASS e.g. SSHFS_PHONE_PASS='your_password’

source $HOME/.env
echo "$SSHFS_PHONE_PASS" | sshfs phone:storage $HOME/Mount/ssh_MotoG77 -o follow_symlinks,password_stdin -p 8022
