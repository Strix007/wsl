#!/bin/bash

USE_FORMAT_ALT=false
DIR="$HOME/.config/rofi/spotify"

# Function for easier player actions
player_action (){
    playerctl --player=spotify $1
}

# Get the status of the player
STATUS=$(player_action status)

# Function for metadata
getmeta (){
    playerctl --player=spotify metadata --format "{{ $1 }}"
}
# Function for track duration
getduration (){
    playerctl metadata --format "{{ duration($1) }}"
}

# Variables for format
ARTIST=$(getmeta artist)
TITLE=$(getmeta title)
ALBUM=$(getmeta album)
LENGTH=$(getduration mpris:length)
POSITION=$(getduration position)
FORMAT="$ARTIST - $ALBUM"
DURATION="$POSITION/$LENGTH"

# Icons for format
# Different icons for play pause states
if [[ $STATUS == "Playing" ]]; then
    ICON_TOGGLE=""
elif [[ $STATUS == "Paused" ]]; then
    ICON_TOGGLE=""
else
    ICON_TOGGLE=""
fi
ICON_STOP=""
ICON_PREV=""
ICON_NEXT=""
ICON_BACKWARD=""
ICON_FORWARD=""
ICON_SHUFFLE="󰒝"
ICON_REPEAT="󰑖"
# Order of icons
if [ $USE_FORMAT_ALT = true ]; then
    ICON_FORMAT="$ICON_TOGGLE\n$ICON_STOP\n$ICON_BACKWARD\n$ICON_FORWARD\n$ICON_PREV\n$ICON_NEXT\n$ICON_SHUFFLE\n$ICON_REPEAT"
    COLUMNS="8"
    LINES="1"
    ACTIVE_SHUFFLE_POS="6"
    URGENT_SHUFFLE_POS="6"
    ACTIVE_REPEAT_POS="7"
    URGENT_REPEAT_POS="7"
    THEME="spotify-alt"
elif [ $USE_FORMAT_ALT = false ]; then
    ICON_FORMAT="$ICON_TOGGLE\n$ICON_STOP\n$ICON_PREV\n$ICON_NEXT\n$ICON_SHUFFLE\n$ICON_REPEAT"
    COLUMNS="6"
    LINES="1"
    ACTIVE_SHUFFLE_POS="4"
    URGENT_SHUFFLE_POS="4"
    ACTIVE_REPEAT_POS="5"
    URGENT_REPEAT_POS="5"
    THEME="spotify"
fi


# Check for shuffle
SHUFFLE_STATUS=$(playerctl --player=spotify shuffle)
if [[ $SHUFFLE_STATUS == "On" ]]; then
    active="-a $ACTIVE_SHUFFLE_POS"
elif [[ $SHUFFLE_STATUS == "Off" ]]; then
    urgent="-u $URGENT_SHUFFLE_POS"
else
    echo "Unexpected shuffle case"
fi

# Check for repeat
REPEAT_STATUS=$(playerctl --player=spotify loop)
if [[ $REPEAT_STATUS == "Track" || $REPEAT_STATUS == "Playlist" ]]; then
    [ -n "$active" ] && active+=",$ACTIVE_REPEAT_POS" || active="-a $ACTIVE_REPEAT_POS"
elif [[ $REPEAT_STATUS == "None" ]]; then
    [ -n "$urgent" ] && urgent+=",$URGENT_REPEAT_POS" || urgent="-u $URGENT_REPEAT_POS"
else
    echo "Unexpected loop check case"
fi

# Run rofi function
# Run function when spotify is playing
rofi_run_playing (){
    printf $1 | rofi -dmenu \
                     -p "$FORMAT" \
                     -theme-str "entry { enabled: false;}" \
                     -theme-str 'textbox-prompt-colon { str: "󰎈";}' \
                     -theme-str 'textbox-prompt-colon { background-color: @active;}' \
                     -theme-str "listview {columns: $COLUMNS; lines: $LINES;}" \
                     -mesg "$TITLE :: $DURATION" \
		                 ${active} ${urgent} \
		                 -theme "$DIR/$THEME.rasi"
}
# Run function when spotify is paused
rofi_run_paused (){
    printf $1 | rofi -dmenu \
                     -p "$FORMAT" \
                     -theme-str "entry { enabled: false;}" \
                     -theme-str 'textbox-prompt-colon { str: "󰎊";}' \
                     -theme-str 'textbox-prompt-colon { background-color: @urgent;}' \
                     -theme-str 'prompt { background-color: @urgent;}' \
                     -theme-str "listview {columns: $COLUMNS; lines: $LINES;}" \
                     -mesg "$TITLE :: $DURATION" \
		                 ${active} ${urgent} \
		                 -theme "$DIR/$THEME.rasi"
}

# Icons for when player is not started
ICON_YES=""
ICON_NO=""
NO_PLAYER_RUNNING_FORMAT="$ICON_YES\n$ICON_NO"

# No player running
confirm_cmd() {
	  printf $1 | rofi -dmenu \
		                 -p 'Confirmation' \
		                 -mesg 'Spotify Is Not Currently Active. Would You Like To Open Spotify?' \
		                 -theme "$HOME/.config/rofi/spotify/confirm.rasi"
}

# Evaluate output
if [[ $STATUS == "Playing" ]]; then
    OUTPUT=$(rofi_run_playing $ICON_FORMAT)
elif [[ $STATUS == "Paused" ]]; then
    OUTPUT=$(rofi_run_paused $ICON_FORMAT)
else
    OUTPUT=$(confirm_cmd $NO_PLAYER_RUNNING_FORMAT)
fi
if [[ $OUTPUT == $ICON_TOGGLE ]]; then
    player_action play-pause
elif [[ $OUTPUT == $ICON_BACKWARD ]]; then
    player_action "position 10-"
elif [[ $OUTPUT == $ICON_FORWARD ]];then
     player_action "position 10+"
elif [[ $OUTPUT == $ICON_PREV ]]; then
    player_action previous
elif [[ $OUTPUT == $ICON_NEXT ]]; then
    player_action next
elif [[ $OUTPUT == $ICON_REPEAT ]]; then
    LOOP_STATUS=$(player_action loop)
    if [[ $LOOP_STATUS == "Track" || $LOOP_STATUS == "Playlist" ]]; then
        player_action "loop none"
    elif [[ $LOOP_STATUS == "None" ]]; then
        player_action "loop Track"
    else
        echo "Unexpected loop case"
    fi
elif [[ $OUTPUT == $ICON_SHUFFLE ]]; then
    player_action "shuffle toggle"
elif [[ $OUTPUT == $ICON_STOP ]]; then
    player_action stop
elif [[ $OUTPUT == $ICON_YES ]]; then
    spotify
elif [[ $OUTPUT == $ICON_NO ]]; then
    echo "Script exited"
    exit
else
    echo "Unexpected input"
fi
