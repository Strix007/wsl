#!/bin/bash

DIR="$HOME/.config/rofi/bookmarks"
THEME="bookmarks"
BOOKMARKS_FILE="$DIR/.bookmarks"
BROWSER="firefox"

# Check if there is a bookmarks file and if not, make one

if [[ ! -a "${BOOKMARKS_FILE}" ]]; then
    touch "${BOOKMARKS_FILE}"
fi

INPUT=$(cat $BOOKMARKS_FILE | rofi -dmenu -theme ${DIR}/${THEME}.rasi -p "")

if   [[ $INPUT == "+"* ]]; then
    INPUT=$(echo $INPUT | sed 's/+//') 
    if [[ $INPUT == *"."* ]]; then
        echo "$INPUT" >> $BOOKMARKS_FILE
    else 
        INPUT="${INPUT}.com" && echo "$INPUT" >> $BOOKMARKS_FILE
    fi
elif [[ $INPUT == "_"* ]]; then
    INPUT=$(echo $INPUT | sed 's/_//') && sed -i "/$INPUT/d" $BOOKMARKS_FILE
elif [[ $INPUT == *"."* ]]; then
    $BROWSER $INPUT
elif [[ -z $INPUT  ]]; then
    exit 0
else
    $BROWSER -new-tab "https://www.google.com/search?q=$INPUT"
fi
