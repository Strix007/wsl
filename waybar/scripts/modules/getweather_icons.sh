#!/bin/bash

source "$HOME"/.env

# Possible values are metric, imperial and kelvin.
UNITS="metric"

URL="http://api.openweathermap.org/data/2.5/weather?APPID="$API_KEY"&id="$CITY_ID"&units="$UNITS""
RESULTS=`curl -s $URL`
ICON_CODE=`echo $RESULTS | jq -r ".weather[].icon"`

if [ "$ICON_CODE" == "01d"  ]; then
    ICON=" "
    CLASS="sunny-day"
elif [ "$ICON_CODE" == "01n"  ]; then
    ICON=""
    CLASS="clear-night"
elif [ "$ICON_CODE" == "02d"  ]; then
    ICON="  "
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "02n"  ]; then
    ICON="  "
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "03d"  ]; then
    ICON=" "
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "03n"  ]; then
    ICON=" "
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "04d"  ]; then
    ICON=" "
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "04n"  ]; then
    ICON=" "
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "09d"  ]; then
    ICON="󰖗  "
    CLASS="rainy"
elif [ "$ICON_CODE" == "09n"  ]; then
    ICON="󰖗  "
    CLASS="rainy"
elif [ "$ICON_CODE" == "10d"  ]; then
    ICON="  "
    CLASS="rainer"
elif [ "$ICON_CODE" == "10n"  ]; then
    ICON="  "
    CLASS="rainer"
elif [ "$ICON_CODE" == "11d"  ]; then
    ICON="   "
    CLASS="stormy"
elif [ "$ICON_CODE" == "11n"  ]; then
    ICON="   "
    CLASS="stormy"
elif [ "$ICON_CODE" == "13d"  ]; then
    ICON=" "
    CLASS="snowy"
elif [ "$ICON_CODE" == "13n"  ]; then
    ICON=" "
    CLASS="snowy"
elif [ "$ICON_CODE" == "40d"  ]; then
    ICON="  "
    CLASS="misty"
elif [ "$ICON_CODE" == "40n"  ]; then
    ICON="  "
    CLASS="misty"
elif [ "$ICON_CODE" == "50d"  ]; then
		ICON="  "
    CLASS="mistier"
elif [ "$ICON_CODE" == "50n"  ]; then
    ICON="  "
    CLASS="mistier"
else
    ICON=" "
    CLASS="unclear-day/night"
fi

RETURN="$ICON"

JSON_STRING=$( jq -n \
                  --arg a "$RETURN" \
                  --arg b "$CLASS" \
                  '{text: $a, class: $b}' )
echo $JSON_STRING
