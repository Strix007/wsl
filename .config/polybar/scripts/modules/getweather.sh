#!/bin/bash

source "$HOME"/.env

# Possible values are metric, imperial and kelvin.
UNITS="metric"

URL="http://api.openweathermap.org/data/2.5/weather?APPID="$API_KEY"&id="$CITY_ID"&units="$UNITS""
RESULTS=`curl -s $URL`
ICON_CODE=`echo $RESULTS | jq -r ".weather[].icon"`
RESULTS_PARSED=`echo $RESULTS | jq .main.temp | cut -f1 -d"."`

if [ "$ICON_CODE" == "01d"  ]; then
    ICON=""
    ICON_HEX="#e5c890"
    CONDITION="Sunny"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "01n"  ]; then
    ICON=""
    ICON_HEX="#c6d0f5"
    CONDITION="Clear"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "02d"  ]; then
    ICON=""
    ICON_HEX="#51576d"
    CONDITION="Cloudy"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "02n"  ]; then
    ICON=""
    ICON_HEX="#708089"
    CONDITION="Cloudy"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "03d"  ]; then
    ICON=""
    ICON_HEX="#7c7f93"
    CONDITION="Cloudier"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "03n"  ]; then
    ICON=""
    ICON_HEX="#708089"
    CONDITION="Cloudier"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "04d"  ]; then
    ICON=""
    ICON_HEX="#7c7f93"
    CONDITION="Cloudiest"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "04n"  ]; then
    ICON=""
    ICON_HEX="#6c7086"
    CONDITION="Cloudiest"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "09d"  ]; then
    ICON="󰖗"
    ICON_HEX="#7dc4e4"
    CONDITION="Rainy"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "09n"  ]; then
    ICON="󰖗"
    ICON_HEX="#7dc4e4"
    CONDITION="Rainy"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "10d"  ]; then
    ICON=""
    ICON_HEX="#f4b8e4"
    CONDITION="Rainier"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "10n"  ]; then
    ICON=""
    ICON_HEX="#f4b8e4"
    CONDITION="Rainier"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "11d"  ]; then
    ICON=""
    ICON_HEX="#e78284"
    CONDITION="Stormy"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "11n"  ]; then
    ICON=""
    ICON_HEX="#e78284"
    CONDITION="Stormy"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "13d"  ]; then
    ICON=""
    ICON_HEX="#99d1db"
    CONDITION="Snowy"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "13n"  ]; then
    ICON=""
    ICON_HEX="#99d1db"
    CONDITION="Snowy"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "40d"  ]; then
    ICON=""
    ICON_HEX="#81c8be"
    CONDITION="Misty"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "40n"  ]; then
    ICON=""
    ICON_HEX="#81c8be"
    CONDITION="Misty"
    DAYTIME="Night"
elif [ "$ICON_CODE" == "50d"  ]; then
		ICON=""
    ICON_HEX="#a7b8b2"
    CONDITION="Mistier"
    DAYTIME="Day"
elif [ "$ICON_CODE" == "50n"  ]; then
    ICON=""
    ICON_HEX="#a7b8b2"
    CONDITION="Mistier"
    DAYTIME="Night"
else
    ICON=""
    ICON_HEX="#f2d5cf"
    CONDITION="Unclear"
    DAYTIME="Unclear"
fi

if   [[ $UNITS == "metric" ]]; then
    UNIT="󰔄"
elif [[ $UNITS == "imperial" ]]; then
    UNIT="󰔅"
elif [[ $UNITS == "kelvin" ]]; then
    UNIT="󰔆"
elif [ -z $UNITS  ]; then
    echo "Units variable empty, please choose which unit do you want the weather in"
else
    echo "Unexpected value, check variable for correct value. Possible values are metric,imperial or kelvin."
fi

FORMAT="%{F$ICON_HEX}| $ICON $CONDITION $DAYTIME $RESULTS_PARSED $UNIT %{F-}"
echo "$FORMAT"
