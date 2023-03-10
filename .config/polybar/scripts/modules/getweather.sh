#!/bin/bash
source $HOME/.env
UNITS="metric"
URL="http://api.openweathermap.org/data/2.5/weather?APPID="$API_KEY"&id="$CITY_ID"&units="$UNITS""
RESULTS=`curl -s $URL`
ICON_CODE=`echo $RESULTS | jq -r ".weather[].icon"`
RESULTS_PARSED=`echo $RESULTS | jq .main.temp | cut -f1 -d"."`
# echo $RESULTS
if [ "$ICON_CODE" == "50d"  ]; then
		ICON=""
    ICON_HEX="#a7b8b2"
elif [ "$ICON_CODE" == "50n"  ]; then
    ICON=""
    ICON_HEX="#85c1dc"
elif [ "$ICON_CODE" == "01d"  ]; then
    ICON=""
    ICON_HEX="#e5c890"
elif [ "$ICON_CODE" == "01n"  ]; then
    ICON=""
    ICON_HEX="#c6d0f5"
elif [ "$ICON_CODE" == "02d"  ]; then
    ICON=""
    ICON_HEX="#51576d"
elif [ "$ICON_CODE" == "02n"  ]; then
    ICON=""
    ICON_HEX="#414559"
elif [ "$ICON_CODE" == "03d"  ]; then
    ICON=""
    ICON_HEX="#51576d"
elif [ "$ICON_CODE" == "03n"  ]; then
    ICON=""
    ICON_HEX="#414559"
elif [ "$ICON_CODE" == "04d"  ]; then
    ICON=""
    ICON_HEX="#51576d"
elif [ "$ICON_CODE" == "04n"  ]; then
    ICON=""
    ICON_HEX="#414559"
elif [ "$ICON_CODE" == "09d"  ]; then
    ICON=""
    ICON_HEX="##ef9f76"
elif [ "$ICON_CODE" == "09n"  ]; then
    ICON=""
    ICON_HEX="##ef9f76"
elif [ "$ICON_CODE" == "10d"  ]; then
    ICON=""
    ICON_HEX="##f4b8e4"
elif [ "$ICON_CODE" == "10n"  ]; then
    ICON=""
    ICON_HEX="##f4b8e4"
elif [ "$ICON_CODE" == "11d"  ]; then
    ICON=""
    ICON_HEX="#e78284"
elif [ "$ICON_CODE" == "11n"  ]; then
    ICON=""
    ICON_HEX="#e78284"
elif [ "$ICON_CODE" == "13d"  ]; then
    ICON=""
    ICON_HEX="#99d1db"
elif [ "$ICON_CODE" == "13n"  ]; then
    ICON=""
    ICON_HEX="#99d1db"
elif [ "$ICON_CODE" == "40d"  ]; then
    ICON=""
    ICON_HEX="#81c8be"
elif [ "$ICON_CODE" == "40n"  ]; then
    ICON=""
    ICON_HEX="#81c8be"
else
    ICON=""
    ICON_HEX="#f2d5cf"
fi
echo "%{F$ICON_HEX}| $ICON $RESULTS_PARSED°C %{F-}"
