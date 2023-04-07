#!/bin/bash

source "$HOME"/.env

# Possible values are metric, imperial and kelvin.
UNITS="metric"

URL="http://api.openweathermap.org/data/2.5/weather?APPID="$API_KEY"&id="$CITY_ID"&units="$UNITS""
RESULTS=`curl -s $URL`
ICON_CODE=`echo $RESULTS | jq -r ".weather[].icon"`
RESULTS_PARSED=`echo $RESULTS | jq .main.temp | cut -f1 -d"."`
FEELS_LIKE=`echo $RESULTS | jq -r .main.feels_like | cut -f1 -d"."`

if [ "$ICON_CODE" == "01d"  ]; then
    CONDITION="Sunny"
    DAYTIME="Day"
    CLASS="sunny-day"
elif [ "$ICON_CODE" == "01n"  ]; then
    CONDITION="Clear"
    DAYTIME="Night"
    CLASS="clear-night"
elif [ "$ICON_CODE" == "02d"  ]; then
    CONDITION="Cloudy"
    DAYTIME="Day"
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "02n"  ]; then
    CONDITION="Cloudy"
    DAYTIME="Night"
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "03d"  ]; then
    CONDITION="Cloudier"
    DAYTIME="Day"
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "03n"  ]; then
    CONDITION="Cloudier"
    DAYTIME="Night"
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "04d"  ]; then
    CONDITION="Cloudiest"
    DAYTIME="Day"
    CLASS="cloudy-day"
elif [ "$ICON_CODE" == "04n"  ]; then
    CONDITION="Cloudiest"
    DAYTIME="Night"
    CLASS="cloudy-night"
elif [ "$ICON_CODE" == "09d"  ]; then
    CONDITION="Rainy"
    DAYTIME="Day"
    CLASS="rainy"
elif [ "$ICON_CODE" == "09n"  ]; then
    CONDITION="Rainy"
    DAYTIME="Night"
    CLASS="rainy"
elif [ "$ICON_CODE" == "10d"  ]; then
    CONDITION="Rainier"
    DAYTIME="Day"
    CLASS="rainier"
elif [ "$ICON_CODE" == "10n"  ]; then
    CONDITION="Rainier"
    DAYTIME="Night"
    CLASS="rainier"
elif [ "$ICON_CODE" == "11d"  ]; then
    CONDITION="Stormy"
    DAYTIME="Day"
    CLASS="stormy"
elif [ "$ICON_CODE" == "11n"  ]; then
    CONDITION="Stormy"
    DAYTIME="Night"
    CLASS="stormy"
elif [ "$ICON_CODE" == "13d"  ]; then
    CONDITION="Snowy"
    DAYTIME="Day"
    CLASS="snowy"
elif [ "$ICON_CODE" == "13n"  ]; then
    CONDITION="Snowy"
    DAYTIME="Night"
    CLASS="snowy"
elif [ "$ICON_CODE" == "40d"  ]; then
   CONDITION="Misty"
    DAYTIME="Day"
    CLASS="misty"
elif [ "$ICON_CODE" == "40n"  ]; then
    CONDITION="Misty"
    DAYTIME="Night"
    CLASS="misty"
elif [ "$ICON_CODE" == "50d"  ]; then
    CONDITION="Mistier"
    DAYTIME="Day"
    CLASS="mistier"
elif [ "$ICON_CODE" == "50n"  ]; then
    CONDITION="Mistier"
    DAYTIME="Night"
    CLASS="mistier"
else
    CONDITION="Unclear"
    DAYTIME="Unclear"
    CLASS="unclear-day/night"
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

RETURN="$CONDITION $DAYTIME $RESULTS_PARSED $UNIT"

tooltip="Feels Like: $FEELS_LIKE $UNIT"
JSON_STRING=$( jq -n \
                  --arg a "$RETURN" \
                  --arg b "$tooltip" \
                  --arg c "$CLASS" \
                  '{text: $a, tooltip: $b, class: $c}' )
echo $JSON_STRING
