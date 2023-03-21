source "$HOME"/.env

UNITS="metric"
URL="http://api.openweathermap.org/data/2.5/weather?APPID="$API_KEY"&id="$CITY_ID"&units="$UNITS""
RESULTS=`curl -s $URL`

SUNRISE_UNIX=`echo $RESULTS | jq -r '.sys[]' | sed -n "2 p"`
SUNSET_UNIX=`echo $RESULTS | jq -r '.sys[]' | sed -n "3 p"`

SUNRISE=`date +"%I:%M" -d @"$SUNRISE_UNIX"`
SUNSET=`date +"%H:%M" -d @"$SUNSET_UNIX"`
echo "%{F#f5e0dc}|   $SUNRISE%{F-} %{F#f9e2af}  $SUNSET%{F-}"
