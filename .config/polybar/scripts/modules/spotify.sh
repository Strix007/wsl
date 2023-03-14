#!/bin/sh
FORMAT="{{ artist }} - {{ title }}"
main() {
  if ! pgrep -x spotify >/dev/null; then
    echo ""; exit
  fi  
  RESULT=$(playerctl --player=spotify metadata --format "$FORMAT")
  echo "$RESULT"
}
main "$@"
