#!/bin/sh
FORMAT="{{ artist }} - {{ title }}"
main() {
  if ! pgrep -x spotify >/dev/null; then
    echo ""; exit
  fi  
  echo $(playerctl --player=spotify metadata --format "$FORMAT")
}
main "$@"