#!/bin/bash

while true
do
    PROGRESS="$(progress 2>/dev/null | sed -n "2 p" | awk '{print $1}')"
    if [[ -z $PROGRESS ]]; then
        echo ""
    else
        for i in {1..10}
        do
            sleep 0.1
            case $i in
                1)
                    echo "| ⠋"
                    ;;
                2)
                    echo "| ⠙"
                    ;;
                3)
                    echo "| ⠹"
                    ;;
                4)
                    echo "| ⠸"
                    ;;
                5)
                    echo "| ⠼"
                    ;;
                6)
                    echo "| ⠴"
                    ;;
                7)
                    echo "| ⠦"
                    ;;
                8)
                    echo "| ⠧"
                    ;;
                9)
                    echo "| ⠇"
                    ;;
                10)
                    echo "| ⠏"
                    ;;
                *)
                    echo "Unexpected case!"
                    ;;
            esac
        done
    fi
done
