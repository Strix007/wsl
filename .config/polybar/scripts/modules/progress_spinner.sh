#!/bin/bash
# ⠋
# ⠙
# ⠹
# ⠸
# ⠼
# ⠴
# ⠦
# ⠧
# ⠇
# ⠏

while true
do
    PROGRESS="$(progress 2>/dev/null | sed -n "2 p" | awk '{print $1}')"
    if [[ -z $PROGRESS ]]; then
        echo ""
    else
        for i in {1..10}
        do
            case $i in
                1)
                    sleep 0.1
                    echo "| ⠋"
                    ;;
                2)
                    sleep 0.1
                    echo "| ⠙"
                    ;;
                3)
                    sleep 0.1
                    echo "| ⠹"
                    ;;
                4)
                    sleep 0.1
                    echo "| ⠸"
                    ;;
                5)
                    sleep 0.1
                    echo "| ⠼"
                    ;;
                6)
                    sleep 0.1
                    echo "| ⠴"
                    ;;
                7)
                    sleep 0.1
                    echo "| ⠦"
                    ;;
                8)
                    sleep 0.1
                    echo "| ⠧"
                    ;;
                9)
                    sleep 0.1
                    echo "| ⠇"
                    ;;
                10)
                    sleep 0.1
                    echo "| ⠏"
                    ;;
                *)
                    echo "Unexpected case!"
                    ;;
            esac
        done
    fi
done
