#!/bin/bash
getstorage() {
    df -h $1 | awk '{print $3}' | tail -n 1 | sed 's/G//' | cut -f1 -d"."
}

USED_ROOT=$(getstorage /)
USED_HOME=$(getstorage /home)
USED_MEDIA=$(getstorage /media)
USED_TOTAL=$(($USED_ROOT+ $USED_HOME))
echo $USED_TOTAL
