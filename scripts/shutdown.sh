#!/bin/bash

# script for running pilot systems in production.  not intended for
# general use, but published here in the hope it will be interesting.

set -e

if [ "$AULA_ROOT_PATH" == "" ]; then
    echo "AULA_ROOT_PATH not defined"; bad=1
fi

echo -n "killing server in $AULA_ROOT_PATH:"
for i in `ls $AULA_ROOT_PATH/run`; do
    pidfile=$AULA_ROOT_PATH/run/$i
    pid=`cat $pidfile`
    echo -n " $pid"
    kill $pid || echo "(pid file must have been stale)"
    while (ps -ax | grep '^\s*'$pid | grep -q 'aula-server$'); do
        echo -n '.'; sleep 0.42
    done
    rm -f $pidfile
done
