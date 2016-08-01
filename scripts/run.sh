#!/bin/bash

# script for running pilot systems in production.  not intended for
# general use, but hopefully for inspiration.

set -e

if [ "$AULA_ROOT_PATH" == "" ]; then
    echo "AULA_ROOT_PATH not defined"; bad=1
fi

if [ "$AULA_EXEC_PATH" == "" ]; then
    echo "AULA_EXEC_PATH not defined"; bad=1
fi

if [ "$AULA_NEW_RELEASE" == "" ]; then
    echo "AULA_NEW_RELEASE not defined"; bad=1
fi

if [ "$bad" == "1" ]; then
    exit 1
fi

cd $AULA_ROOT_PATH
mkdir -p $AULA_ROOT_PATH/run
export PIDFILE=$AULA_ROOT_PATH/run/$AULA_NEW_RELEASE.pid

( test -f $PIDFILE && ( ps -ax | grep -e '^\s*'`cat $PIDFILE` | grep -q 'aula-server$' ) ) || not_already_running=1
if [ "$not_already_running" != "1" ]; then
    echo "server already running: doing nothing."
    exit 0
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

if [ -f $AULA_ROOT_PATH/run/* ]; then
    echo " failed!"
    exit 1
else
    echo " ok"
fi

echo "starting $AULA_NEW_RELEASE in $AULA_ROOT_PATH"
echo "writing stdout/err to $AULA_ROOT_PATH/log"
nohup $AULA_EXEC_PATH/aula-server >> $AULA_ROOT_PATH/log 2>&1 & pid=$!
echo $pid > $PIDFILE
echo "done!"
