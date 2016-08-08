#!/bin/bash

# script for running pilot systems in production.  not intended for
# general use, but published here in the hope it will be interesting.

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

./scripts/shutdown.sh

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
