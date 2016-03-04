#!/bin/sh

AULA_IMAGE=quay.io/liqd/aula

if [ "$1" = "--connect" ]; then
    export CONNECT_TO_RUNNING_CONTAINER=1
fi

if [ "$THENTOS_ROOT_PATH" = "" ]; then
    export THENTOS_ROOT_PATH=`pwd`/../thentos/thentos-core/
fi

export VOLUMES="-v `pwd`:/liqd/aula  -v $THENTOS_ROOT_PATH/..:/liqd/thentos"

if [ "$AULA_SAMPLES" != "" ]; then
    export VOLUMES="$VOLUMES -v $AULA_SAMPLES:/liqd/html-templates"
fi

if [ "$CONNECT_TO_RUNNING_CONTAINER" = "1" ]; then
    docker exec -it `docker ps -q --filter="ancestor=$AULA_IMAGE"` /bin/bash
else
    docker run -it --rm -p 8080:8080 $VOLUMES $AULA_IMAGE /bin/bash
fi
