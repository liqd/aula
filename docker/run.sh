#!/bin/sh

AULA_IMAGE=quay.io/liqd/aula

if [ "$THENTOS_ROOT_PATH" = "" ]; then
    export THENTOS_ROOT_PATH=`pwd`/../thentos/thentos-core/
fi

export VOLUMES="-v `pwd`:/root/aula -v $THENTOS_ROOT_PATH/..:/root/thentos"

if [ "$AULA_SAMPLES" != "" ]; then
    export VOLUMES="$VOLUMES -v $AULA_SAMPLES:/root/html-templates"
fi

docker run -it --rm -p 8080:8080 $VOLUMES $AULA_IMAGE /bin/bash
