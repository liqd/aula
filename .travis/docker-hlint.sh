#!/bin/sh

/liqd/aula/.travis/docker-link-stack-workdir.sh

cd /liqd/aula

make hlint
