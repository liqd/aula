#!/bin/sh

/liqd/aula/.travis/docker-link-stack-workdir.sh

cd /liqd/aula

stack exec -- ghc-pkg list
