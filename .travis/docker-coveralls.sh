#!/bin/sh

/liqd/aula/.travis/docker-link-stack-workdir.sh

# FIXME: Coverage does not work
cd /liqd/aula

echo $TRAVIS
echo $TRAVIS_JOB_ID
hpc-coveralls spec --exclude-dir=test --curl-verbose
