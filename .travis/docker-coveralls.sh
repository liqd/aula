#!/bin/sh

cd /root/aula

echo $TRAVIS
echo $TRAVIS_JOB_ID
hpc-coveralls spec --exclude-dir=test --curl-verbose
