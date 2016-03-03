#!/bin/sh -e

/liqd/aula/.travis/docker-link-stack-workdir.sh

# Change to the source directory which is attacehed as docker volume
cd /liqd/aula

stack install --fast --test --coverage --allow-different-user --pedantic aula

# FIXME: Coveralls coverage
# # Test
# set +e
# run-cabal-test spec --show-details=never
# RESULT=`echo $?`
# cat dist/test/aula-*-*.log
#
# exit $RESULT
