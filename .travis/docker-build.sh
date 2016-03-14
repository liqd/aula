#!/bin/sh -e

/liqd/aula/.travis/docker-link-stack-workdir.sh

# Change to the source directory which is attacehed as docker volume
cd /liqd/aula

if [ -z "$1" ] ; then
    NO_OF_CASES="100"
else
    NO_OF_CASES="$1"
fi

stack install --fast --test --test-arguments "-a ${NO_OF_CASES}" --coverage --allow-different-user --pedantic aula

# FIXME: Coveralls coverage
# # Test
# set +e
# run-cabal-test spec --show-details=never
# RESULT=`echo $?`
# cat dist/test/aula-*-*.log
#
# exit $RESULT
