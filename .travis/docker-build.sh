#!/bin/bash -e

/liqd/aula/.travis/docker-link-stack-workdir.sh

# Change to the source directory which is attacehed as docker volume
cd /liqd/aula

NO_OF_CASES="100"
SIZE_OF_CASES="30"

while [[ $# > 1 ]]
do
key=$1
case $key in
	--qc-max-success)
	NO_OF_CASES="$2"
	shift
	;;
	--qc-max-size)
	SIZE_OF_CASES="$2"
	shift
	;;
	*)
	;;
esac
shift
done

stack install --fast --test --test-arguments "--qc-max-success=${NO_OF_CASES} --qc-max-size=${SIZE_OF_CASES}" --coverage --allow-different-user --pedantic aula

# FIXME: Coveralls coverage
# # Test
# set +e
# run-cabal-test spec --show-details=never
# RESULT=`echo $?`
# cat dist/test/aula-*-*.log
#
# exit $RESULT
