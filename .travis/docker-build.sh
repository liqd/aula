#!/bin/bash -e

/liqd/aula/.travis/docker-link-stack-workdir.sh
/liqd/aula/docker/selenium.sh restart

# Change to the source directory which is attached as docker volume
cd /liqd/aula

QC_MAX_SUCCESS="100"
QC_MAX_SIZE="30"

while [[ $# > 1 ]]; do
    key=$1
    case $key in
        --qc-max-success)
            QC_MAX_SUCCESS="$2"
            ;;

        --qc-max-size)
            QC_MAX_SIZE="$2"
            ;;

        *)
            ;;
    esac
    shift
done

stack install --fast --test --test-arguments "--qc-max-success=${QC_MAX_SUCCESS} --qc-max-size=${QC_MAX_SIZE}" --coverage --allow-different-user --pedantic aula

# FIXME: Coveralls coverage
# # Test
# set +e
# run-cabal-test spec --show-details=never
# RESULT=`echo $?`
# cat dist/test/aula-*-*.log
#
# exit $RESULT
