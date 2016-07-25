#!/bin/bash

# This is called from Makefile rule `seito-docker-hack`.

# NOTES:
# - mimics filesystem paths in host system so that error messages make
#   sense to the IDE there.
# - the mkdir-rmdir-dance creates the entire hierarchy of directories
#   up to the one that should be a link, then replaces the last one by
#   the link.

export OUTSIDE_WD=`cat /liqd/aula/pwd.log`
mkdir -p $OUTSIDE_WD
rm -rf $OUTSIDE_WD
ln -sT /liqd/aula/ $OUTSIDE_WD
cd $OUTSIDE_WD && make seito
