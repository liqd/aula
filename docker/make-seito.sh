#!/bin/bash

# NOTES:
# - not sure why i have to do this, but it helped
# - the mkdir-rmdir-dance creates the entire hierarchy of directories
#   up to the one that should be a link, then replaces the last one by
#   the link.

export PATH=$PATH:/liqd/thentos/.cabal-sandbox/bin/
export OUTSIDE_WD=`cat /root/aula/pwd.log`
test -h $OUTSIDE_WD || ( mkdir -p $OUTSIDE_WD; rmdir $OUTSIDE_WD; ln -s /root/aula $OUTSIDE_WD )
cd $OUTSIDE_WD && make seito
