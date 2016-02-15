#!/bin/sh -e

# Change to the source directory which is attacehed as docker volume
cd /root/aula

# Init cabal sandbox environment
cabal update
cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox

# Install from the current code
cabal install --enable-tests --only-dependencies --reorder-goals
cabal configure --enable-tests --disable-optimization
cabal build

# Test
set +e
cabal test --show-details=never
RESULT=`echo $?`
cat dist/test/aula-*-tests.log

exit $RESULT
