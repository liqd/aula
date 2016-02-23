#!/bin/sh -e

CABAL_SANDBOX=/liqd/thentos/.cabal-sandbox
GHC_OPTIONS="-Werror -Wall"

# Change to the source directory which is attacehed as docker volume
cd /root/aula

# Init cabal sandbox environment
cabal update
cabal sandbox init --sandbox=$CABAL_SANDBOX

# Install from the current code
cabal install --enable-tests --only-dependencies --reorder-goals
cabal configure --enable-tests --disable-optimization
cabal build --ghc-options="$GHC_OPTIONS"

# Test
set +e
cabal test --show-details=never --ghc-options="$GHC_OPTIONS"
RESULT=`echo $?`
cat dist/test/aula-*-*.log

exit $RESULT
