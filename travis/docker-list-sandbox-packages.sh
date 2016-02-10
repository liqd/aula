#!/bin/sh

cd /root/aula

cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox
cabal exec -- ghc-pkg list
