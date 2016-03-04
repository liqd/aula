#!/bin/sh -e

# Linking stack working directory

test -h /liqd/aula/.stack-work || ln -s /liqd/stack /liqd/aula/.stack-work
test -h /liqd/aula/.stack-work || ln -s /liqd/stack /liqd/thentos/.stack-work
