status: just brainstorming

## Build Status

[![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula)

## HTML hacking

To watch some generated test content (mostly for work on HTML / css):

```shell
cabal sandbox init
cabal install
cabal run -- aula-server
```

To re-generate the HTML:

```shell
cabal run -- aula-html-dummies --refresh
```

To generate new haskell data values (implies re-generation of HTML):

```shell
cabal run -- aula-html-dummies --recreate
```
