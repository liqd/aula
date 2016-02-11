status: just brainstorming

## Build Status

[![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula)


## Getting started

- install docker
- docker pull fisx/aula
- git checkout aula
- cd aula
- docker run --rm -it -p 8080:8080 -v `pwd`:/root/aula fisx/aula /bin/bash
- inside container:
    - cd /root/aula
    - cabal init --sandbox=/liqd/thentos/.cabal-sandbox
    - perl -i -pe s/"127.0.0.1"/"0.0.0.0"/ src/Config.hs
    - cabal run aula-server
- then point your browser to localhost:8080

if you want to hack both thentos and aula, `/root/thentos` is exposed
as a docker volume similarly to `/root/aula`.  Just duplicate the `-v`
option to `docker run`.


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
