status: just brainstorming

## Build Status

[![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula)


## Getting started (with docker)

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


## Getting started (with sensei)

https://github.com/hspec/sensei is a tool for rapid re-compilation and
testing.  If you want to use it, follow these steps:

- git clone --recursive https://github.com/hspec/sensei
- git clone --recursive https://github.com/liqd/thentos
- export THENTOS_ROOT_PATH=`pwd`/thentos
- cd thentos
- ./misc/thentos-install.hs -p
- cd ..
- git clone https://github.com/liqd/aula
- export AULA_ROOT_PATH=`pwd`/aula
- cd aula
- cabal sandbox init --sandbox=../thentos
- cabal install
- make sensei-full

This will watch your files, and if anything changes, re-compile and
run the test suite.

You can use seito (same git repo, different executable) to pull the
last error report into your IDE to get pointed to the source code
locations.

Backround: sensei does not support multi-package development as such,
so we simply add the source files of all packages we want to use to
the source paths with a long list of `-i`s.  This way, any change in
any of `thentos-*` or `aula` will trigger a re-run.


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
