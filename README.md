## Build Status

[![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula)


## Getting started (with docker)

- install docker
- docker pull fisx/aula
- git clone https://github.com/liqd/aula
- git clone --recursive https://github.com/liqd/thentos
- cd aula
- ./docker/run.sh
- inside container:
    - cd /root/thentos
    - cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox
    - cd /root/aula
    - cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox
    - cabal run aula-server
- then point your browser to localhost:8080


## Getting started (with sensei)

https://github.com/hspec/sensei is a tool for rapid re-compilation and
testing.  If you want to use it, follow these steps:

- install any required system and ghc packages, e.g., for Debian-based
  systems, those mentioned in
  https://github.com/liqd/aula-docker/blob/master/Dockerfile
  in particular, libpq-dev and zlib1g-dev
- git clone --recursive https://github.com/hspec/sensei
- git clone --recursive https://github.com/liqd/thentos
- export THENTOS_ROOT_PATH=`pwd`/thentos
- cd thentos
- ./misc/thentos-install.hs -p
- cd ..
- git clone https://github.com/liqd/aula
- export AULA_ROOT_PATH=`pwd`/aula
- cd aula
- cabal sandbox init --sandbox=../thentos/.cabal-sandbox
- cabal install --enable-tests
- cabal run aula-server
- make sensei-full

This will watch your files, and if anything changes, re-compile and
run the test suite.

You can use seito (same git repo, different executable) to pull the
last error report into your IDE to get pointed to the source code
locations.

Background: sensei does not support multi-package development as such,
so we simply add the source files of all packages we want to use to
the source paths with a long list of `-i`s.  This way, any change in
any of `thentos-*` or `aula` will trigger a re-run.

Note: if compilation speed is an issue, you can modify thentos-install.hs,
commenting out thentos-adhocracy and adding --disable-optimization
(and --disable-library-profiling, if you have profiling on by default,
but don't want to use prof stack traces for this project)
to some of the runCabal invocations.


## HTML hacking

To create arbitrary (randomized) test content and browse it (mostly
for work on HTML / css):

```shell
export AULA_SAMPLES=/tmp/aula-samples/
cabal sandbox init
cabal install
make click-dummies-recreate
make aula-server
```

To refresh the HTML from the same content (same texts and everything):

```shell
make click-dummies-refresh
```

The html pages are created in `/tmp/aula-samples/`, and can be browsed
under `http://localhost:8080/samples/`.  You can either edit the html
pages directly, or the source code in this repo under
`src/Frontend/Page/*.hs`.  If you edit `src/...`, `/tmp/aula-samples/`
will be overwritten.  If you want to keep changes you did to the
generated html code, you can initialize a local git repo (`cd
/tmp/aula-samples && git init`) and commit your changes before editing
the haskell sources.  If you do so, you can use `git diff` to make
sure that the haskell code does what you expected.

Both `aula-server` and `click-dummies-refresh` go into a loop, so you
need to terminals.  If you have two displays, you can move your
browser and the terminal with the refresh loop to one, and your source
code editor to the other.  The source code will auto-refresh on the
former and you will never have to focus there, no matter whether you
work on the haskell sources or on the html.
