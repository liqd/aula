status: experimental


## Build Status

[![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula)
[![Coverage Status](https://coveralls.io/repos/github/liqd/aula/badge.svg?branch=master)](https://coveralls.io/github/liqd/aula?branch=master)


## Getting started (with docker)

in a shell:

```shell
# install docker
docker pull quay.io/liqd/aula
git clone https://github.com/liqd/aula
git clone --recursive https://github.com/liqd/thentos
cd aula
./docker/run.sh
# now you are inside the container.
cd /root/thentos
cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox
cd /root/aula
cabal sandbox init --sandbox=/liqd/thentos/.cabal-sandbox
cabal install --enable-tests
```

Now, to have a quick look at the pages, do

```shell
cabal run aula-server
```

then point your browser to localhost:8080

Note: when you want to `git pull`, do this outside of docker,
just as `git clone` was performed outside. Otherwise, paths may be wrong.


## Getting started (without docker)

https://github.com/hspec/sensei is a tool for rapid re-compilation and
testing.  If you want to use it, follow these steps:

- install any required system and ghc packages, e.g., for Debian-based
  systems, those mentioned in
  https://github.com/liqd/aula-docker/blob/master/Dockerfile
  in particular, libpq-dev and zlib1g-dev
- in a shell:

```shell
git clone --recursive https://github.com/hspec/sensei
git clone --recursive https://github.com/liqd/thentos
export THENTOS_ROOT_PATH=`pwd`/thentos/thentos-core
cd thentos
./misc/thentos-install.hs -p
cd ..
git clone https://github.com/liqd/aula
export AULA_ROOT_PATH=`pwd`/aula
cd aula
cabal sandbox init --sandbox=../thentos/.cabal-sandbox
```

Now, to have a quick look at the pages, do

```shell
cabal run aula-server
```

then point your browser to localhost:8080


## Using sensei for file-watch testing during development

[This section assumes you run without docker, but with
`./docker/run.sh` you should be able to do the same thing in the
docker setting as well.]

To start sensei, in another terminal do

```shell
cabal install --enable-tests
make sensei
```

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
export AULA_SAMPLES=$HOME/aula-samples/
make click-dummies-recreate
make aula-server
```

Note: `$AULA_SAMPLES` directory shouldn't be inside `aula` or
`thentos`.  If it is, a sensei re-run will be triggered by its own
changes to the html files, resulting in a very noisy loop.

NOTE on docker: docker has a VOLUME under `/root/html-templates` that
allows you to edit the html code from outside of docker, and still
serve it from the inside.  If you set $AULA_SAMPLES to a directory of
your choice outside docker, and to `/root/html-templates inside, the
script ./docker/run.sh` will mount the outside directory into the
inside one.

To refresh the HTML from the same content (same texts and everything):

```shell
make click-dummies-refresh
```

The html pages are created in `$AULA_SAMPLES/`, and can be browsed
under `http://localhost:8080/samples/`.  You can do two things now:

1. Edit the html pages directly.  If you want to keep or diff your
   changes, you can initialize a local git repo (`cd $AULA_SAMPLES &&
   git init`) and commit them there.

2. Edit source code in the original repo under `src/Frontend/Page/*.hs`.
   If you do this, `$AULA_SAMPLES/*.html` will be overwritten, so make
   sure that all valuable changes you make there are under git control
   and recorded apropriately.

You can combine the two work-flows and edit first the html, then
record, then edit the Haskell source code and use git to diff the
generated html against the html you tweaked manually.

Both `aula-server` and `click-dummies-refresh` go into a loop, so you
need two terminals.  If you have two displays, you can move your
browser and the terminal with the refresh loop to one, and your source
code editor to the other.  The browser will auto-refresh at any modification
and you will never have to focus there, no matter whether you
work on the haskell sources or on the html.

If you use docker, to be able to use more terminals, you can start
a new shell with

```shell
./docker/run.sh --connect
```

For more info on this, check out `./docs/PageCreatorHOWTO.md`.

### SASS Hacking

SASS is used for styling, use the following command to compile the SASS
to CSS

You need the sass compiler installed http://sass-lang.com/install

```shell
sass --watch static-src/scss/imports.scss:static/css/all.css --style compressed
```

### Creating icons

You need fontcustom installed https://github.com/FontCustom/fontcustom

1. Add your SVGs to static/icons. No strokes, fills only.
2. run `fontcustom compile static/svg -o static/icons`
