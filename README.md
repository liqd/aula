
project status | build status                                                                                          | more info
---------------|-------------------------------------------------------------------------------------------------------|--------------------------
experimental   | [![Build Status](https://travis-ci.org/liqd/aula.svg?branch=master)](https://travis-ci.org/liqd/aula) | http://aula-blog.website/


## Getting started (with docker)

in a shell:

```shell
# install docker
docker pull quay.io/liqd/aula
git clone https://github.com/liqd/aula
cd aula
./docker/run.sh
# now you are inside the container.
stack install --fast --allow-different-user aula
```

Now, to have a quick look at the pages, do

```shell
make aula-server
```

then point your browser to localhost:8080

Note: when you want to `git pull`, do this outside of docker,
just as `git clone` was performed outside. Otherwise, paths may be wrong.


## Getting started (without docker, without stack)

https://github.com/hspec/sensei is a tool for rapid re-compilation and
testing.  If you want to use it, follow these steps:

- install any required system and ghc packages, e.g., for Debian-based
  systems, those mentioned in
  https://github.com/liqd/aula-docker/blob/master/Dockerfile
  in particular, libpq-dev and zlib1g-dev
- in a shell:

```shell
git clone https://github.com/liqd/aula
export AULA_ROOT_PATH=`pwd`/aula
cd aula
cabal sandbox init
```

Now, to have a quick look at the pages, do

```shell
make aula-server
make content  # (in another terminal)
```

The second line generates some demo content, but also the initial user
without which you can't login and create more users.

Now point your browser to localhost:8080


## Using sensei for file-watch testing during development (with docker)

To start sensei, open a new terminal window and do this:

```shell
./docker/run.sh
# now you are inside the container.
make sensei
```

This will watch your files, and if anything changes, re-compile and
run the test suite.

If you are already running a docker container in another terminal, but
want to keep that terminal open to do other things, you can connect to
that container with a new shell:

```shell
./docker/run.sh --connect
# now you are inside the container.
make sensei
```

You can use seito (same git repo, different executable) to pull the
last error report into your IDE to get pointed to the source code
locations.


## HTML hacking

In demo mode, the aula footer contains a link "create sample page"
that points you to a text page that contains the data describing the
status of the web page you are looking at.

If you cut&paste this value into a file, and move it to
`$HOME/aula-samples`, you can use the following process to work on the
`ToHtml` instance of that page in real-time, having instantaneous
refresh on your changes.  This is also valuable if you want to report
an issue with the contents of a page.

Again, start the server like above, but this time make sure that
outside of the docker container, you have $AULA_SAMPLES defined:

```shell
export AULA_SAMPLES=$HOME/aula-samples/
./docker/run.sh  # do not use --connect here!
# now you are inside the container.
make aula-server
```

NOTE: `$AULA_SAMPLES` directory shouldn't be inside the `aula` repo.
If it is, a sensei re-run will be triggered by its own
changes to the html files, resulting in a very noisy loop.

NOTE: docker has a VOLUME under `/root/html-templates` that
allows you to edit the html code from outside of docker, and still
serve it from the inside.  If you set $AULA_SAMPLES to a directory of
your choice outside docker, and to `/root/html-templates inside, the
script ./docker/run.sh` will mount the outside directory into the
inside one.

NOTE: This only works if you do not call `./docker/run.sh` without
`--connect`.  (If you have a docker container running elsewhere,
consider shutting it down first, and later connecting to the one
running here.)

To refresh the HTML from the same content (same texts and everything),
open a new terminal window and do this:

```shell
./docker/run.sh --connect
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

For more info on this, check out `./docs/page-creator-howto.md`.


### SASS Hacking

SASS is used for styling, use the following command to compile the SASS
to CSS

You need the sass compiler installed http://sass-lang.com/install

```shell
sass --watch static-src/scss/imports.scss:static/css/all.css --style compressed
```

...  or just run it once:

```shell
sass static-src/scss/imports.scss:static/css/all.css --style compressed
```


### Creating icons

You need fontcustom installed https://github.com/FontCustom/fontcustom

1. Add your SVGs to static-src/icons-svg/ No strokes, fills only.
2. run `fontcustom compile -c ./fontcustom.yml`
