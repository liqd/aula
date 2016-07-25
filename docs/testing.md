# misc details on how we test aula

## test suites

During the development if an issue occurs, we try to program the type system
to reject code which could lead to similar issues in the future. The second
defense line is to write quick check properties that check against the
assumptions we have at the moment.

As the test suite grows it is necessary to make a separation between the
tests which we run during the development cycle and those tests that should
be run on the continuous integration. We call it **smoke test suite**,
the second one we can omit the 'smoke' prefix, or it can be called the
**large test suite**.

### Smoke test suite

The number and the runtime of the test cases should be kept at minimum
in the smoke test suite, but those tests should test as many requirements
as possible.

### Larger test suite

Test cases marked with `Large` tag in the test suite run only in the
the large test suite, which should run on the CI. This test
suite uses different parameters for the HSpec test runner, such as
running more test cases, and run quickcheck test with larger size
parameter. Also includes the test cases with the `Large` tag.

### Future test suites

In the future, integration, stress, and UI based tests could be written
which should be run before releases.


## selenium / webdriver

The docker image http://quay.io/liqd/aula comes comes with selenium
grid (see http://seleniumhq.com/) since tag `aula-docker-0.2`.

The tests can be run from within the docker image as follows:

```shell
./docker/selenium.sh restart
make selenium
```

You can watch the tests by connecting to the docker image via vnc:

```shell
vncviewer `docker exec -it \`docker ps -q\` /liqd/aula/docker/get-ip.sh`:5900
[...]
Password: gandalf
```

...  now rerun the test suite and watch.


### saucelabs.com and sauce connect.

Via saucelabs, you can connect to the docker image via sauce connect
and use any browser and os ever released to for manual testing and
issue reproduction.  You need two terminals for that, one for the
sauce connect proxy:

```shell
wget https://saucelabs.com/downloads/sc-4.3.16-linux.tar.gz
tar xvpzf sc-4.3.16-linux.tar.gz
./sc-4.3.16-linux/bin/sc -u $SAUCELABS_USER -k $SAUCELABS_KEY
```

...  and one for the aula server.  (As mentioned in README.md, use
`./docker/run.sh --connect` to open new terminals inside the docker
image.

```shell
perl -i -pe s/8080/80/g aula.yaml
perl -i -pe s/localhost/`./docker/get-ip.sh`/g aula.yaml
make aula-server
```
