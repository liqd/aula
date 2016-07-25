# misc details on how we test aula

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
