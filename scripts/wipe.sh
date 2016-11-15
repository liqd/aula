#!/bin/bash

set -e

echo "please read the script before you run it!"
# ...  and comment out the following line if you are absolutely sure you know what you are doing:
exit 1

#export AULA_EXPOSED_URL=https://schule-am-hafen.aula.de/
export AULA_EXPOSED_URL=`perl -ne '/_exposedUrl: (http.*)$/ && print $1' < aula.yaml`
export AULA_ADMIN_PASSWD=pssst

echo "about to wipe $AULA_EXPOSED_URL.  if you agree please press ^D!"
cat > /dev/null

rm -f cookie-jar
curl -c cookie-jar -F /login.user=admin -F /login.pass=$AULA_ADMIN_PASSWD ${AULA_EXPOSED_URL}login
curl -b cookie-jar -XPOST ${AULA_EXPOSED_URL}api/manage-state/wipe
rm -f cookie-jar
