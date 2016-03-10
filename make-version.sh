#!/bin/bash

set -e
cd `dirname $0`
export NEW_VERSION="$1"
export ONLY_RUN_ON_BRANCH="master"

test -z "$NEW_VERSION" \
    && ( echo "Please provide a version number to move to."; exit 1 )
git status -b | grep -q "On branch $ONLY_RUN_ON_BRANCH" \
    || ( echo "Please only run this script on master."; exit 1 )

perl -i -pe 's/(version:\s*).*/${1}'"$NEW_VERSION"'/' package.yaml
hpack
git add package.yaml aula.cabal

git commit -m 'Release '"$NEW_VERSION"'.'
git tag v$NEW_VERSION || ( echo "please manually revert the release commit that was just created."; exit 1 )

echo "created new commit and tag."
echo -e "\n\ngit show:\n"; git show
echo -e "\n\ngit tag -l:\n"; git tag -l

echo -e "\nto push, run:\ngit push\ngit push --tags"
echo -e "\nto undo, run:\ngit reset --soft HEAD~1\ngit reset HEAD package.yaml aula.cabal\ngit checkout package.yaml aula.cabal"
