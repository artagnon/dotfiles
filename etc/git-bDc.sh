#!/bin/sh

cd "$(git rev-parse --show-toplevel 2>/dev/null)"
test -e .git/HEAD || exit 1
cur=$(sed 's/ref: refs\/heads\///' .git/HEAD)
test $cur = "master" && exit 1
git checkout master
git branch -D $cur
