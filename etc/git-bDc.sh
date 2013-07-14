#!/bin/sh

test -e .git/HEAD || exit 1
cur=$(sed 's/ref: refs\/heads\///' .git/HEAD)
test $cur = "master" && exit 1
git checkout master
git branch -D $cur
