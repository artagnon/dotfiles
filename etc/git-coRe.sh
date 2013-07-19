#!/bin/sh

test -e .git/HEAD || exit 1
test $# = 1 || exit 1
git checkout -b "$1-2"
git cherry-pick ..$1
git branch -M $1
