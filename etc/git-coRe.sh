#!/bin/sh

test "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" || exit 1
test $# = 1 || exit 1
git checkout -b "$1-2"
git cherry-pick ..$1
git branch -M $1
