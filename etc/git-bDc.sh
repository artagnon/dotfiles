#!/bin/sh

test "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" || exit 1
git checkout master
git branch -D @{-1}
