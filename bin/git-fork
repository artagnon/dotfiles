#!/bin/sh

test "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" || exit 1
hub fork
git remote rename artagnon ram
git config remote.pushdefault ram
