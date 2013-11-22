#!/bin/sh

test "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" || exit 1
branches="$@"
pushstring=
for branch in $branches; do git branch -D $branch; done
for branch in $branches; do pushstring="$pushstring +:$branch"; done
if git remote show | grep -e ^ram$ >/dev/null
then
	git push ram $pushstring
else
	git push origin $pushstring
fi
