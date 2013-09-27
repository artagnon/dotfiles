#!/bin/sh

if test $# != 1; then exit 1; fi
branchname=$1
git branch -D $branchname
if git remote show | grep -e ^ram$ >/dev/null
then
	git push ram +:$branchname
else
	git push origin +:$branchname
fi
