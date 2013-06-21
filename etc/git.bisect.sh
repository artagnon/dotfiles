#!/bin/sh

make clean &&
make -j 8 &&
cd t &&
sh -v -i regression.sh
