#!/bin/sh

git clean -dfx &&
make -C src -j8 ocaml-fstar-ocaml &&
make -C ulib -j8
