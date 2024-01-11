#!/bin/bash

mkdir tmp/
# TODO - currently assumes a binary has been built. Need to
# add a dune dependency in order to ensure that it is built before test_basic.ml is ran
../../bin/compile.exe "$@" > tmp/test.j || exit 1
krak2 asm --out "tmp/Foo.zip" tmp/test.j > /dev/null
cd tmp
unzip -u Foo >/dev/null
java Foo
cd ..
rm -rf tmp/