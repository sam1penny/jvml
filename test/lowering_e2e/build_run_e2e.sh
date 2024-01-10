#!/bin/bash

mkdir tmp/
dune exec -- ../../bin/compile.exe "$@" > tmp/test.j || exit 1
krak2 asm --out "tmp/Foo.zip" tmp/test.j > /dev/null
cd tmp
unzip -u Foo >/dev/null
java Foo
cd ..
rm -rf tmp/