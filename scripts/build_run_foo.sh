#!/bin/bash

mkdir tmp/
dune exec -- _build/default/bin/compile.exe "$@" > tmp/tmp.j || exit 1
krak2 asm --out "tmp/Foo.zip" tmp/tmp.j > /dev/null
cd tmp
unzip -u Foo >/dev/null
java -jar ../scripts/stackmap_gen.jar sam/generated/Foo.class || exit 1
java sam/generated/Foo
cd ..
rm -rf tmp/