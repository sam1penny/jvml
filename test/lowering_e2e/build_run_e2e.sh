#!/bin/bash

# assume tmp directory exists, with zip inside
krak2 asm --out "tmp/Foo.zip" tmp/test.j > /dev/null
cd tmp
unzip -u Foo >/dev/null
java -jar ../../../../../scripts/stackmap_gen.jar Foo.class
java Foo
cd ..
rm -rf tmp/