#!/bin/bash

copy_args=( "$@" )


generated_class="Foo"

# assume tmp directory exists, with zip inside
krak2 asm --out "tmp/${generated_class}.zip" tmp/test.j > /dev/null
cd tmp
unzip -u "${generated_class}" >/dev/null
java -jar ../../../../../scripts/stackmap_gen.jar "jvml/generated/${generated_class}.class"
java "jvml/generated/${generated_class}"
cd ..
rm -rf tmp/