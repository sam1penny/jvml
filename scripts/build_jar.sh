#!/bin/bash

copy_args=( "$@" )

generated_class="Foo"
while :; do
    case $1 in
        -c)
        if [ "$2" ]; then
            generated_class=$2
            shift
        else
            printf 'error: "-c" requires an argument\n'
            exit 1
        fi
        ;;
        -?*)
        shift
        ;;
        *) break

    esac
    shift
done

mkdir tmp/
dune exec -- _build/default/bin/compile.exe "${copy_args[@]}" -o tmp/tmp.j || exit 1
krak2 asm --out "tmp/${generated_class}.zip" tmp/tmp.j > /dev/null
cd tmp
unzip -u "${generated_class}" >/dev/null
java -jar ../scripts/stackmap_gen.jar "jvml/generated/${generated_class}.class"
jar cf "../${generated_class}.jar" jvml/generated/*.class
cd ..
rm -rf tmp/