mkdir tmp/
dune exec _build/default/bin/compile.exe > tmp/test.j
krak2 asm --out "tmp/Foo.zip" tmp/test.j
cd tmp
unzip -u Foo
java Foo
cd ..
rm -rf tmp/