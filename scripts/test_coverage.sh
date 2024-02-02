#!/bin/bash

find . -name '*.coverage' | xargs rm -f
rm -rf _coverage/
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
bisect-ppx-report summary
open _coverage/index.html