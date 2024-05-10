# JVML - Optimising functional programming on the JVM!

## Installation Instructions

1. Install [opam](https://opam.ocaml.org/doc/Install.html)
2. run `opam switch create 4.14.0`
3. run `opam install . --deps-only`
4. Install [Krakatau](https://github.com/Storyyeller/Krakatau)
5. Install a copy of a JDK.

## Usage
```
> bash scripts/build_and_run.sh -h
_build/default/bin/compile.exe: unknown option '-h'.
compile -s <program_string> -f <file> <options>
  -s                Set string to compile
  -f                Set file to compile
  -c                Set name of generated class
  -o                Set output file
  -opt-all          Enable all optimisations
  -peep             Enable peephole optimisations
  -const-fp         Enable constant folding and propagation
  -inline           Enable inlining
  -inl-threshold    Adjust threshold for expression score in order to apply inlining. Default 10
  -tmm              Enable tail recursion modulo monoid
  -tco              Enable tail call optimisation
  -tmc              Enable tail recursion modulo cons
  -dyn-lambdas      Compile lambas using invokedynamic
  -peep-box         Enable boxing peephole optimisation
  -peep-push-pop    Enable push-pop peephole optimisation
  -peep-goto-label  Enable goto-label peephole optimisation
  -peep-store-load  Enable store-load peephole optimisation
  -dump_debug       Dump debug information (currently just compile times) to json file
  -help             Display this list of options
```

## Benchmarking Instructions

To run the benchmarking programs, additional dependencies are required:

mlton, moscow ml, polyml, sml/nj, hyperfine, maven, python, matplotlib and numpy.