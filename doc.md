# BAST documentation

## Abstract

BAST is an experimental language that I made in order to learn some basics of
compiler development in OCaml and to experiment with novel syntax ideas.
The main goals with the syntax design is to keep it clean and aesthetically
pleasing.

It aims to provide basic tools for both procedural and functional programming
and allow mixing both approaches as seem fitting.

The original idea was to target the
[WASM-4](https://wasm4.org/)
platform, but that ended up being out of the project scope for now.

The original syntax design turned to be a bit too lax, and the parser had
problems with handling it, so some corners had to be cut.
Main artefacts of this are the `read`/`write` constructs and `\`` for
unary minus.

Consider this to be just a demo.

## Compiler usage

After building the compiler, it can be invoked from the command line with no
arguments to compile the current directory, or the directory to compile can be
passed as an argument.
After compilation, the program is automatically executed as well.
To run the executable again without recompiling, use the `moonrun` command.

All files in the source directory ending in `.bast`, `.bst`, `.☥` or `.𓋹` are
considered source files.
All functions and variables from one file can be freely accessed by the rest,
which also means that no two files can both define a top-level variable or
function of the same name.

By default, no program entry point is set.
To specify an entry point, add the `bast.conf` file to the source directory.
Each line of the configuration file is interpreted as a key-value pair separated
by `:`.
To set the entry point to a function named `main`, do:

```
entry : main
```

## Language tutorial



## Implementation
