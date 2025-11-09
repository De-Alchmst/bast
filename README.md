# The BAST programming language

BAST is an experimental programming language made for educational purchases.
It is currently only in alpha stage and is unable to do any real work, but it
implements enough to do basic calculations.

## features

- dynamic typing
- mix of procedural and functional programming
- unusual minimal syntax
- compiled to WASM

## build

As the compiler is written in [OCaml](https://ocaml.org/) one naturally needs
install the OCaml environment.
In addition, the compiler target is [MoonBit](https://www.moonbitlang.com/) so
the MoobBit CLI needs to be installed as well.

Once all the compilers are installed, the BAST compiler can be compiled with:

```
dune install
dune exec bin/main.exe
```
