# BAST documentation

## Abstract

BAST is an experimental programming language that I made in order to learn some
basics of
compiler development in OCaml and to experiment with novel syntax ideas.
The main goals with the syntax design is to keep it clean and aesthetically
pleasing.

It aims to provide basic tools for both procedural and functional programming
and allow mixing both approaches as seem fitting.

The original goal was to target the
[WASM-4](https://wasm4.org/)
platform, but that ended up being out of the project scope for now.

The original syntax design turned out being a bit too lax, and the parser had
problems with handling it, so some corners had to be cut.
Main artefacts of this are the `read`/`write` constructs and `` ` `` for
unary minus.

Consider this to be just a demo of what could have been.

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

### The basics (variables and functions)

Here is a mandatory "hello, world!" program:

```
; this is a comment
func [main]:[
  [println "Hello, World!"]
]
```

As one can see, BAST uses S-expression-like syntax when calling functions.
I say "hello, world!", since for implementation reasons all strings are converted
to lower-case.
In fact, all of BAST is case insensitive, so the following code does the exact
same thing.

```
; THIS IS A COMMENT
FUNC [MAIN]:[
  [println "HELLO, WORLD!"]
]
```

Only two kinds of statements are allowed at top-level: `func` and `var`.

```
var [
  message:"hello, world!"
]

func [main]:[
  [println message]
]
```

When declaring a variable, one must specify it's initial value.
The `:` is used all over BAST to group syntax together.
It is referred to as "bind", so we say that `var` expects a block of variable
names with their initial values bound to them.
If the `var` block contains only one declaration, the brackets can be omitted.

Variables can also be declared inside function body.

```
func [main]:[
  var message:"hello, world!"

  [println message]
]
```

When declaring variables inside a function, the `var` part can be omitted by
binding an additional declaration block before the function body.

```
func [main]:[
  foo:N
  bar:nil
  message:"hello, world!"
]:[
  [println message]
]
```

Here, `N` is a shorthand for `nil`. BAST allows alternative forms for some
keywords.
It also reserves all single-letter names for special use, so you can't give
variables names like `x` or `i`.
You need to get a bit more creative.
Variables usually use kebab-case, but they cannot neither start nor end with `-`.
`_` is not allowed in function names, as it is reserved for future use.

To declare a functions that takes arguments, just simply write the argument
names after the function's name.

```
func [print2 aa bb]:[
  [println aa]
  [println bb]
]

func [main]:[
  [print2 "Hello" "World"]
]
```

The `func` statement only works in top-level.
To declare local function, use a lambda.
Lambda can be declared with any of the following keywords:
`lambda`, `lamb`, `λ`.


```
func [main]:[
  aux : lamb [aa bb]:[
    [println aa]
    [println bb]
  ]
]:[
  [aux "Hello" "World"]
]
```

### Numbers and operators

Now lets get to numbers

```
func [main]:[
  foo:3
  bar:0.7
  baz:N
]:[
  baz := (foo + bar) * 10
  [println baz]
]
```

This example demonstrates multiple things.
First, that variables are assigned with `:=`.
This actually creates an unary operator by binding `=` to a variable name and it
returns the newly assigned value, but more on that later.

Variables are dynamically typed.
Math is done with infix operators, as is usual in ALGOL-like languages
And lastly, BAST does not differentiate between whole and decimal numbers.

Due to poor syntax design, unary minus, also used for negative literals, is
written using the `` ` `` symbol.
Otherwise it wouldn't be possible to tell whether an expression starts with an
unary minus, or if the previous expression continues with a binary one.

Here is a list of all the infix operators:

- `-`   : substraction
- `+`   : addition
- `*`   : multiplication
- `/`   : division
- `%`   : modulo
- `^`   : exponent
- `//`  : remainless division
- `~`   : concatenation
- `=`   : equals
- `!=`  : not equals
- `<>`  : not equals
- `<`   : less than
- `<=`  : less or equal than
- `>`   : greater than
- `>=`  : greater or equal than
- `&&`  : logical and
- `and` : logical and
- `||`  : logical or
- `or`  : logical or
- `^^`  : logical xor
- `xor` : logical xor
- `\`   : cons (right associative)

All operators (except word `and`,  `or` and `xor` variants) also have a
function counterpart, created by prepending them with "f",
so the previous code can be also written like so:

```
func [main]:[
  foo:3
  bar:0.7
  baz:N
]:[
  baz := [f* [f+ foo bar] * 10]
  [println baz]
]
```

The concatenation operator (`~`) can be used to join strings, lists and arrays
together. If the left hand side is a string, the right hand side is
automatically converted to string as well.

```
func [main]:[
  [println "1 + 1 = " ~ (1+1)]
]
```

All functions automatically return the result of the last statement of their
body.

```
func [add2 num]:[
  num + 2
]

func [main]:[
  [println "the answer is: " ~ [add2 40]]
]
```

Arithmetic operators can be further modified by adding special constraints to
them.
By binding a less-than (`<`) and a numeric expression to an operator
prevents the result from exceeding the specified value.
Binding greater-than (`>`) prevents it from being lower instead.
Binding modulo (`%`) applies modulo of the given value to the result.

```
func [main]:[
  [println 10 +:<:15 10] ; result is 15
  [println 10 +:>:25 10] ; result is 25
  [println 10 +:%:2  10] : result is 0
]
```

To specify both upper and lower bounds, an "interval" form can be used by
binding:
`<`, lower bound, upper bound, `>`.
By adding modulo before the interval, the result will loop in the interval,
 instead of being stopped at the bounds.

```
func [main]:[
  [println 10 +:<:5:15:> 10]   ; result is 15
  [println 10 +:%:<:5:15:> 10] ; result is 9
]                              ; (15 and 5 left, which loops to 5 -> 6 -> 7 -> 8 -> 9)
```

For simplicity, when only an numeric expression is provided, it is interpreted
as an upper bound.

Any arithmetic operator, including modified ones, and concatenation can be bound
to a variable instead of `=`.
This acts like the `<operaotr>=` syntax in C-like languages.
Just like regular assignment, it is an expression, thus it returns a value. 

```
func [main]:[
  foo:3 bar:7
]:[
  foo := bar :* 6 ; sets both foo and bar to (bar * 6)
]
```

There are also alternatives to C's `++` and `--`. Those can be written either
also as `++` and `--`, or as `inc`, and `dec`.
Binding them to a variable adds/subtracts one to/from it and then returns it's
new value.
By binding the variable to the operator (can also be said as prebinding the
operator to the variable), the old value before modification is returned instead.

```
func [main]:[
  foo:0
]:[
  [println foo:++]  ; prints 1
  [println inc:foo] ; prints 1
  [println foo]     ; prints 2
]
```

### Control flow

The infix operators operators have a lot of comparison and logical operators,
which can be used in conditionals and loops.

Lets start with conditionals.
The `if` expression can take two forms.
First of it's blocks always contains a logical expression.
Only false expression is the `false` value.
Zero, nil, empty array and similar values are all true.

The second block is a code block that will be executed when the expression is
true, and it's last expression is returned as per usual.
Under false condition, nothing happens and `Nil` is returned.
If Two code blocks are provided, the first executes under true conditions and
the second one under false conditions.

```
func [main]:[
  [println
    if [2 + 2 = 5]:[
      "true"
    ]:[
      "false"
    ]
  ]
]
```

Expression's logical value can be negated with the `!` or `not` unary operator.
There is also the `unless` expression, which acts like `if`, but negates the
condition automatically

```
func [main]:[
  [println
    unless [not 2 + 2 = 5]:[
      "true"
    ]:[
      "false"
    ]
  ]
]
```

Next, there are the loops.
The `while` loop takes a condition block, followed by a code block, which will
be executed for as long as the condition is true.
In addition, it can also have a variable block, just like a function.
All variables in the declaration block are initialised anew each iteration.
Just like `if`, it has the `until` counterpart.
As per usual, the value of the last expression in the last iteration is returned

```
func [main]:[
  foo:0
]:[
  [println ; prints 5
    while [foo < 5]:[
      foo:++
    ]
  ]
]
```

The `while` and `until` expressions also have `do-while` and `do-until`
variants, which act the same, but ignore the expression for the first iteration.

```
func [main]:[
  foo:0
]:[
  [println ; prints 1, even tho the expression was never true
    do-while [foo > 1]:[
      foo:++
    ]
  ]
]
```

If a loop does not get executed at all, it returns `Nil`.

Next is the `for` loop.
The `for` loop is special, as it declares and iterates over a local variable.
The head of a `for` loop first expects a name for the iterator. Then it takes
two numeric expressions, first one lower than the other, separated by an arrow.
The arrow can take four forms:

- `->` - iterates from the left  side (included) to the right side (included)
- `<-` - iterates from the right side (included) to the left  side (included)
- `=>` - iterates from the left  side (included) to the right side (excluded)
- `<=` - iterates from the right side (excluded) to the left  side (included)

```
func [main]:[
  foo:""
]:[
  for [ind 0 <= 10]:[
    foo := foo ~ ind ~ " "
  ]
  [println foo] ; "9 8 7 6 5 4 3 2 1 0 "
]
```

Last is the creatively named `loop` loop.
It does not take a condition and it iterates endlessly.
How to escape from it then?
Well, there comes the `return` statement, which exits from a function early.

```
func [main]:[
  foo:0
]:[
  loop [
    [println ++:foo]
    unless [foo < 10]:[return Nil]
  ]
]
```

In addition, there is one similar expression named either `do`, `block`, `blck`
or `blk`.
It does not take condition, and it executes exactly once.
Therefor it is technically not a loop, but it is similar enough to put here.

```
func [main]:[
  [println
    do [
      foo:N
      bar:7
    ]:[
      foo := (bar - 1) * 5
      foo + bar
    ]
  ]
]
```

### Arrays and Lists

BAST has two primary data structures: arrays and singly linked lists.

Lets start with arrays.
Arrays are created with curly braces.

```
func [main]:[
    [println {"hello," "world!"}]
]
```

Arrays are indexed from 0.
Data can be read from an array with the `read` or `r` expression.
Read takes a block containing the array itself and index of the element to read.
To access nested arrays, multiple indices can be put inside one `read`.
To write data to an array, use the `write` or `r` expression.
In addition to the array and indices, it also takes the value to write.

```
func [main]:[
  foo : {
    {1 0 3}
    {4 5 6}
    {7 8 9}
  }
]:[
  w [foo 0 1 2]
  [println r [foo 0 1]] ; 2
]
```

Multiple functions for array manipulation exist.
The basic ones are:

- `[push arr val]` - returns a copy of `arr` with `val` appended to the end
- `[pop arr]`  - returns a copy of `arr` without it's last element
- `[insert arr ind val]` - returns a copy of `arr` with `val` inserted to index `ind`
- `[remove arr ind]` - returns a copy of `arr` without the value at index `ind`
- `[push! arr val]` - adds `val` to the end of `arr` and returns `val`
- `[pop! arr]`  - removes the last value from `arr` and returns it
- `[insert! arr ind val]` - inserts `val` to `arr` at the index `ind` and returns it
- `[remove! arr ind]` - removes the value at index `ind` from `arr` and returns it

```
func [main]:[
  arr:{1 2 3}
]:[
  
  [push! arr [remove! arr 1]]
  [println arr] ; {1 3 2}
]
```

Lists can be formed in three ways.
First is with the `list` expression.
Since lists are formed from
[cons cells](https://cs.gmu.edu/~sean/lisp/cons/),
the second one is the cons (`\`) operator.
Last option is using the `cons` function.
Note that when creating lists manually via consing, one needs to provide the
`Nil` value at the end.
Cons chain ending in anything but `nil` is not a valid list.

```
func [main]:[
  foo : list [1 2 3]
  bar : 1\2\3\N
  baz : [cons 1 [cons 2 [cons 3 N]]]
]:[
  [println foo \ bar \ baz] ; not a list, but a valid data structure
]
```

Cons cells can be traversed with the `car` and `cdr` functions.

```
func [main]:[
  example : "a" \ "b"
]:[
  [println [car example]] ; "a"
  [println [cdr example]] ; "b"
]
```

Just like in LISP, `car`s and `cdr`s can be chained together.

```
func [main]:[
  example : list [ 1\2\N 3\4\N 5\6\N ]
]:[
  [println [cadadr example]]
  ; same as
  [println [car [cdr [car [cdr example]]]]]
]
```

Unlike LISP, these composed functions can be of arbitrary length.
Since it can be easier to think about such expressions from the back, these
chained functions can also be written in reverse

```
func [main]:[
  example : list [ 1\2\N 3\4\N 5\6\N ]
]:[
  [println [rdadac example]]
  ; still same as same as
  [println [car [cdr [car [cdr example]]]]]
]
```

Both lists and arrays can be measured with the `len` function.

```
func [main]:[
  lst : 1\2\3\N
  arr : {1 2 3}
]:[
  [println [len lst] = [len arr]]
]
```

### +> block syntax

BAST uses quite a lot of square brackets.
Trailing brackets tend to group at the end of blocks, which does not
look particularly nice.
To combat this, the `+>` block syntax was introduced.

`+>` acts syntactically the same as if everything from it to the end of the
parent block was enclosed in extra set of brackets.
It is easier to show on an example.

```
func [main]:[
  data : {1\2\3\N  2\3\4\N  3\4\5\N}
  aux  : lamb [acc lst]:+>
    cond
      [+> nil? lst]:[acc]
     :[T]:+>+> aux acc + [car lst] [cdr lst]
]:[
  if [+> array? data]:+>
    for [ind 0 => +> len data]:+>
      +> println +> aux 0 r +> data ind
]
```

This code is the same as:

```
func [main]:[
  data : {1\2\3\N  2\3\4\N  3\4\5\N}
  aux  : lamb [acc lst]:[
    cond
      [[nil? lst]]:[acc]
     :[T]:[[aux acc + [car lst] [cdr lst]]]
  ]
]:[
  if [[array? data]]:[
    for [ind 0 => +> len data]:[
      [println [aux 0 r [data ind]]]
    ]
  ]
]
```

Yes, I admit, this is a bit excessive.
You would usually not use `+>`to this extend, but it is a nice demonstration
of `+>`
being really just a syntactic sugar and working everywhere brackets would.
It is usually used for when calling a function as a condition or for
functions/blocks in the return position.

More balanced version could look something like so:

```
func [main]:[
  data : {1\2\3\N  2\3\4\N  3\4\5\N}
  aux  : lamb [acc lst]:[
    cond
      [+> nil? lst]:[acc]
     :[T]:[+> aux acc + [car lst] [cdr lst]]
  ]
]:[
  if [+> array? data]:+>
    for [ind 0 => +> len data]:+>
      [println [aux 0 r [data ind]]]
]
```

### Built-in functions 

There are some additional functions, that didn't fit anywhere else.
Note that `iter` can be either an array, or a list.

- `[num? val]` - is `val` a number?
- `[string? val]` - is `val` a string?
- `[bool? val]` - is `val` a boolean?
- `[func? val]` - is `val` a function?
- `[cons? val]` - is `val` a cons cell?
- `[nil? val]` - is `val` a `Nil`?
- `[list? val]` - is `val` a list? (cons chain ending in `Nil`, just `Nil` is also a valid list)
- `[array? val]` - is `val` an array?
- `[atom? val]` - is `val` anything but array or cons cell?
- `[split str sep]` - returns a list of all substrings of `str` separated by `sep`
- `[chars str]` - returns a list of strings, each being one character of `str`
- `[arity fn]` - returns the number of arguments `fn` takes
- `[panic val]` - prints `val` and exits the program
- `[to-string val]` or `[2string val]` -  returns a pretty string representing `val`
- `[to-debug val]` or `[2debug val]` - returns a internal representation of the `val`
- `[array-make num val]` or `[arr-make num val]` - returns a new array of `num` elements, all having the value `val`
- `[rev iter]` - returns the reversed version of `iter`
- `[map iter fn]` - returns a version of `iter` where each element is it's value, after applying `fn` to it
- `[indmap iter fn]` - just like `map`, but `fn` first takes an index, and then the value
- `[automap iter fn]` - either `map` or `indmap`, depending on `fn`'s arity
- `[filter iter]` - returns a version of `iter` with only the elements, whose value is true after applying `fn` to them
- `[foldl iter val fn]` - left fold, see [https://en.wikipedia.org/wiki/Fold_(higher-order_function)](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
- `[foldr iter val fn]` - right fold, see [https://en.wikipedia.org/wiki/Fold_(higher-order_function)](https://en.wikipedia.org/wiki/Fold_(higher-order_function))

## Implementation

At the core of most compilers stands the lexer, and the parser.
For lexer, I am using
[ocamllex](https://ohama.github.io/ocaml/ocamllex-tutorial/).
All lexer code can be found in
[lib/lexer.mll](lib/lexer.mll).
I decided that the best implementation of `+>` would be to replace it with
brackets at lex-time.
To do this I will need some non-standard lexer functionality.

I need matches to be able to return multiple values, which I implemented using
a queue.
Lexer looks if there are any tokens in the queue.
If so, it pops one and returns it, else it lexes like usual, which might add
some tokens to the queue to be returned next.

I also need some sort of counting of how many brackets to close.
This I implemented with a stack.
Each opening bracket pushes a new value to the stack and closing one returns a
number of closing bracket tokens based on that value.
All `+>` needs to do is to add to the number at the top of the stack and return
opening a bracket token.

As far as parser goes, it is pretty standard.
It uses [Menhir](https://gitlab.inria.fr/fpottier/menhir)
and can be found in
[lib/parser.mly](lib/parser.mly).

The parser does allow only some types of statements at top-level.
That is a good thing, but it also makes writing tests for it annoying.
That is why in addition to `prog`, it also exports the `prog_debug` function,
which does not have any such limitation.
The tests themselves can be found in
[test/parser_test.ml](test/parser_test.ml).

The parser generates AST (available in [lib/ast.ml](lib/ast.ml)), which is then
passed to codegen.
Before that happens, tho, the configuration file is red in
[lib/moonbit_conf.ml](lib/moonbit_conf.ml)
and compilation directory is created/updated from
[lib/moonbit_project](lib/moonbit_project).
The compiler creates a directory named `_BAST_work_dir` in the source directory,
where it puts all the MoonBit stuff, including the generated source code.
Some files in `_BAST_work_dir` only need to be written once, as they change only
with new compiler version.
For this reason, the compiler tracks it's current version and stores it in then
`_BAST_work_dir/compiler.version` file, so that it knows when to rewrite them.

When the `_BAST_work_dir` skeleton is complete, code generation can start.
[lib/moonbit_codegen.ml](lib/moonbit_codegen.ml)
is not all that interesting, as it is mostly just some minimal pattern matching
on the ATS.
The interesting stuff happens in
[lib/moonbit_lib.ml](lib/moonbit_lib.ml).

This file contains MoonBit source code which implements both basic dynamic
typing and all the basic functions and operators.
Values are stored as an enum type.
Variables are a struct, which holds it's name and a value.
The name was supposed to be used for error messages, but as variable itself is
rarely passed around, it only comes to play when a variable is called as a
function.

Functions are implemented as MoonBit functions taking a list of arguments, with
it's arity stored in the enum value.
They are called with intermediary functions, which check for arity.

For numeric binary operators, special wrapper function `val_num_binop` exists,
which checks that both arguments are numbers and applies any modifiers if
specified.

Due to the way cons cells are implemented (a simple enum entry of two values),
and the way arguments are passed around (by value), writing to a cons cell
directly is currently not supported.
This could be solved by making it it's own structure, to which the enum value
only holds a reference to.
It was, however, not important enough to implement yet.

In addition to
[lib/moonbit_lib.ml](lib/moonbit_lib.ml),
there is also
[lib/bast_lib.ml](lib/bast_lib.ml),
which implements more advanced functions in BAST itself.
Due to this, it needs to be compiled like a file, but it still recompiles only
on version changes.
