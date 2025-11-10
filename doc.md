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

### The basics (variables and functions)

Here is a mandatory "hello, world!" program:

```
; this is a comment
func [main]:[
  [println "Hello, World!"]
]
```

As one can see, BAST uses S-expression-like syntax when calling functions.
I say "hello, world!", as for implementation reasons, all strings are converted
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

Here, `N` is a shorthand for `nil`. BAST allows a fair share of alternative for
some keywords.
It also reserves all single-letter names for special use, so you can't name
variables things like `x` or `i`, but you need to get a bit more creative.
Variables usually use kebab-case, but they cannot either start nor end with `-`.

To declare functions that take arguments, just simply write their names after
the function's name.

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
This actually creates an infix operator by binding `=` to a variable name and it
returns the newly assigned value, but more on that later.

The next are that variables are dynamically typed, that math is done with infix
operators, as is usual in ALGOL-like languages, and that BAST do not
differentiate between whole and decimal numbers.

Due to poor syntax design, unary minus, also used for negative literals, is
written using the `\`` symbol.
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
- `== ` : deep comparison
- `<=>` : identity check
- `&&`  : logical and
- `and` : logical and
- `||`  : logical or
- `or`  : logical or
- `^^`  : logical xor
- `xor` : logical xor

All operators (except word `and`,  `or` and `xor` variants) can also have a
function counterpart, so the previous code can be also written like so.

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
together. If the left-hand side is a string, the right-hand side is
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
By binding a less-than (`<`) and a numeric expression to an operator, one
prevents the result from exceeding the specified value.
Binding greater-than (`>`) prevents the result from being lower than the value
instead.
Binding modulo (`%`) applies modulo of the given value to the result.

```
func [main]:[
  [println 10 +:<:15 10] ; result is 15
  [println 10 +:>:25 10] ; result is 25
  [println 10 +:%:2  10] : result is 0
]
```

To specify both upper and lower bounds, an "interval" form can be used:
`<`, lower bound, upper bound `>`.
By adding module before the interval, the result will loop in the interval,
 instead of being stopped at the bounds.

```
func [main]:[
  [println 10 +:<:5:15:> 10]   ; result is 15
  [println 10 +:%:<:5:15:> 10] ; result is 9
]                              ; (15 and 5 left, which loops to 5→6→7→8→9)
```

For simplicity, when only expression is provided, it is interpreted as a
upper bound.

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
Zero, empty lists and similar are all true.

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
The `while` loop takes a condition block, followed by code block, which will be
executed for as long as the condition is true.
In addition, it can also have a variable block just like functions.
All variables are initialised anew each iteration.
Just like `if`, it has a `until` counterpart.
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
  [println ; prints 1, even the the expression wasn not ever true
    do-while [foo > 1]:[
      foo:++
    ]
  ]
]
```

If a loop does not get executed at all, it returns `Nil`.

Next is the `for` loop.
The for loop is special, because it declares and iterates over a local variable.
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
It does not take condition and it iterates endlessly.
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

In addition, there is one similar expression named `do`, `block`, `blck` or
`blk`.
It does not take condition, and it executes exactly once.
Therefor it is technically not a loop, but it is similar enough.

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

### Iterables

## Implementation
