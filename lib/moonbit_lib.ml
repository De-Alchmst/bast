let src = {|
// DYNAMIC TYPES //

enum Value {
  Nil
  Num(Double)
  Str(String)
  Boo(Bool)
  Arr(Array[Value])
  Fun((Array[Value]) -> Value, Int) // func, arity
  Cons(Value, Value)
  Err
} derive(Show)

struct Var {
  name: String
  mut val: Value
} derive(Show)

enum Mod {
  NoMod
  UpTo(Value)
  DownTo(Value)
  ModTo(Value)
  InRange(Value, Value)
  LoopInRange(Value, Value)
}

fn ass_var(ass: Var, new: Value) -> Value {
  ass.val = new
  ass.val
}

fn pos_ass_var(ass: Var, new: Value) -> Value {
  let old = ass.val
  ass.val = new
  old
}

fn min(n1: Double, n2:Double) -> Double { if n1 < n2 { n1 } else { n2 } }
fn max(n1: Double, n2:Double) -> Double { if n1 > n2 { n1 } else { n2 } }


fn val_to_bool(x: Value) -> Bool {
  match x {
    Boo(b) => b
    _      => true
  }
}


fn call_val_func(name: String, fun: (Array[Value]) -> Value, arity: Int, argv: Array[Value]) -> Value {
  if arity != argv.length() {
    println("Wrong number of arguments for \{name}, expected \{arity}, but got \{argv.length()}")
    panic()
  } else {
    fun(argv)
  }
}

// FUNCTION CALL WRAPPERS //

fn call_var_func(fun: Var, argv: Array[Value]) -> Value {
  match fun.val {
    Fun(f, arity) => call_val_func(fun.name, f, arity, argv)
    _ => {
      println("\{fun.name} is not a function!")
      panic()
    }
  }
}


fn num_up_to(res: Double, upto: Double) -> Double {
  if res < upto { res } else { upto }
}


fn num_down_to(res: Double, downto: Double) -> Double {
  if res > downto { res } else { downto }
}

fn num_in_range(res: Double, n1: Double, n2: Double) -> Double {
  let mi = min(n1, n2)
  let ma = max(n1, n2)

  if      res > ma { ma  }
  else if res < mi { mi  }
  else             { res }
}


// could probably be optimised, but whatever...
fn num_loop_in_range(res: Double, n1: Double, n2: Double) -> Double {
  let mi = min(n1, n2)
  let ma = max(n1, n2)
  
  if res < mi {
    num_loop_in_range(ma - mi + res + 1, mi, ma)
  } else if res > ma {
    num_loop_in_range(mi - ma + res - 1, mi, ma)
  } else{
    res
  }
}


fn val_num_binop(mod: Mod, f: (Double, Double) -> Double, cannot: String, argv: Array[Value]) -> Value {
  match argv {
    [Num(x), Num(y)] => {
      let res = f(x, y)
      match mod {
        NoMod => Num(res)
        UpTo(Num(z))   => Num(num_up_to(res, z))
        DownTo(Num(z)) => Num(num_down_to(res, z))
        ModTo(Num(z))  => Num(res % z)
        InRange(Num(z), Num(w)) => Num(num_in_range(res, z, w))
        LoopInRange(Num(z), Num(w)) => Num(num_loop_in_range(res, z, w))
        _ => {
          println("cannot bind upto \{x} to \{cannot}")
          panic()
        }
      }
    }
    [x, y] => {
      println("cannot \{cannot} values of type: \{x}, \{y}")
      panic()
    }
    _ => {
      println("invalid number of arguments for \{cannot}, expected 2, got \{argv.length()}")
      panic()
    }
  }
}


fn val_bool_num_binop(f: (Double, Double) -> Bool, cannot: String, argv: Array[Value]) -> Value {
  match argv {
    [Num(x), Num(y)] => Boo(f(x, y))
    [x, y] => {
      println("cannot \{cannot} values of type: \{x}, \{y}")
      panic()
    }
    _ => {
      println("invalid number of arguments for \{cannot}, expected 2, got \{argv.length()}")
      panic()
    }
  }
}


fn val_bool_binop(f: (Bool, Bool) -> Bool, cannot: String, argv: Array[Value]) -> Value {
  match argv {
    [x, y] => {
      Boo(f(val_to_bool(x), val_to_bool(y)))
    }
    _ => {
      println("invalid number of arguments for \{cannot}, expected 2, got \{argv.length()}")
      panic()
    }
  }
}


fn val_num_unop(f: (Double) -> Double, cannot: String, argv: Value) -> Value {
  match argv {
    Num(x) => Num(f(x))
    x => {
      println("cannot \{cannot} value of type \{x}")
      panic()
    }
  }
}


fn call_fun(fun: Value, argv: Array[Value]) -> Value {
  match fun {
    Fun(f, arity) => call_val_func("λ", f, arity, argv)
    _ => {
      println("\{fun} is not a function!")
      panic()
    }
  }
}

// DYNAMIC FUNCTIONS //

// binops

fn op_val_add(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {x+y}, "add", argv)
}

fn op_val_sub(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {x-y}, "substract", argv)
}

fn op_val_mul(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {x-y}, "multiply", argv)
}

fn op_val_mod(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {x%y}, "modulo", argv)
}

fn op_val_div(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {
      if y == 0 {
        println("Division by zero is not implemented yet, might get to it later...")
        panic()
      } else {x/y}
  }, "divide", argv)
}

fn op_val_div_remles(mod: Mod, argv: Array[Value]) -> Value {
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {
      if y == 0 {
        println("Division by zero is not implemented yet, might get to it later...")
        panic()
      } else {(x/y).to_int().to_double()}
  }, "divide remainlessly", argv)
}

// as standalone functions
fn val_add(argv: Array[Value]) -> Value { op_val_add(NoMod, argv) }
fn val_sub(argv: Array[Value]) -> Value { op_val_sub(NoMod, argv) }
fn val_mul(argv: Array[Value]) -> Value { op_val_mul(NoMod, argv) }
fn val_div(argv: Array[Value]) -> Value { op_val_div(NoMod, argv) }
fn val_mod(argv: Array[Value]) -> Value { op_val_mod(NoMod, argv) }
fn val_div_remles(argv: Array[Value]) -> Value { op_val_div_remles(NoMod, argv) }

// unops

fn val_minus(arg: Value) -> Value {
  val_num_unop(fn (x: Double) -> Double {-x}, "negate", arg)
}

fn val_plus(arg: Value) -> Value {
  // ah yes, usefulness
  val_num_unop(fn (x: Double) -> Double {x}, "nop?", arg)
}

// logic binop functions

// let's pray that it works
fn op_val_equal(argv: Array[Value]) -> Value {
  if argv.length() != 2 {
    println("equal function expects exactly 2 arguments, got \{argv.length()}")
    panic()
  }
  Boo(match argv {
    [Nil, Nil] => true
    [Num(x), Num(y)] => x == y
    [Str(x), Str(y)] => x == y
    [Boo(x), Boo(y)] => x == y
    [Arr(x), Arr(y)] => {
      if x.length() != y.length() {
        false
      } else {
        let mut eq = true
        let len = x.length()
        for i = 0; i < len; i = i + 1 {
          if !(match op_val_equal([x[i], y[i]]) {
            Boo(b) => b
            _ => false
          }) {
            eq = false
            break
          }
        }
        eq
      }
    }
    [Cons(x1, y1), Cons(x2, y2)] =>
         match op_val_and([op_val_equal([x1, x2]), op_val_equal([y1, y2])]) {
            Boo(b) => b
            _ => false
        }
    _ => false
  })
}


fn val_not(arg: Value) -> Value {
  Boo(!val_to_bool(arg))
}


fn op_val_greater(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x>y},
                     "greater than", argv)
}

fn op_val_less(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x<y},
                     "less than", argv)
}

fn op_val_greater_eq(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x>=y},
                     "greater than or equal to", argv)
}

fn op_val_less_eq(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x<=y},
                     "less than or equal to", argv)
}

fn op_val_and(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x && y}, "and", argv)
}

fn op_val_or(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x || y}, "or", argv)
}

fn op_val_xor(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x != y}, "xor", argv)
}

|}
