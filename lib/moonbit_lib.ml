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


fn val_num_binop(f: (Double, Double) -> Double, cannot: String, argv: Array[Value]) -> Value {
  match argv {
    [Num(x), Num(y)] => Num(f(x, y))
    [x, y] => {
      println("cannot \{cannot} values of type: \{x}, \{y}")
      panic()
    }
    _ => panic()
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

fn val_add(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {x+y}, "add", argv)
}

fn val_sub(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {x-y}, "substract", argv)
}

fn val_mul(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {x-y}, "multiply", argv)
}

fn val_mod(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {x%y}, "modulo", argv)
}

fn val_div(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {
      if y == 0 {
        println("Division by zero is not implemented yet, might get to it later...")
        panic()
      } else {x/y}
  }, "divide", argv)
}

fn val_div_remles(argv: Array[Value]) -> Value {
  val_num_binop(fn (x: Double, y:Double) -> Double {
      if y == 0 {
        println("Division by zero is not implemented yet, might get to it later...")
        panic()
      } else {(x/y).to_int().to_double()}
  }, "divide remainlessly", argv)
}

// unops

fn val_minus(arg: Value) -> Value {
  val_num_unop(fn (x: Double) -> Double {-x}, "negate", arg)
}

fn val_plus(arg: Value) -> Value {
  // ah yes, usefulness
  val_num_unop(fn (x: Double) -> Double {x}, "nop?", arg)
}

|}
