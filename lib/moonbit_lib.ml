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

fn values_to_idexes(inds: Array[Value]) -> Array[Int] {
  let new : Array[Int] = []
  for ind in inds {
    match ind {
      Num(n) => new.push(n.to_int())
      _ => {
        println("read indices must be numbers, got \{inds}")
        panic()
      }
    }
  }
  new
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
  val_num_binop(mod, fn (x: Double, y:Double) -> Double {x*y}, "multiply", argv)
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

fn val_neg(arg: Value) -> Value {
  val_num_unop(fn (x: Double) -> Double {-x}, "negate", arg)
}

fn val_not(arg: Value) -> Value {
  Boo(!val_to_bool(arg))
}

// logic binop functions

// let's pray that it works
fn val_equal(argv: Array[Value]) -> Value {
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
          if !val_to_bool(val_equal([x[i], y[i]])) {
            eq = false
            break
          }
        }
        eq
      }
    }
    [Cons(x1, y1), Cons(x2, y2)] =>
       val_to_bool(val_and([val_equal([x1, x2]), val_equal([y1, y2])]))
    _ => false
  })
}

fn val_not_equal(argv: Array[Value]) -> Value {
  val_not(val_equal(argv))
}

fn val_a_not(argv: Array[Value]) -> Value {
  val_not(argv[0])
}

fn val_greater(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x>y},
                     "greater than", argv)
}

fn val_lower(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x<y},
                     "less than", argv)
}

fn val_greater_eq(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x>=y},
                     "greater than or equal to", argv)
}

fn val_lower_eq(argv: Array[Value]) -> Value {
  val_bool_num_binop(fn (x: Double, y: Double) -> Bool {x<=y},
                     "less than or equal to", argv)
}

fn val_and(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x && y}, "and", argv)
}

fn val_or(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x || y}, "or", argv)
}

fn val_xor(argv: Array[Value]) -> Value {
  val_bool_binop(fn (x: Bool, y: Bool) -> Bool {x != y}, "xor", argv)
}


// CONS


fn val_cons(argv: Array[Value]) -> Value { Cons(argv[0], argv[1]) }
fn val_car(arg: Array[Value]) -> Value {
  match arg[0] {
    Cons(h, _) => h
    _ => {
      println("car expects a cons cell, got \{arg}")
      panic()
    }
  }
}
fn val_cdr(arg: Array[Value]) -> Value {
  match arg[0] {
    Cons(_, t) => t
    _ => {
      println("cdr expects a cons cell, got \{arg}")
      panic()
    }
  }
}


fn cxr_to_func(cxr: String) -> (Array[Value]) -> Value {
  fn (argv: Array[Value]) -> Value {
    let mut ret = argv[0]
    for ch in cxr {
      if ch == 'a' {
        ret = val_car([ret])
      } else if ch == 'd' {
        ret = val_cdr([ret])
      } else {
        println("invalid cxr operation: \{cxr}")
        panic()
      }
    }
    ret
  }
}

// predicates

fn val_nil_p(argv: Array[Value]) -> Value {
  match argv[0] { Nil => Boo(true); _ => Boo(false) }
}

fn val_atom_p(argv: Array[Value]) -> Value {
  match argv[0] { Arr(_) => Boo(false); Cons(_, _) => Boo(false); _ => Boo(true) }
}

fn val_bool_p(argv: Array[Value]) -> Value {
  match argv[0] { Boo(_) => Boo(true); _ => Boo(false) }
}

fn val_num_p(argv: Array[Value]) -> Value {
  match argv[0] { Num(_) => Boo(true); _ => Boo(false) }
}

fn val_string_p(argv: Array[Value]) -> Value {
  match argv[0] { Str(_) => Boo(true); _ => Boo(false) }
}

fn val_array_p(argv: Array[Value]) -> Value {
  match argv[0] { Arr(_) => Boo(true); _ => Boo(false) }
}

fn val_func_p(argv: Array[Value]) -> Value {
  match argv[0] { Fun(_) => Boo(true); _ => Boo(false) }
}

fn val_cons_p(argv: Array[Value]) -> Value {
  match argv[0] { Cons(_, _) => Boo(true); _ => Boo(false) }
}

fn val_list_p(argv: Array[Value]) -> Value {
  match argv[0] {
    Nil        => Boo(true)
    Cons(_, x) => val_list_p([x])
    _          => Boo(false)
  }
}

// print 

fn value_to_string(arg: Value) -> String {
  match arg {
    Nil        => "nil"
    Num(n)     => n.to_string()
    Str(s)     => s
    Boo(b)     => b.to_string()
    Cons(h, t) => "(" + value_to_string(h) + " \\ " + value_to_string(t) + ")"
    Fun(_, _)  => "<function>"
    Arr(arr)   => {
      let mut s = "{"
      for i = 0; i < arr.length(); i = i + 1 {
        s += value_to_string(arr[i])
        if i < arr.length() - 1 {
          s += ", "
        }
      }
      s + "}"
    }
  }
}

fn val_to_string(argv: Array[Value]) -> Value {
  Str(value_to_string(argv[0]))
}

fn val_println(argv: Array[Value]) -> Value {
  println(value_to_string(argv[0]))
  Nil
}


fn arr_read(src: Value, indexs: Array[Int], offset: Int) -> Value {
  if indexs.length() <= offset {
    return src
  }
  let cur = indexs[offset]

  match src {
    Arr(arr) => {
      if cur < 0 || cur >= arr.length() {
        println("array index \{cur} out of bounds for array of length \{arr.length()}")
        panic()
      }
      arr_read(arr[cur], indexs, offset + 1)
    }
    _ => {
      println("read not implemented for type \{src}")
      panic()
    }
  }
}


fn arr_write(src: Value, indexs: Array[Int], offset: Int, new: Value) -> Value {
  if indexs.length() <= offset {
    return new
  }
  let cur = indexs[offset]

  match src {
    Arr(arr) => {
      if cur < 0 || cur >= arr.length() {
        println("array index \{cur} out of bounds for array of length \{arr.length()}")
        panic()
      }
      arr[cur] = arr_write(arr[cur], indexs, offset + 1, new)
      src
    }
    _ => {
      println("read not implemented for type \{src}")
      panic()
    }
  }
}


fn val_read(src: Value, indexs: Array[Value]) -> Value {
  let inds = values_to_idexes(indexs)
  match src {
    Arr(_) => { arr_read(src, inds, 0) }
    _ => {
      println("read not implemented for type \{src}")
      panic()
    }
  }
}


fn val_write(src: Value, indexs: Array[Value], new: Value) -> Value {
  let inds = values_to_idexes(indexs)
  match src {
    Arr(_) => { arr_write(src, inds, 0, new) }
    _ => {
      println("read not implemented for type \{src}")
      panic()
    }
  }
}

fn cons_len(cons: Value) -> Value {
  fn aux(acc: Double, cell: Value) -> Double {
    match cell {
      Nil        => acc
      Cons(_, x) => aux(acc + 1.0, x)
      _          => -1.0
    }
  }
  Num(aux(0.0, cons))
}


fn val_len(argv: Array[Value]) -> Value {
  match argv[0] {
    Nil        => Num(0.0)
    Str(s)     => Num(s.length().to_double())
    Arr(arr)   => Num(arr.length().to_double())
    Cons(_, _) => cons_len(argv[0])
    _ => {
      println("len not implemented for \{argv[0]}")
      panic()
    }
  }
}

// join

fn list_join(l1: Value, l2: Value) -> Value {
  match l1 {
    Nil => l2
    Cons(h, t) => Cons(h, list_join(t, l2))
    _ => {
      println("Cannot join to CONS ending in \{l1}")
      panic()
    }
  }
}

fn val_join(argv: Array[Value]) -> Value {
  match argv[0] {
    Str(s) => Str(s + value_to_string(argv[1]))
    Cons(_, _) | Nil =>
      match argv[1] {
        Cons(_, _) | Nil => list_join(argv[0], argv[1])
        _ => {
          println("Cannot join CONS with \{argv[1]}")
          panic()
        }
      }

    Arr(arr1) => 
      match argv[1] {
        Arr(arr2) => Arr(arr1.add(arr2))
        _ => {
          println("Cannot join ARRAY with \{argv[1]}")
          panic()
        }
      }

      _ => {
        println("Cannot join to \{argv[0]}")
        panic()
      }
    
  }
}

// array stuff

fn val_array_make(argv: Array[Value]) -> Value {
  match argv[0] {
    Num(n) => Arr(Array::make(n.to_int(), argv[1]))
    _ => {
      println("array-make expects a number as first argument, got \{argv[0]}")
      panic()
    }
  }
}

|}
