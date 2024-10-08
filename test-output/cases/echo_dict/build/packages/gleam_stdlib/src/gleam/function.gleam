@deprecated("Use a fn literal instead, it is easier to understand")
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}

@deprecated("Use the anonymous function syntax instead")
pub fn curry2(fun: fn(a, b) -> value) {
  fn(a) { fn(b) { fun(a, b) } }
}

@deprecated("Use the anonymous function syntax instead")
pub fn curry3(fun: fn(a, b, c) -> value) {
  fn(a) { fn(b) { fn(c) { fun(a, b, c) } } }
}

@deprecated("Use the anonymous function syntax instead")
pub fn curry4(fun: fn(a, b, c, d) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fun(a, b, c, d) } } } }
}

@deprecated("Use the anonymous function syntax instead")
pub fn curry5(fun: fn(a, b, c, d, e) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fn(e) { fun(a, b, c, d, e) } } } } }
}

@deprecated("Use the anonymous function syntax instead")
pub fn curry6(fun: fn(a, b, c, d, e, f) -> value) {
  fn(a) {
    fn(b) { fn(c) { fn(d) { fn(e) { fn(f) { fun(a, b, c, d, e, f) } } } } }
  }
}

/// Takes a function that takes two arguments and returns a new function that
/// takes the same two arguments, but in reverse order.
///
pub fn flip(fun: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { fun(a, b) }
}

/// Takes a single argument and always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}

@deprecated("Use a fn literal instead, it is easier to understand")
pub fn constant(value: a) -> fn(b) -> a {
  fn(_) { value }
}

/// Takes an argument and a single function,
/// calls that function with that argument
/// and returns that argument instead of the function return value.
/// Useful for running synchronous side effects in a pipeline.
///
pub fn tap(arg: a, effect: fn(a) -> b) -> a {
  effect(arg)
  arg
}

@deprecated("Use a fn literal instead, it is easier to understand")
pub fn apply1(fun: fn(a) -> value, arg1: a) -> value {
  fun(arg1)
}

@deprecated("Use a fn literal instead, it is easier to understand")
pub fn apply2(fun: fn(a, b) -> value, arg1: a, arg2: b) -> value {
  fun(arg1, arg2)
}

@deprecated("Use a fn literal instead, it is easier to understand")
pub fn apply3(fun: fn(a, b, c) -> value, arg1: a, arg2: b, arg3: c) -> value {
  fun(arg1, arg2, arg3)
}
