/// Takes two functions and chains them together to form one function that takes
/// the input from the first and returns the output of the second.
///
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}

/// Takes a function with arity two
/// and returns a curried equivalent.
/// fn(a, b) -> c becomes fn(a) -> fn(b) -> c
pub fn curry2(fun: fn(a, b) -> value) {
  fn(a) { fn(b) { fun(a, b) } }
}

/// Takes a function with arity three
/// and returns a curried equivalent.
/// fn(a, b, c) -> d becomes fn(a) -> fn(b) -> fn(c) -> d
pub fn curry3(fun: fn(a, b, c) -> value) {
  fn(a) { fn(b) { fn(c) { fun(a, b, c) } } }
}

/// Takes a function with arity four
/// and returns a curried equivalent.
pub fn curry4(fun: fn(a, b, c, d) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fun(a, b, c, d) } } } }
}

/// Takes a function with arity five
/// and returns a curried equivalent.
pub fn curry5(fun: fn(a, b, c, d, e) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fn(e) { fun(a, b, c, d, e) } } } } }
}

/// Takes a function with arity six
/// and returns a curried equivalent.
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

/// A function that always returns its input value.
///
pub fn identity(x: a) -> a {
  x
}
