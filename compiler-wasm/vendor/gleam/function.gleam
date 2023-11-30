/// Takes two functions and chains them together to form one function that
/// takes the input from the first and returns the output of the second.
///
pub fn compose(fun1: fn(a) -> b, fun2: fn(b) -> c) -> fn(a) -> c {
  fn(a) { fun2(fun1(a)) }
}

/// Takes a function with `2` arguments (an arity of `2`), and returns the
/// curried equivalent.
///
/// `fn(a, b) -> c` becomes `fn(a) -> fn(b) -> c`.
///
/// ## Examples
///
/// *Currying* creates a new function that is identical to the given function
/// except that arguments must now be supplied one by one over several function
/// calls. It thus is the process of taking a function with `n` arguments
/// and producing a sequence of `n` single-argument functions. Given:
///
/// ```gleam
/// > fn my_fun(i: Int, s: String) -> String { ... }
/// ```
///
/// …calling `curry2(my_fun)` would return the curried equivalent, like so:
///
/// ```gleam
/// > curry2(my_fun)
/// fn(Int) -> fn(String) -> String
/// ```
///
/// Currying is useful when you want to partially apply a function with
/// some arguments and then pass it somewhere else, for example:
///
/// ```gleam
/// > import gleam/list
/// > let multiply = curry2(fn(x, y) { x * y })
/// > let doubles = list.map([1, 2, 3], multiply(2))
/// [2, 4, 6]
/// ```
///
pub fn curry2(fun: fn(a, b) -> value) {
  fn(a) { fn(b) { fun(a, b) } }
}

/// Takes a function with `3` arguments (an arity of `3`), and returns the
/// curried equivalent.
///
/// `fn(a, b, c) -> d` becomes `fn(a) -> fn(b) -> fn(c) -> d`.
///
/// See [`curry2`](#curry2) for a detailed explanation.
///
pub fn curry3(fun: fn(a, b, c) -> value) {
  fn(a) { fn(b) { fn(c) { fun(a, b, c) } } }
}

/// Takes a function with `4` arguments (an arity of `4`), and returns the
/// curried equivalent.
///
/// `fn(a, b, c, d) -> e` becomes `fn(a) -> fn(b) -> fn(c) -> fn(d) -> e`.
///
/// See [`curry2`](#curry2) for a detailed explanation.
///
pub fn curry4(fun: fn(a, b, c, d) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fun(a, b, c, d) } } } }
}

/// Takes a function with `5` arguments (an arity of `5`), and returns the
/// curried equivalent.
///
/// `fn(a, b, c, d, e) -> f` becomes
/// `fn(a) -> fn(b) -> fn(c) -> fn(d) -> fn(e) -> f`.
///
/// See [`curry2`](#curry2) for a detailed explanation.
///
pub fn curry5(fun: fn(a, b, c, d, e) -> value) {
  fn(a) { fn(b) { fn(c) { fn(d) { fn(e) { fun(a, b, c, d, e) } } } } }
}

/// Takes a function with `6` arguments (an arity of `6`), and returns the
/// curried equivalent.
///
/// `fn(a, b, c, d, e, f) -> g` becomes
/// `fn(a) -> fn(b) -> fn(c) -> fn(d) -> fn(e) -> fn(f) -> g`.
///
/// See [`curry2`](#curry2) for a detailed explanation.
///
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

/// Takes a single argument and returns a new function that
/// ignores its argument and always returns the input value.
///
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

/// Takes a function with arity one and an argument,
/// calls that function with the argument and returns the function return value.
///
/// Useful for concisely calling functions returned as a part of a pipeline.
///
/// ## Example
///
/// ```gleam
/// > let doubler = fn() {
/// >  fn(x: Int) { x * 2 }
/// > }
/// >
/// > doubler()
/// > |> apply1(2)
/// 4
/// ```
///
pub fn apply1(fun: fn(a) -> value, arg1: a) -> value {
  fun(arg1)
}

/// Takes a function with arity two and two arguments,
/// calls that function with the arguments
/// and returns the function return value.
///
/// See [`apply1`](#apply1) for more details.
///
pub fn apply2(fun: fn(a, b) -> value, arg1: a, arg2: b) -> value {
  fun(arg1, arg2)
}

/// Takes a function with arity three and three arguments,
/// calls that function with the arguments
/// and returns the function return value.
///
/// See [`apply1`](#apply1) for more details.
///
pub fn apply3(fun: fn(a, b, c) -> value, arg1: a, arg2: b, arg3: c) -> value {
  fun(arg1, arg2, arg3)
}
