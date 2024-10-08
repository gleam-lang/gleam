//// Functions for working with floats.
////
//// ## Division by zero
////
//// Gleam runs on the Erlang virtual machine, which does not follow the IEEE
//// 754 standard for floating point arithmetic and does not have an `Infinity`
//// value.  In Erlang division by zero results in a crash, however Gleam does
//// not have partial functions and operators in core so instead division by zero
//// returns zero, a behaviour taken from Pony, Coq, and Lean.
////
//// This may seem unexpected at first, but it is no less mathematically valid
//// than crashing or returning a special value. Division by zero is undefined
//// in mathematics.

import gleam/order.{type Order}

/// Attempts to parse a string as a `Float`, returning `Error(Nil)` if it was
/// not possible.
///
/// ## Examples
///
/// ```gleam
/// parse("2.3")
/// // -> Ok(2.3)
/// ```
///
/// ```gleam
/// parse("ABC")
/// // -> Error(Nil)
/// ```
///
pub fn parse(string: String) -> Result(Float, Nil) {
  do_parse(string)
}

@external(erlang, "gleam_stdlib", "parse_float")
@external(javascript, "../gleam_stdlib.mjs", "parse_float")
fn do_parse(a: String) -> Result(Float, Nil)

/// Returns the string representation of the provided `Float`.
///
/// ## Examples
///
/// ```gleam
/// to_string(2.3)
/// // -> "2.3"
/// ```
///
pub fn to_string(x: Float) -> String {
  do_to_string(x)
}

@external(erlang, "gleam_stdlib", "float_to_string")
@external(javascript, "../gleam_stdlib.mjs", "float_to_string")
fn do_to_string(a: Float) -> String

/// Restricts a `Float` between a lower and upper bound.
///
/// ## Examples
///
/// ```gleam
/// clamp(1.2, min: 1.4, max: 1.6)
/// // -> 1.4
/// ```
///
pub fn clamp(x: Float, min min_bound: Float, max max_bound: Float) -> Float {
  x
  |> min(max_bound)
  |> max(min_bound)
}

/// Compares two `Float`s, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// ## Examples
///
/// ```gleam
/// compare(2.0, 2.3)
/// // -> Lt
/// ```
///
/// To handle
/// [Floating Point Imprecision](https://en.wikipedia.org/wiki/Floating-point_arithmetic#Accuracy_problems)
/// you may use [`loosely_compare`](#loosely_compare) instead.
///
pub fn compare(a: Float, with b: Float) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a <. b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

/// Compares two `Float`s within a tolerance, returning an `Order`:
/// `Lt` for lower than, `Eq` for equals, or `Gt` for greater than.
///
/// This function allows Float comparison while handling
/// [Floating Point Imprecision](https://en.wikipedia.org/wiki/Floating-point_arithmetic#Accuracy_problems).
///
/// Notice: For `Float`s the tolerance won't be exact:
/// `5.3 - 5.0` is not exactly `0.3`.
///
/// ## Examples
///
/// ```gleam
/// loosely_compare(5.0, with: 5.3, tolerating: 0.5)
/// // -> Eq
/// ```
///
/// If you want to check only for equality you may use
/// [`loosely_equals`](#loosely_equals) instead.
///
pub fn loosely_compare(
  a: Float,
  with b: Float,
  tolerating tolerance: Float,
) -> Order {
  let difference = absolute_value(a -. b)
  case difference <=. tolerance {
    True -> order.Eq
    False -> compare(a, b)
  }
}

/// Checks for equality of two `Float`s within a tolerance,
/// returning an `Bool`.
///
/// This function allows Float comparison while handling
/// [Floating Point Imprecision](https://en.wikipedia.org/wiki/Floating-point_arithmetic#Accuracy_problems).
///
/// Notice: For `Float`s the tolerance won't be exact:
/// `5.3 - 5.0` is not exactly `0.3`.
///
/// ## Examples
///
/// ```gleam
/// loosely_equals(5.0, with: 5.3, tolerating: 0.5)
/// // -> True
/// ```
///
/// ```gleam
/// loosely_equals(5.0, with: 5.1, tolerating: 0.1)
/// // -> False
/// ```
///
pub fn loosely_equals(
  a: Float,
  with b: Float,
  tolerating tolerance: Float,
) -> Bool {
  let difference = absolute_value(a -. b)
  difference <=. tolerance
}

/// Compares two `Float`s, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// min(2.0, 2.3)
/// // -> 2.0
/// ```
///
pub fn min(a: Float, b: Float) -> Float {
  case a <. b {
    True -> a
    False -> b
  }
}

/// Compares two `Float`s, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// max(2.0, 2.3)
/// // -> 2.3
/// ```
///
pub fn max(a: Float, b: Float) -> Float {
  case a >. b {
    True -> a
    False -> b
  }
}

/// Rounds the value to the next highest whole number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// ceiling(2.3)
/// // -> 3.0
/// ```
///
pub fn ceiling(x: Float) -> Float {
  do_ceiling(x)
}

@external(erlang, "math", "ceil")
@external(javascript, "../gleam_stdlib.mjs", "ceiling")
fn do_ceiling(a: Float) -> Float

/// Rounds the value to the next lowest whole number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// floor(2.3)
/// // -> 2.0
/// ```
///
pub fn floor(x: Float) -> Float {
  do_floor(x)
}

@external(erlang, "math", "floor")
@external(javascript, "../gleam_stdlib.mjs", "floor")
fn do_floor(a: Float) -> Float

/// Rounds the value to the nearest whole number as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// round(2.3)
/// // -> 2
/// ```
///
/// ```gleam
/// round(2.5)
/// // -> 3
/// ```
///
pub fn round(x: Float) -> Int {
  do_round(x)
}

@external(erlang, "erlang", "round")
fn do_round(x: Float) -> Int {
  case x >=. 0.0 {
    True -> js_round(x)
    _ -> 0 - js_round(negate(x))
  }
}

@external(javascript, "../gleam_stdlib.mjs", "round")
fn js_round(a: Float) -> Int

/// Returns the value as an `Int`, truncating all decimal digits.
///
/// ## Examples
///
/// ```gleam
/// truncate(2.4343434847383438)
/// // -> 2
/// ```
///
pub fn truncate(x: Float) -> Int {
  do_truncate(x)
}

@external(erlang, "erlang", "trunc")
@external(javascript, "../gleam_stdlib.mjs", "truncate")
fn do_truncate(a: Float) -> Int

/// Converts the value to a given precision as a `Float`.
/// The precision is the number of allowed decimal places.
/// Negative precisions are allowed and force rounding
/// to the nearest tenth, hundredth, thousandth etc.
///
/// ## Examples
///
/// ```gleam
/// to_precision(2.43434348473, precision: 2)
/// // -> 2.43
/// ```
///
/// ```gleam
/// to_precision(547890.453444, precision: -3)
/// // -> 548000.0
/// ```
///
pub fn to_precision(x: Float, precision: Int) -> Float {
  let factor = do_power(10.0, do_to_float(-precision))
  do_to_float(round(x /. factor)) *. factor
}

@external(erlang, "erlang", "float")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_to_float(a: Int) -> Float

/// Returns the absolute value of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// absolute_value(-12.5)
/// // -> 12.5
/// ```
///
/// ```gleam
/// absolute_value(10.2)
/// // -> 10.2
/// ```
///
pub fn absolute_value(x: Float) -> Float {
  case x >=. 0.0 {
    True -> x
    _ -> 0.0 -. x
  }
}

/// Returns the results of the base being raised to the power of the
/// exponent, as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// power(2.0, -1.0)
/// // -> Ok(0.5)
/// ```
///
/// ```gleam
/// power(2.0, 2.0)
/// // -> Ok(4.0)
/// ```
///
/// ```gleam
/// power(8.0, 1.5)
/// // -> Ok(22.627416997969522)
/// ```
///
/// ```gleam
/// 4.0 |> power(of: 2.0)
/// // -> Ok(16.0)
/// ```
///
/// ```gleam
/// power(-1.0, 0.5)
/// // -> Error(Nil)
/// ```
///
pub fn power(base: Float, of exponent: Float) -> Result(Float, Nil) {
  let fractional: Bool = ceiling(exponent) -. exponent >. 0.0
  // In the following check:
  // 1. If the base is negative and the exponent is fractional then
  //    return an error as it will otherwise be an imaginary number
  // 2. If the base is 0 and the exponent is negative then the expression
  //    is equivalent to the exponent divided by 0 and an error should be
  //    returned
  case base <. 0.0 && fractional || base == 0.0 && exponent <. 0.0 {
    True -> Error(Nil)
    False -> Ok(do_power(base, exponent))
  }
}

@external(erlang, "math", "pow")
@external(javascript, "../gleam_stdlib.mjs", "power")
fn do_power(a: Float, b: Float) -> Float

/// Returns the square root of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// square_root(4.0)
/// // -> Ok(2.0)
/// ```
///
/// ```gleam
/// square_root(-16.0)
/// // -> Error(Nil)
/// ```
///
pub fn square_root(x: Float) -> Result(Float, Nil) {
  power(x, 0.5)
}

/// Returns the negative of the value provided.
///
/// ## Examples
///
/// ```gleam
/// negate(1.0)
/// // -> -1.0
/// ```
///
pub fn negate(x: Float) -> Float {
  -1.0 *. x
}

/// Sums a list of `Float`s.
///
/// ## Example
///
/// ```gleam
/// sum([1.0, 2.2, 3.3])
/// // -> 6.5
/// ```
///
pub fn sum(numbers: List(Float)) -> Float {
  numbers
  |> do_sum(0.0)
}

fn do_sum(numbers: List(Float), initial: Float) -> Float {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_sum(rest, x +. initial)
  }
}

/// Multiplies a list of `Float`s and returns the product.
///
/// ## Example
///
/// ```gleam
/// product([2.5, 3.2, 4.2])
/// // -> 33.6
/// ```
///
pub fn product(numbers: List(Float)) -> Float {
  case numbers {
    [] -> 1.0
    _ -> do_product(numbers, 1.0)
  }
}

fn do_product(numbers: List(Float), initial: Float) -> Float {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x *. initial)
  }
}

/// Generates a random float between the given zero (inclusive) and one
/// (exclusive).
///
/// On Erlang this updates the random state in the process dictionary.
/// See: <https://www.erlang.org/doc/man/rand.html#uniform-0>
///
/// ## Examples
///
/// ```gleam
/// random()
/// // -> 0.646355926896028
/// ```
///
@external(erlang, "rand", "uniform")
@external(javascript, "../gleam_stdlib.mjs", "random_uniform")
pub fn random() -> Float

/// Computes the modulo of an float division of inputs as a `Result`.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// modulo(13.3, by: 3.3)
/// // -> Ok(0.1)
/// ```
///
/// ```gleam
/// modulo(-13.3, by: 3.3)
/// // -> Ok(3.2)
/// ```
///
/// ```gleam
/// modulo(13.3, by: -3.3)
/// // -> Ok(-3.2)
/// ```
///
/// ```gleam
/// modulo(-13.3, by: -3.3)
/// // -> Ok(-0.1)
/// ```
///
pub fn modulo(dividend: Float, by divisor: Float) -> Result(Float, Nil) {
  case divisor {
    0.0 -> Error(Nil)
    _ -> Ok(dividend -. floor(dividend /. divisor) *. divisor)
  }
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// divide(0.0, 1.0)
/// // -> Ok(0.0)
/// ```
///
/// ```gleam
/// divide(1.0, 0.0)
/// // -> Error(Nil)
/// ```
///
pub fn divide(a: Float, by b: Float) -> Result(Float, Nil) {
  case b {
    0.0 -> Error(Nil)
    b -> Ok(a /. b)
  }
}

/// Adds two floats together.
///
/// It's the function equivalent of the `+.` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// add(1.0, 2.0)
/// // -> 3.0
/// ```
///
/// ```gleam
/// import gleam/list
///
/// list.fold([1.0, 2.0, 3.0], 0.0, add)
/// // -> 6.0
/// ```
///
/// ```gleam
/// 3.0 |> add(2.0)
/// // -> 5.0
/// ```
///
pub fn add(a: Float, b: Float) -> Float {
  a +. b
}

/// Multiplies two floats together.
///
/// It's the function equivalent of the `*.` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// multiply(2.0, 4.0)
/// // -> 8.0
/// ```
///
/// ```gleam
/// import gleam/list
///
/// list.fold([2.0, 3.0, 4.0], 1.0, multiply)
/// // -> 24.0
/// ```
///
/// ```gleam
/// 3.0 |> multiply(2.0)
/// // -> 6.0
/// ```
///
pub fn multiply(a: Float, b: Float) -> Float {
  a *. b
}

/// Subtracts one float from another.
///
/// It's the function equivalent of the `-.` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// subtract(3.0, 1.0)
/// // -> 2.0
/// ```
///
/// ```gleam
/// import gleam/list
///
/// list.fold([1.0, 2.0, 3.0], 10.0, subtract)
/// // -> 4.0
/// ```
///
/// ```gleam
/// 3.0 |> subtract(_, 2.0)
/// // -> 1.0
/// ```
///
/// ```gleam
/// 3.0 |> subtract(2.0, _)
/// // -> -1.0
/// ```
///
pub fn subtract(a: Float, b: Float) -> Float {
  a -. b
}
