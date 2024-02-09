import gleam/order.{Order}

/// Attempts to parse a string as a `Float`, returning `Error(Nil)` if it was
/// not possible.
///
/// ## Examples
///
/// ```gleam
/// > parse("2.3")
/// Ok(2.3)
/// ```
///
/// ```gleam
/// > parse("ABC")
/// Error(Nil)
/// ```
///
pub fn parse(string: String) -> Result(Float, Nil) {
  do_parse(string)
}

if erlang {
  external fn do_parse(String) -> Result(Float, Nil) =
    "gleam_stdlib" "parse_float"
}

if javascript {
  external fn do_parse(String) -> Result(Float, Nil) =
    "../gleam_stdlib.mjs" "parse_float"
}

/// Returns the string representation of the provided `Float`.
///
/// ## Examples
///
/// ```gleam
/// > to_string(2.3)
/// "2.3"
/// ```
///
pub fn to_string(x: Float) -> String {
  do_to_string(x)
}

if erlang {
  external fn do_to_string(Float) -> String =
    "gleam_stdlib" "float_to_string"
}

if javascript {
  external fn do_to_string(Float) -> String =
    "../gleam_stdlib.mjs" "float_to_string"
}

/// Restricts a `Float` between a lower and upper bound.
///
/// ## Examples
///
/// ```gleam
/// > clamp(1.2, min: 1.4, max: 1.6)
/// 1.4
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
/// > compare(2.0, 2.3)
/// Lt
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
/// > loosely_compare(5.0, with: 5.3, tolerating: 0.5)
/// Eq
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
/// > loosely_equals(5.0, with: 5.3, tolerating: 0.5)
/// True
/// ```
///
/// ```gleam
/// > loosely_equals(5.0, with: 5.1, tolerating: 0.1)
/// False
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
/// > min(2.0, 2.3)
/// 2.0
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
/// > max(2.0, 2.3)
/// 2.3
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
/// > ceiling(2.3)
/// 3.0
/// ```
///
pub fn ceiling(x: Float) -> Float {
  do_ceiling(x)
}

if erlang {
  external fn do_ceiling(Float) -> Float =
    "math" "ceil"
}

if javascript {
  external fn do_ceiling(Float) -> Float =
    "../gleam_stdlib.mjs" "ceiling"
}

/// Rounds the value to the next lowest whole number as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// > floor(2.3)
/// 2.0
/// ```
///
pub fn floor(x: Float) -> Float {
  do_floor(x)
}

if erlang {
  external fn do_floor(Float) -> Float =
    "math" "floor"
}

if javascript {
  external fn do_floor(Float) -> Float =
    "../gleam_stdlib.mjs" "floor"
}

/// Rounds the value to the nearest whole number as an `Int`.
///
/// ## Examples
///
/// ```gleam
/// > round(2.3)
/// 2
/// ```
///
/// ```gleam
/// > round(2.5)
/// 3
/// ```
///
pub fn round(x: Float) -> Int {
  do_round(x)
}

if erlang {
  external fn do_round(Float) -> Int =
    "erlang" "round"
}

if javascript {
  fn do_round(x: Float) -> Int {
    case x >=. 0.0 {
      True -> js_round(x)
      _ -> 0 - js_round(negate(x))
    }
  }

  external fn js_round(Float) -> Int =
    "../gleam_stdlib.mjs" "round"
}

/// Returns the value as an `Int`, truncating all decimal digits.
///
/// ## Examples
///
/// ```gleam
/// > truncate(2.4343434847383438)
/// 2
/// ```
///
pub fn truncate(x: Float) -> Int {
  do_truncate(x)
}

if erlang {
  external fn do_truncate(Float) -> Int =
    "erlang" "trunc"
}

if javascript {
  external fn do_truncate(Float) -> Int =
    "../gleam_stdlib.mjs" "truncate"
}

/// Returns the absolute value of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// > absolute_value(-12.5)
/// 12.5
/// ```
///
/// ```gleam
/// > absolute_value(10.2)
/// 10.2
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
/// > power(2.0, -1.0)
/// Ok(0.5)
/// ```
///
/// ```gleam
/// > power(2.0, 2.0)
/// Ok(4.0)
/// ```
///
/// ```gleam
/// > power(8.0, 1.5)
/// Ok(22.627416997969522)
/// ```
///
/// ```gleam
/// > 4.0 |> power(of: 2.0)
/// Ok(16.0)
/// ```
///
/// ```gleam
/// > power(-1.0, 0.5)
/// Error(Nil)
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

if erlang {
  external fn do_power(Float, Float) -> Float =
    "math" "pow"
}

if javascript {
  external fn do_power(Float, Float) -> Float =
    "../gleam_stdlib.mjs" "power"
}

/// Returns the square root of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// > square_root(4.0)
/// Ok(2.0)
/// ```
///
/// ```gleam
/// > square_root(-16.0)
/// Error(Nil)
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
/// > negate(1.0)
/// -1.0
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
/// > sum([1.0, 2.2, 3.3])
/// 6.5
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
/// > product([2.5, 3.2, 4.2])
/// 33.6
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

/// Returns `0.0` if `boundary_a` and `boundary_b` are equal,
/// otherwise returns a `Float x` where `lower_boundary =< x < upper_boundary`.
///
/// ## Examples
///
/// ```gleam
/// > random(1.0, 5.0)
/// 2.646355926896028
/// ```
///
pub fn random(boundary_a: Float, boundary_b: Float) -> Float {
  // Based on:
  //
  // ```javascript
  // return Math.random() * (max - min) + min; // The minimum is inclusive and the maximum is exclusive
  // ```
  //
  // See: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random#getting_a_random_number_between_two_values>
  let #(min, max) = case boundary_a, boundary_b {
    a, b if a <=. b -> #(a, b)
    a, b if a >. b -> #(b, a)
  }
  case min, max {
    min, _max if min == max -> min
    min, max -> do_random_uniform() *. { max -. min } +. min
  }
}

if erlang {
  /// Returns a random float uniformly distributed in the value range
  /// 0.0 =< X < 1.0 and updates the state in the process dictionary.
  /// See: <https://www.erlang.org/doc/man/rand.html#uniform-0>
  ///
  external fn do_random_uniform() -> Float =
    "rand" "uniform"
}

if javascript {
  external fn do_random_uniform() -> Float =
    "../gleam_stdlib.mjs" "random_uniform"
}

/// Returns division of the inputs as a `Result`.
///
/// ## Examples
///
/// ```gleam
/// > divide(0.0, 1.0)
/// Ok(1.0)
/// ```
///
/// ```gleam
/// > divide(1.0, 0.0)
/// Error(Nil)
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
/// > add(1.0, 2.0)
/// 3.0
/// ```
///
/// ```gleam
/// > import gleam/list
/// > list.fold([1.0, 2.0, 3.0], 0.0, add)
/// 6.0
/// ```
///
/// ```gleam
/// > 3.0 |> add(2.0)
/// 5.0
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
/// > multiply(2.0, 4.0)
/// 8.0
/// ```
///
/// ```gleam
/// import gleam/list
/// > list.fold([2.0, 3.0, 4.0], 1.0, multiply)
/// 24.0
/// ```
///
/// ```gleam
/// > 3.0 |> multiply(2.0)
/// 6.0
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
/// > subtract(3.0, 1.0)
/// 2.0
/// ```
///
/// ```gleam
/// > import gleam/list
/// > list.fold([1.0, 2.0, 3.0], 10.0, subtract)
/// 4.0
/// ```
///
/// ```gleam
/// > 3.0 |> subtract(_, 2.0)
/// 1.0
/// ```
///
/// ```gleam
/// > 3.0 |> subtract(2.0, _)
/// -1.0
/// ```
///
pub fn subtract(a: Float, b: Float) -> Float {
  a -. b
}
