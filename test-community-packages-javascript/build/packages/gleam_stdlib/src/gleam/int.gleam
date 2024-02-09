import gleam/float
import gleam/order.{Order}

/// Returns the absolute value of the input.
///
/// ## Examples
///
/// ```gleam
/// > absolute_value(-12)
/// 12
/// ```
///
/// ```gleam
/// > absolute_value(10)
/// 10
/// ```
///
pub fn absolute_value(x: Int) -> Int {
  case x >= 0 {
    True -> x
    False -> x * -1
  }
}

/// Returns the results of the base being raised to the power of the
/// exponent, as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// > power(2, -1.0)
/// Ok(0.5)
/// ```
///
/// ```gleam
/// > power(2, 2.0)
/// Ok(4.0)
/// ```
///
/// ```gleam
/// > power(8, 1.5)
/// Ok(22.627416997969522)
/// ```
///
/// ```gleam
/// > 4 |> power(of: 2.0)
/// Ok(16.0)
/// ```
///
/// ```gleam
/// > power(-1, 0.5)
/// Error(Nil)
/// ```
///
pub fn power(base: Int, of exponent: Float) -> Result(Float, Nil) {
  base
  |> to_float()
  |> float.power(exponent)
}

/// Returns the square root of the input as a `Float`.
///
/// ## Examples
///
/// ```gleam
/// > square_root(4)
/// Ok(2.0)
/// ```
///
/// ```gleam
/// > square_root(-16)
/// Error(Nil)
/// ```
///
pub fn square_root(x: Int) -> Result(Float, Nil) {
  x
  |> to_float()
  |> float.square_root()
}

/// Parses a given string as an int if possible.
///
/// ## Examples
///
/// ```gleam
/// > parse("2")
/// Ok(2)
/// ```
///
/// ```gleam
/// > parse("ABC")
/// Error(Nil)
/// ```
///
pub fn parse(string: String) -> Result(Int, Nil) {
  do_parse(string)
}

if erlang {
  external fn do_parse(String) -> Result(Int, Nil) =
    "gleam_stdlib" "parse_int"
}

if javascript {
  external fn do_parse(String) -> Result(Int, Nil) =
    "../gleam_stdlib.mjs" "parse_int"
}

/// Parses a given string as an int in a given base if possible.
/// Supports only bases 2 to 36, for values outside of which this function returns an `Error(Nil)`.
///
/// ## Examples
///
/// ```gleam
/// > base_parse("10", 2)
/// Ok(2)
///
/// > base_parse("30", 16)
/// Ok(48)
///
/// > base_parse("1C", 36)
/// Ok(48)
///
/// > base_parse("48", 1)
/// Error(Nil)
///
/// > base_parse("48", 37)
/// Error(Nil)
/// ```
///
pub fn base_parse(string: String, base: Int) -> Result(Int, Nil) {
  case base >= 2 && base <= 36 {
    True -> do_base_parse(string, base)
    False -> Error(Nil)
  }
}

if erlang {
  external fn do_base_parse(String, Int) -> Result(Int, Nil) =
    "gleam_stdlib" "int_from_base_string"
}

if javascript {
  external fn do_base_parse(String, Int) -> Result(Int, Nil) =
    "../gleam_stdlib.mjs" "int_from_base_string"
}

/// Prints a given int to a string.
///
/// ## Examples
///
/// ```gleam
/// > to_string(2)
/// "2"
/// ```
///
pub fn to_string(x: Int) {
  do_to_string(x)
}

if erlang {
  external fn do_to_string(Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_string(Int) -> String =
    "../gleam_stdlib.mjs" "to_string"
}

/// Error value when trying to operate with a base out of the allowed range.
///
pub type InvalidBase {
  InvalidBase
}

/// Prints a given int to a string using the base number provided.
/// Supports only bases 2 to 36, for values outside of which this function returns an `Error(InvalidBase)`.
/// For common bases (2, 8, 16, 36), use the `to_baseN` functions.
///
/// ## Examples
///
/// ```gleam
/// > to_base_string(2, 2)
/// Ok("10")
/// ```
///
/// ```gleam
/// > to_base_string(48, 16)
/// Ok("30")
/// ```
///
/// ```gleam
/// > to_base_string(48, 36)
/// Ok("1C")
/// ```
///
/// ```gleam
/// > to_base_string(48, 1)
/// Error(InvalidBase)
/// ```
///
/// ```gleam
/// > to_base_string(48, 37)
/// Error(InvalidBase)
/// ```
///
pub fn to_base_string(x: Int, base: Int) -> Result(String, InvalidBase) {
  case base >= 2 && base <= 36 {
    True -> Ok(do_to_base_string(x, base))
    False -> Error(InvalidBase)
  }
}

if erlang {
  external fn do_to_base_string(Int, Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_base_string(Int, Int) -> String =
    "../gleam_stdlib.mjs" "int_to_base_string"
}

/// Prints a given int to a string using base-2.
///
/// ## Examples
///
/// ```gleam
/// > to_base2(2)
/// "10"
/// ```
///
pub fn to_base2(x: Int) -> String {
  do_to_base_string(x, 2)
}

/// Prints a given int to a string using base-8.
///
/// ## Examples
///
/// ```gleam
/// > to_base8(15)
/// "17"
/// ```
///
pub fn to_base8(x: Int) -> String {
  do_to_base_string(x, 8)
}

/// Prints a given int to a string using base-16.
///
/// ## Examples
///
/// ```gleam
/// > to_base16(48)
/// "30"
/// ```
///
pub fn to_base16(x: Int) -> String {
  do_to_base_string(x, 16)
}

/// Prints a given int to a string using base-36.
///
/// ## Examples
///
/// ```gleam
/// > to_base36(48)
/// "1C"
/// ```
///
pub fn to_base36(x: Int) -> String {
  do_to_base_string(x, 36)
}

/// Takes an int and returns its value as a float.
///
/// ## Examples
///
/// ```gleam
/// > to_float(5)
/// 5.0
/// ```
///
/// ```gleam
/// > to_float(0)
/// 0.0
/// ```
///
/// ```gleam
/// > to_float(-3)
/// -3.0
/// ```
///
pub fn to_float(x: Int) -> Float {
  do_to_float(x)
}

if erlang {
  external fn do_to_float(Int) -> Float =
    "erlang" "float"
}

if javascript {
  external fn do_to_float(Int) -> Float =
    "../gleam_stdlib.mjs" "identity"
}

/// Restricts an int between a lower and upper bound.
///
/// ## Examples
///
/// ```gleam
/// > clamp(40, min: 50, max: 60)
/// 50
/// ```
///
pub fn clamp(x: Int, min min_bound: Int, max max_bound: Int) -> Int {
  x
  |> min(max_bound)
  |> max(min_bound)
}

/// Compares two ints, returning an order.
///
/// ## Examples
///
/// ```gleam
/// > compare(2, 3)
/// Lt
/// ```
///
/// ```gleam
/// > compare(4, 3)
/// Gt
/// ```
///
/// ```gleam
/// > compare(3, 3)
/// Eq
/// ```
///
pub fn compare(a: Int, with b: Int) -> Order {
  case a == b {
    True -> order.Eq
    False ->
      case a < b {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

/// Compares two ints, returning the smaller of the two.
///
/// ## Examples
///
/// ```gleam
/// > min(2, 3)
/// 2
/// ```
///
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Compares two ints, returning the larger of the two.
///
/// ## Examples
///
/// ```gleam
/// > max(2, 3)
/// 3
/// ```
///
pub fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

/// Returns whether the value provided is even.
///
/// ## Examples
///
/// ```gleam
/// > is_even(2)
/// True
/// ```
///
/// ```gleam
/// > is_even(3)
/// False
/// ```
///
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

/// Returns whether the value provided is odd.
///
/// ## Examples
///
/// ```gleam
/// > is_odd(3)
/// True
/// ```
///
/// ```gleam
/// > is_odd(2)
/// False
/// ```
///
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}

/// Returns the negative of the value provided.
///
/// ## Examples
///
/// ```gleam
/// > negate(1)
/// -1
/// ```
///
pub fn negate(x: Int) -> Int {
  -1 * x
}

/// Sums a list of ints.
///
/// ## Example
///
/// ```gleam
/// > sum([1, 2, 3])
/// 6
/// ```
///
pub fn sum(numbers: List(Int)) -> Int {
  numbers
  |> do_sum(0)
}

fn do_sum(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_sum(rest, x + initial)
  }
}

/// Multiplies a list of ints and returns the product.
///
/// ## Example
///
/// ```gleam
/// > product([2, 3, 4])
/// 24
/// ```
///
pub fn product(numbers: List(Int)) -> Int {
  case numbers {
    [] -> 1
    _ -> do_product(numbers, 1)
  }
}

fn do_product(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x * initial)
  }
}

/// Splits an integer into its digit representation in the specified base
///
/// ## Examples
///
/// ```gleam
/// > digits(234, 10)
/// Ok([2,3,4])
/// ```
///
/// ```gleam
/// > digits(234, 1)
/// Error(InvalidBase)
/// ```
///
pub fn digits(x: Int, base: Int) -> Result(List(Int), InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> Ok(do_digits(x, base, []))
  }
}

fn do_digits(x: Int, base: Int, acc: List(Int)) -> List(Int) {
  case absolute_value(x) < base {
    True -> [x, ..acc]
    False -> do_digits(x / base, base, [x % base, ..acc])
  }
}

/// Joins a list of digits into a single value.
/// Returns an error if the base is less than 2 or if the list contains a digit greater than or equal to the specified base.
///
/// ## Examples
///
/// ```gleam
/// > undigits([2,3,4], 10)
/// Ok(234)
/// ```
///
/// ```gleam
/// > undigits([2,3,4], 1)
/// Error(InvalidBase)
/// ```
///
/// ```gleam
/// > undigits([2,3,4], 2)
/// Error(InvalidBase)
/// ```
///
pub fn undigits(numbers: List(Int), base: Int) -> Result(Int, InvalidBase) {
  case base < 2 {
    True -> Error(InvalidBase)
    False -> do_undigits(numbers, base, 0)
  }
}

fn do_undigits(
  numbers: List(Int),
  base: Int,
  acc: Int,
) -> Result(Int, InvalidBase) {
  case numbers {
    [] -> Ok(acc)
    [digit, ..] if digit >= base -> Error(InvalidBase)
    [digit, ..rest] -> do_undigits(rest, base, acc * base + digit)
  }
}

/// Returns `0` if `boundary_a` and `boundary_b` are equal,
/// otherwise returns an `Int x` where `lower_boundary =< x < upper_boundary`.
///
/// ## Examples
///
/// ```gleam
/// > random(1, 5)
/// 2
/// ```
///
pub fn random(boundary_a: Int, boundary_b: Int) -> Int {
  // Based on:
  //
  // ```javascript
  // min = Math.ceil(min);
  // max = Math.floor(max);
  // return Math.floor(Math.random() * (max - min) + min); // The minimum is inclusive and the maximum is exclusive
  // ```
  //
  // See: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random#getting_a_random_integer_between_two_values>
  let #(min, max) = case boundary_a, boundary_b {
    a, b if a <= b -> #(a, b)
    a, b if a > b -> #(b, a)
  }

  let min =
    to_float(min)
    |> float.ceiling()
  let max =
    to_float(max)
    |> float.floor()

  float.random(min, max)
  |> float.floor()
  |> float.round()
}

/// Performs a truncated integer division.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// > divide(0, 1)
/// Ok(1)
/// ```
///
/// ```gleam
/// > divide(1, 0)
/// Error(Nil)
/// ```
///
/// ```gleam
/// > divide(5, 2)
/// Ok(2)
/// ```
///
/// ```gleam
/// > divide(-99, 2)
/// Ok(-49)
/// ```
///
pub fn divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend / divisor)
  }
}

/// Computes the remainder of an integer division of inputs as a `Result`.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// Most the time you will want to use the `%` operator instead of this
/// function.
///
/// ## Examples
///
/// ```gleam
/// > remainder(3, 2)
/// Ok(1)
/// ```
///
/// ```gleam
/// > remainder(1, 0)
/// Error(Nil)
/// ```
///
/// ```gleam
/// > remainder(10, -1)
/// Ok(0)
/// ```
///
/// ```gleam
/// > remainder(13, by: 3)
/// Ok(1)
/// ```
///
/// ```gleam
/// > remainder(-13, by: 3)
/// Ok(-1)
/// ```
///
/// ```gleam
/// > remainder(13, by: -3)
/// Ok(1)
/// ```
///
/// ```gleam
/// > remainder(-13, by: -3)
/// Ok(-1)
/// ```
///
pub fn remainder(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor -> Ok(dividend % divisor)
  }
}

/// Computes the modulo of an integer division of inputs as a `Result`.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// Most the time you will want to use the `%` operator instead of this
/// function.
///
/// ## Examples
///
/// ```gleam
/// > modulo(3, 2)
/// Ok(1)
/// ```
///
/// ```gleam
/// > modulo(1, 0)
/// Error(Nil)
/// ```
///
/// ```gleam
/// > modulo(10, -1)
/// Ok(0)
/// ```
///
/// ```gleam
/// > modulo(13, by: 3)
/// Ok(1)
/// ```
///
/// ```gleam
/// > modulo(-13, by: 3)
/// Ok(2)
/// ```
///
/// ```gleam
/// > modulo(13, by: -3)
/// Ok(-2)
/// ```
///
/// ```gleam
/// > modulo(-13, by: -3)
/// Ok(-1)
/// ```
///
pub fn modulo(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    _ -> {
      let remainder = dividend % divisor
      case remainder * divisor < 0 {
        True -> Ok(remainder + divisor)
        False -> Ok(remainder)
      }
    }
  }
}

/// Performs a *floored* integer division, which means that the result will
/// always be rounded towards negative infinity.
///
/// If you want to perform truncated integer division (rounding towards zero),
/// use `int.divide()` or the `/` operator instead.
///
/// Returns division of the inputs as a `Result`: If the given divisor equals
/// `0`, this function returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// > floor_divide(1, 0)
/// Error(Nil)
/// ```
///
/// ```gleam
/// > floor_divide(5, 2)
/// Ok(2)
/// ```
///
/// ```gleam
/// > floor_divide(6, -4)
/// Ok(-2)
/// ```
///
/// ```gleam
/// > floor_divide(-99, 2)
/// Ok(-50)
/// ```
///
pub fn floor_divide(dividend: Int, by divisor: Int) -> Result(Int, Nil) {
  case divisor {
    0 -> Error(Nil)
    divisor ->
      case dividend * divisor < 0 && dividend % divisor != 0 {
        True -> Ok(dividend / divisor - 1)
        False -> Ok(dividend / divisor)
      }
  }
}

/// Adds two integers together.
///
/// It's the function equivalent of the `+` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// > add(1, 2)
/// 3
/// ```
///
/// ```gleam
/// import gleam/list
/// > list.fold([1, 2, 3], 0, add)
/// 6
/// ```
///
/// ```gleam
/// > 3 |> add(2)
/// 5
/// ```
///
pub fn add(a: Int, b: Int) -> Int {
  a + b
}

/// Multiplies two integers together.
///
/// It's the function equivalent of the `*` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// > multiply(2, 4)
/// 8
/// ```
///
/// ```gleam
/// import gleam/list
/// > list.fold([2, 3, 4], 1, multiply)
/// 24
/// ```
///
/// ```gleam
/// > 3 |> multiply(2)
/// 6
/// ```
///
pub fn multiply(a: Int, b: Int) -> Int {
  a * b
}

/// Subtracts one int from another.
///
/// It's the function equivalent of the `-` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// > subtract(3, 1)
/// 2.0
/// ```
///
/// ```gleam
/// import gleam/list
/// > list.fold([1, 2, 3], 10, subtract)
/// 4
/// ```
///
/// ```gleam
/// > 3 |> subtract(2)
/// 1
/// ```
///
/// ```gleam
/// > 3 |> subtract(2, _)
/// -1
/// ```
///
pub fn subtract(a: Int, b: Int) -> Int {
  a - b
}
