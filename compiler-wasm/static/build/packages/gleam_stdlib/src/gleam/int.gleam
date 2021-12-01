import gleam/order.{Order}

/// Returns the absolute value of the input.
///
/// ## Examples
///
///    > absolute_value(-12)
///    12
///
///    > absolute_value(10)
///    10
///
pub fn absolute_value(num: Int) -> Int {
  case num >= 0 {
    True -> num
    False -> num * -1
  }
}

/// Parses a given string as an int if possible.
///
/// ## Examples
///
///    > parse("2")
///    Ok(2)
///
///    > parse("ABC")
///    Error(Nil)
///
pub fn parse(string) {
  do_parse(string)
}

if erlang {
  external fn do_parse(String) -> Result(Int, Nil) =
    "gleam_stdlib" "parse_int"
}

if javascript {
  external fn do_parse(String) -> Result(Int, Nil) =
    "../gleam_stdlib.js" "parse_int"
}

/// Prints a given int to a string.
///
/// ## Examples
///
///    > to_string(2)
///    "2"
///
pub fn to_string(int) {
  do_to_string(int)
}

if erlang {
  external fn do_to_string(Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_string(Int) -> String =
    "../gleam_stdlib.js" "to_string"
}

/// For use in to_base_string when base is outside of the allowed range.
pub type InvalidBase {
  InvalidBase
}

/// Prints a given int to a string using the base number provided.
/// Supports only bases 2 to 36, for values outside of which this function returns an Error(InvalidBase).
/// For common bases (2, 8, 16, 36), use the to_baseN functions.
///
/// ## Examples
///
///    > to_base_string(2, 2)
///    Ok("10")
///
///    > to_base_string(48, 16)
///    Ok("30")
///
///    > to_base_string(48, 36)
///    Ok("1C")
///
///    > to_base_string(48, 1)
///    Error(InvalidBase)
///
///    > to_base_string(48, 37)
///    Error(InvalidBase)
///
pub fn to_base_string(int, base) -> Result(String, InvalidBase) {
  case base >= 2 && base <= 36 {
    True -> Ok(do_to_base_string(int, base))
    False -> Error(InvalidBase)
  }
}

if erlang {
  external fn do_to_base_string(Int, Int) -> String =
    "erlang" "integer_to_binary"
}

if javascript {
  external fn do_to_base_string(Int, Int) -> String =
    "../gleam_stdlib.js" "int_to_base_string"
}

/// Prints a given int to a string using base2.
///
/// ## Examples
///
///    > to_base2(2)
///    "10"
///
pub fn to_base2(int) {
  do_to_base_string(int, 2)
}

/// Prints a given int to a string using base8.
///
/// ## Examples
///
///    > to_base8(15)
///    "17"
///
pub fn to_base8(int) {
  do_to_base_string(int, 8)
}

/// Prints a given int to a string using base16.
///
/// ## Examples
///
///    > to_base16(48)
///    "30"
///
pub fn to_base16(int) {
  do_to_base_string(int, 16)
}

/// Prints a given int to a string using base16.
///
/// ## Examples
///
///    > to_base36(48)
///    "1C"
///
pub fn to_base36(int) {
  do_to_base_string(int, 36)
}

/// Takes an int and returns its value as a float
///
/// ## Examples
///
///   > to_float(5)
///   5.
///
///   > to_float(0)
///   0.
///
///   > to_float(-3)
///   -3.
///
pub fn to_float(int) {
  do_to_float(int)
}

if erlang {
  external fn do_to_float(a: Int) -> Float =
    "erlang" "float"
}

if javascript {
  external fn do_to_float(a: Int) -> Float =
    "../gleam_stdlib.js" "identity"
}

/// Restricts an Int between a lower and upper bound
///
/// ## Examples
///
/// ```
/// > clamp(40, min: 50, max: 60)
/// 50
/// ```
///
pub fn clamp(n: Int, min min_bound: Int, max max_bound: Int) -> Int {
  n
  |> min(max_bound)
  |> max(min_bound)
}

/// Compares two ints, returning an order.
///
/// ## Examples
///
///    > compare(2, 3)
///    Lt
///
///    > compare(4, 3)
///    Gt
///
///    > compare(3, 3)
///    Eq
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

/// Compares two int, returning the smaller of the two.
///
/// ## Examples
///
///    > min(2, 3)
///    2
///
pub fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

/// Compares two int, returning the larger of the two.
///
/// ## Examples
///
///    > max(2, 3)
///    3
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
///    > is_even(2)
///    True
///
///    > is_even(3)
///    False
///
pub fn is_even(x: Int) -> Bool {
  x % 2 == 0
}

/// Returns whether the value provided is odd.
///
/// ## Examples
///
///    > is_odd(3)
///    True
///
///    > is_odd(2)
///    False
///
pub fn is_odd(x: Int) -> Bool {
  x % 2 != 0
}

/// Returns the negative of the value provided
///
/// ## Examples
///
///    > negate(1)
///    -1
///
pub fn negate(x: Int) -> Int {
  -1 * x
}

/// Sums a list of Ints.
///
/// ## Example
///
///    > sum([1, 2, 3])
///    6
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

/// Multiplies a list of Ints and returns the product.
///
/// ## Example
///
///    > product([2, 3, 4])
///    24
///
pub fn product(numbers: List(Int)) -> Int {
  case numbers {
    [] -> 0
    _ -> do_product(numbers, 1)
  }
}

fn do_product(numbers: List(Int), initial: Int) -> Int {
  case numbers {
    [] -> initial
    [x, ..rest] -> do_product(rest, x * initial)
  }
}
