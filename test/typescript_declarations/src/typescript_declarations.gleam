import gleam/option.{type Option}

/// Alias to Int
pub type IntAlias =
  Int

/// Int constant
pub const const_int: Int = 0

/// Constat with type of Int alias
pub const const_int_alias: IntAlias = 0

/// String list constant
pub const const_string_list: List(String) = ["hello", "world"]

/// Int list constant
pub const const_int_list: List(Int) = [0, 1, 2]

/// Tuple constant
pub const const_tuple = #("hello", 0)

/// Add two numbers
pub fn function_int_int_returns_int(first a: Int, second b: Int) -> Int {
  a + b
}

/// Alias to add function
pub const function_int_int_returns_int_alias = function_int_int_returns_int

// /// Create alert on browser target
// @external(javascript, "globalThis", "alert")
// pub fn js_alert_external(text: String) -> Nil

/// Return function that will add one to given number
pub fn function_closure_returns_fn_int_which_returns_int_alias() -> fn(Int) ->
  IntAlias {
  let func = function_int_int_returns_int_alias(1, _)
  func
}

pub fn function_generic_fn_generic_which_returns_generic_returns_generic(
  val: a,
  function: fn(a) -> a,
) -> a {
  function(function(val))
}

pub type Either(a, b) {
  Left(a)
  Right(b)
}

pub const either_int: Either(Int, Int) = Left(2)

pub fn function_option() -> Option(Int) {
  option.Some(0)
}
