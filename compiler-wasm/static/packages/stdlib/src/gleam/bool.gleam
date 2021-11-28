//// A type with two possible values, True and False. Used to indicate whether
//// things are... true or false!
////
//// Often is it clearer and offers more type safety to define a custom type
//// than to use Bool. For example, rather than having a `is_teacher: Bool`
//// field consider having a `role: SchoolRole` field where SchoolRole is a custom
//// type that can be either Student or Teacher.

import gleam/order.{Order}

/// Returns the opposite bool value.
///
/// This is the same as the `!` or `not` operators in some other languages.
///
/// ## Examples
///
///    > negate(True)
///    False
///
///    > negate(False)
///    True
///
pub fn negate(bool: Bool) -> Bool {
  case bool {
    True -> False
    False -> True
  }
}

/// Returns the nor of two bools
///
/// ## Examples
///
///    > nor(False, False)
///    True
///
///    > nor(False, True)
///    False
///
///    > nor(True, False)
///    False
///
///    > nor(True, True)
///    False
///
pub fn nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> False
  }
}

/// Returns the nand of two bools
///
/// ## Examples
///
///    > nand(False, False)
///    True
///
///    > nand(False, True)
///    True
///
///    > nand(True, False)
///    True
///
///    > nand(True, True)
///    False
///
pub fn nand(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> True
    True, False -> True
    True, True -> False
  }
}

/// Returns the exclusive or of two bools
///
/// ## Examples
///
///    > exclusive_or(False, False)
///    False
///
///    > exclusive_or(False, True)
///    True
///
///    > exclusive_or(True, False)
///    True
///
///    > exclusive_or(True, True)
///    False
///
pub fn exclusive_or(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> False
    False, True -> True
    True, False -> True
    True, True -> False
  }
}

/// Returns the exclusive nor of two bools
///
/// ## Examples
///
///    > exclusive_nor(False, False)
///    True
///
///    > exclusive_nor(False, True)
///    False
///
///    > exclusive_nor(True, False)
///    False
///
///    > exclusive_nor(True, True)
///    True
///
pub fn exclusive_nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> True
  }
}

/// Compares two bools and returns the first values Order to the second.
///
/// ## Examples
///
///    > import gleam/order
///    > compare(True, False)
///    order.Gt
///
pub fn compare(a: Bool, with b: Bool) -> Order {
  case a, b {
    True, True -> order.Eq
    True, False -> order.Gt
    False, False -> order.Eq
    False, True -> order.Lt
  }
}

/// Returns True if either bool value is True.
///
/// ## Examples
///
///    > max(True, False)
///    True
///
///    > max(False, True)
///    True
///
///    > max(False, False)
///    False
///
pub fn max(a: Bool, b: Bool) -> Bool {
  case a {
    True -> True
    False -> b
  }
}

/// Returns False if either bool value is False.
///
/// ## Examples
///
///    > min(True, False)
///    False
///
///    > min(False, True)
///    False
///
///    > min(False, False)
///    False
///
pub fn min(a: Bool, b: Bool) -> Bool {
  case a {
    False -> False
    True -> b
  }
}

/// Returns a numeric representation of the given bool.
///
/// ## Examples
///
///    > to_int(True)
///    1
///
///    > to_int(False)
///    0
///
pub fn to_int(bool: Bool) -> Int {
  case bool {
    False -> 0
    True -> 1
  }
}
