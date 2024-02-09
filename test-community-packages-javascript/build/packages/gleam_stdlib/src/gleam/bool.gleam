//// A type with two possible values, `True` and `False`. Used to indicate whether
//// things are... true or false!
////
//// Often is it clearer and offers more type safety to define a custom type
//// than to use `Bool`. For example, rather than having a `is_teacher: Bool`
//// field consider having a `role: SchoolRole` field where `SchoolRole` is a custom
//// type that can be either `Student` or `Teacher`.

import gleam/order.{Order}

/// Returns the and of two bools, but it evaluates both arguments.
///
/// It's the function equivalent of the `&&` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// > and(True, True)
/// True
/// ```
///
/// ```gleam
/// > and(False, True)
/// False
/// ```
///
/// ```gleam
/// > False |> and(True)
/// False
/// ```
///
pub fn and(a: Bool, b: Bool) -> Bool {
  a && b
}

/// Returns the or of two bools, but it evaluates both arguments.
///
/// It's the function equivalent of the `||` operator.
/// This function is useful in higher order functions or pipes.
///
/// ## Examples
///
/// ```gleam
/// > or(True, True)
/// True
/// ```
///
/// ```gleam
/// > or(False, True)
/// True
/// ```
///
/// ```gleam
/// > False |> or(True)
/// True
/// ```
///
pub fn or(a: Bool, b: Bool) -> Bool {
  a || b
}

/// Returns the opposite bool value.
///
/// This is the same as the `!` or `not` operators in some other languages.
///
/// ## Examples
///
/// ```gleam
/// > negate(True)
/// False
/// ```
///
/// ```gleam
/// > negate(False)
/// True
/// ```
///
pub fn negate(bool: Bool) -> Bool {
  case bool {
    True -> False
    False -> True
  }
}

/// Returns the nor of two bools.
///
/// ## Examples
///
/// ```gleam
/// > nor(False, False)
/// True
/// ```
///
/// ```gleam
/// > nor(False, True)
/// False
/// ```
///
/// ```gleam
/// > nor(True, False)
/// False
/// ```
///
/// ```gleam
/// > nor(True, True)
/// False
/// ```
///
pub fn nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> False
  }
}

/// Returns the nand of two bools.
///
/// ## Examples
///
/// ```gleam
/// > nand(False, False)
/// True
/// ```
///
/// ```gleam
/// > nand(False, True)
/// True
/// ```
///
/// ```gleam
/// > nand(True, False)
/// True
/// ```
///
/// ```gleam
/// > nand(True, True)
/// False
/// ```
///
pub fn nand(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> True
    True, False -> True
    True, True -> False
  }
}

/// Returns the exclusive or of two bools.
///
/// ## Examples
///
/// ```gleam
/// > exclusive_or(False, False)
/// False
/// ```
///
/// ```gleam
/// > exclusive_or(False, True)
/// True
/// ```
///
/// ```gleam
/// > exclusive_or(True, False)
/// True
/// ```
///
/// ```gleam
/// > exclusive_or(True, True)
/// False
/// ```
///
pub fn exclusive_or(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> False
    False, True -> True
    True, False -> True
    True, True -> False
  }
}

/// Returns the exclusive nor of two bools.
///
/// ## Examples
///
/// ```gleam
/// > exclusive_nor(False, False)
/// True
/// ```
///
/// ```gleam
/// > exclusive_nor(False, True)
/// False
/// ```
///
/// ```gleam
/// > exclusive_nor(True, False)
/// False
/// ```
///
/// ```gleam
/// > exclusive_nor(True, True)
/// True
/// ```
///
pub fn exclusive_nor(a: Bool, b: Bool) -> Bool {
  case a, b {
    False, False -> True
    False, True -> False
    True, False -> False
    True, True -> True
  }
}

/// Compares two bools and returns the first value's `Order` to the second.
///
/// ## Examples
///
/// ```gleam
/// > import gleam/order
/// > compare(True, False)
/// order.Gt
/// ```
///
pub fn compare(a: Bool, with b: Bool) -> Order {
  case a, b {
    True, True -> order.Eq
    True, False -> order.Gt
    False, False -> order.Eq
    False, True -> order.Lt
  }
}

/// Returns `True` if either argument's value is `True`.
///
/// ## Examples
///
/// ```gleam
/// > max(True, False)
/// True
/// ```
///
/// ```gleam
/// > max(False, True)
/// True
/// ```
///
/// ```gleam
/// > max(False, False)
/// False
/// ```
///
pub fn max(a: Bool, b: Bool) -> Bool {
  case a {
    True -> True
    False -> b
  }
}

/// Returns `False` if either bool value is `False`.
///
/// ## Examples
///
/// ```gleam
/// > min(True, False)
/// False
/// ```
///
/// ```gleam
/// > min(False, True)
/// False
///
/// > min(False, False)
/// False
/// ```
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
/// ```gleam
/// > to_int(True)
/// 1
///
/// > to_int(False)
/// 0
/// ```
///
pub fn to_int(bool: Bool) -> Int {
  case bool {
    False -> 0
    True -> 1
  }
}

/// Returns a string representation of the given bool.
///
/// ## Examples
///
/// ```gleam
/// > to_string(True)
/// "True"
/// ```
///
/// ```gleam
/// > to_string(False)
/// "False"
/// ```
///
pub fn to_string(bool: Bool) -> String {
  case bool {
    False -> "False"
    True -> "True"
  }
}

/// Run a callback function if the given bool is `False`, otherwise return a
/// default value.
///
/// With a `use` expression this function can simulate the early-return pattern
/// found in some other programming languages.
///
/// In a procedural language:
///
/// ```js
/// if (predicate) return value;
/// // ...
/// ```
///
/// In Gleam with a `use` expression:
///
/// ```gleam
/// use <- guard(when: predicate, return: value)
/// // ...
/// ```
///
/// Like everything in Gleam `use` is an expression, so it short circuits the
/// current block, not the entire function. As a result you can assign the value
/// to a variable:
///
/// ```gleam
/// let x = {
///   use <- guard(when: predicate, return: value)
///   // ...
/// }
/// ```
///
/// Note that unlike in procedural languages the `return` value is evaluated
/// even when the predicate is `False`, so it is advisable not to perform
/// expensive computation there.
///
///
/// ## Examples
///
/// ```gleam
/// > let name = ""
/// > use <- guard(when: name == "", return: "Welcome!")
/// > "Hello, " <> name
/// "Welcome!"
/// ```
///
/// ```gleam
/// > let name = "Kamaka"
/// > use <- guard(when: name == "", return: "Welcome!")
/// > "Hello, " <> name
/// "Hello, Kamaka"
/// ```
///
pub fn guard(
  when requirement: Bool,
  return consequence: t,
  otherwise alternative: fn() -> t,
) -> t {
  case requirement {
    True -> consequence
    False -> alternative()
  }
}
