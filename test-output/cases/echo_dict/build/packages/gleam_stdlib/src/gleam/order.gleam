/// Represents the result of a single comparison to determine the precise
/// ordering of two values.
///
pub type Order {
  /// Less-than
  Lt

  /// Equal
  Eq

  /// Greater than
  Gt
}

/// Inverts an order, so less-than becomes greater-than and greater-than
/// becomes less-than.
///
/// ## Examples
///
/// ```gleam
/// negate(Lt)
/// // -> Gt
/// ```
///
/// ```gleam
/// negate(Eq)
/// // -> Eq
/// ```
///
/// ```gleam
/// negate(Gt)
/// // -> Lt
/// ```
///
pub fn negate(order: Order) -> Order {
  case order {
    Lt -> Gt
    Eq -> Eq
    Gt -> Lt
  }
}

/// Produces a numeric representation of the order.
///
/// ## Examples
///
/// ```gleam
/// to_int(Lt)
/// // -> -1
/// ```
///
/// ```gleam
/// to_int(Eq)
/// // -> 0
/// ```
///
/// ```gleam
/// to_int(Gt)
/// // -> 1
/// ```
///
pub fn to_int(order: Order) -> Int {
  case order {
    Lt -> -1
    Eq -> 0
    Gt -> 1
  }
}

/// Compares two `Order` values to one another, producing a new `Order`.
///
/// ## Examples
///
/// ```gleam
/// compare(Eq, with: Lt)
/// // -> Gt
/// ```
///
pub fn compare(a: Order, with b: Order) -> Order {
  case a, b {
    x, y if x == y -> Eq
    Lt, _ | Eq, Gt -> Lt
    _, _ -> Gt
  }
}

/// Inverts an ordering function, so less-than becomes greater-than and greater-than
/// becomes less-than.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
/// import gleam/list
///
/// list.sort([1, 5, 4], by: reverse(int.compare))
/// // -> [5, 4, 1]
/// ```
///
pub fn reverse(orderer: fn(a, a) -> Order) -> fn(a, a) -> Order {
  fn(a, b) { orderer(b, a) }
}

/// Return a fallback `Order` in case the first argument is `Eq`.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// break_tie(in: int.compare(1, 1), with: Lt)
/// // -> Lt
/// ```
///
/// ```gleam
/// import gleam/int
///
/// break_tie(in: int.compare(1, 0), with: Eq)
/// // -> Gt
/// ```
///
pub fn break_tie(in order: Order, with other: Order) -> Order {
  case order {
    Lt | Gt -> order
    Eq -> other
  }
}

/// Invokes a fallback function returning an `Order` in case the first argument
/// is `Eq`.
///
/// This can be useful when the fallback comparison might be expensive and it
/// needs to be delayed until strictly necessary.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// lazy_break_tie(in: int.compare(1, 1), with: fn() { Lt })
/// // -> Lt
/// ```
///
/// ```gleam
/// import gleam/int
///
/// lazy_break_tie(in: int.compare(1, 0), with: fn() { Eq })
/// // -> Gt
/// ```
///
pub fn lazy_break_tie(in order: Order, with comparison: fn() -> Order) -> Order {
  case order {
    Lt | Gt -> order
    Eq -> comparison()
  }
}
