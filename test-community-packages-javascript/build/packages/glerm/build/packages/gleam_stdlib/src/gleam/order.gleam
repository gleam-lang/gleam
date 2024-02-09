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
/// > negate(Lt)
/// Gt
/// ```
///
/// ```gleam
/// > negate(Eq)
/// Eq
/// ```
///
/// ```gleam
/// > negate(Lt)
/// Gt
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
/// > to_int(Lt)
/// -1
/// ```
///
/// ```gleam
/// > to_int(Eq)
/// 0
/// ```
///
/// ```gleam
/// > to_int(Gt)
/// 1
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
/// > compare(Eq, with: Lt)
/// Gt
/// ```
///
pub fn compare(a: Order, with b: Order) -> Order {
  case a, b {
    x, y if x == y -> Eq
    Lt, _ | Eq, Gt -> Lt
    _, _ -> Gt
  }
}

/// Returns the largest of two orders given that `Gt > Eq > Lt`.
///
/// ## Examples
///
/// ```gleam
/// > max(Eq, Lt)
/// Eq
/// ```
///
pub fn max(a: Order, b: Order) -> Order {
  case a, b {
    Gt, _ -> Gt
    Eq, Lt -> Eq
    _, _ -> b
  }
}

/// Returns the smallest of two orders given that `Gt > Eq > Lt`.
///
/// ## Examples
///
/// ```gleam
/// > min(Eq, Lt)
/// Lt
/// ```
///
pub fn min(a: Order, b: Order) -> Order {
  case a, b {
    Lt, _ -> Lt
    Eq, Gt -> Eq
    _, _ -> b
  }
}

/// Inverts an ordering function, so less-than becomes greater-than and greater-than
/// becomes less-than.
///
/// ## Examples
///
/// ```gleam
/// > list.sort([1, 5, 4], by: reverse(int.compare))
/// [5, 4, 1]
/// ```
///
pub fn reverse(orderer: fn(a, a) -> Order) -> fn(a, a) -> Order {
  fn(a, b) { orderer(b, a) }
}
