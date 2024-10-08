/// Returns the first element in a pair.
///
/// ## Examples
///
/// ```gleam
/// first(#(1, 2))
/// // -> 1
/// ```
///
pub fn first(pair: #(a, b)) -> a {
  let #(a, _) = pair
  a
}

/// Returns the second element in a pair.
///
/// ## Examples
///
/// ```gleam
/// second(#(1, 2))
/// // -> 2
/// ```
///
pub fn second(pair: #(a, b)) -> b {
  let #(_, a) = pair
  a
}

/// Returns a new pair with the elements swapped.
///
/// ## Examples
///
/// ```gleam
/// swap(#(1, 2))
/// // -> #(2, 1)
/// ```
///
pub fn swap(pair: #(a, b)) -> #(b, a) {
  let #(a, b) = pair
  #(b, a)
}

/// Returns a new pair with the first element having had `with` applied to
/// it.
///
/// ## Examples
///
/// ```gleam
/// #(1, 2) |> map_first(fn(n) { n * 2 })
/// // -> #(2, 2)
/// ```
///
pub fn map_first(of pair: #(a, b), with fun: fn(a) -> c) -> #(c, b) {
  let #(a, b) = pair
  #(fun(a), b)
}

/// Returns a new pair with the second element having had `with` applied to
/// it.
///
/// ## Examples
///
/// ```gleam
/// #(1, 2) |> map_second(fn(n) { n * 2 })
/// // -> #(1, 4)
/// ```
///
pub fn map_second(of pair: #(a, b), with fun: fn(b) -> c) -> #(a, c) {
  let #(a, b) = pair
  #(a, fun(b))
}

/// Returns a new pair with the given elements. This can also be done using the dedicated
/// syntax instead: `new(1, 2) == #(1, 2)`.
///
/// ## Examples
///
/// ```gleam
/// new(1, 2)
/// // -> #(1, 2)
/// ```
///
pub fn new(first: a, second: b) -> #(a, b) {
  #(first, second)
}
