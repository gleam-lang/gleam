/// `Option` represents a value that may be present or not. `Some` means the value is
/// present, `None` means the value is not.
///
/// This is Gleam's alternative to having a value that could be Null, as is
/// possible in some other languages.
///
/// ## `Option` and `Result`
///
/// In other languages failible functions may return either `Result` or
/// `Option` depending on whether there is more information to be given about the
/// failure. In Gleam all failible functions return `Result`, and `Nil` is used
/// as the error if there is no extra detail to give. This consistency removes
/// the boilerplate that would otherwise be needed to convert between `Option`
/// and `Result` types, and makes APIs more predictable.
///
/// The `Option` type should only be used for taking optional values as
/// function arguments, or for storing them in other data structures.
///
pub type Option(a) {
  Some(a)
  None
}

fn do_all(list: List(Option(a)), acc: List(a)) -> Option(List(a)) {
  case list {
    [] -> Some(acc)
    [x, ..rest] -> {
      let accumulate = fn(acc, item) {
        case acc, item {
          Some(values), Some(value) -> Some([value, ..values])
          _, _ -> None
        }
      }
      accumulate(do_all(rest, acc), x)
    }
  }
}

/// Combines a list of `Option`s into a single `Option`.
/// If all elements in the list are `Some` then returns a `Some` holding the list of values.
/// If any element is `None` then returns`None`.
///
/// ## Examples
///
/// ```gleam
/// all([Some(1), Some(2)])
/// // -> Some([1, 2])
/// ```
///
/// ```gleam
/// all([Some(1), None])
/// // -> None
/// ```
///
pub fn all(list: List(Option(a))) -> Option(List(a)) {
  do_all(list, [])
}

/// Checks whether the `Option` is a `Some` value.
///
/// ## Examples
///
/// ```gleam
/// is_some(Some(1))
/// // -> True
/// ```
///
/// ```gleam
/// is_some(None)
/// // -> False
/// ```
///
pub fn is_some(option: Option(a)) -> Bool {
  option != None
}

/// Checks whether the `Option` is a `None` value.
///
/// ## Examples
///
/// ```gleam
/// is_none(Some(1))
/// // -> False
/// ```
///
/// ```gleam
/// is_none(None)
/// // -> True
/// ```
///
pub fn is_none(option: Option(a)) -> Bool {
  option == None
}

/// Converts an `Option` type to a `Result` type.
///
/// ## Examples
///
/// ```gleam
/// to_result(Some(1), "some_error")
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// to_result(None, "some_error")
/// // -> Error("some_error")
/// ```
///
pub fn to_result(option: Option(a), e) -> Result(a, e) {
  case option {
    Some(a) -> Ok(a)
    _ -> Error(e)
  }
}

/// Converts a `Result` type to an `Option` type.
///
/// ## Examples
///
/// ```gleam
/// from_result(Ok(1))
/// // -> Some(1)
/// ```
///
/// ```gleam
/// from_result(Error("some_error"))
/// // -> None
/// ```
///
pub fn from_result(result: Result(a, e)) -> Option(a) {
  case result {
    Ok(a) -> Some(a)
    _ -> None
  }
}

/// Extracts the value from an `Option`, returning a default value if there is none.
///
/// ## Examples
///
/// ```gleam
/// unwrap(Some(1), 0)
/// // -> 1
/// ```
///
/// ```gleam
/// unwrap(None, 0)
/// // -> 0
/// ```
///
pub fn unwrap(option: Option(a), or default: a) -> a {
  case option {
    Some(x) -> x
    None -> default
  }
}

/// Extracts the value from an `Option`, evaluating the default function if the option is `None`.
///
/// ## Examples
///
/// ```gleam
/// lazy_unwrap(Some(1), fn() { 0 })
/// // -> 1
/// ```
///
/// ```gleam
/// lazy_unwrap(None, fn() { 0 })
/// // -> 0
/// ```
///
pub fn lazy_unwrap(option: Option(a), or default: fn() -> a) -> a {
  case option {
    Some(x) -> x
    None -> default()
  }
}

/// Updates a value held within the `Some` of an `Option` by calling a given function
/// on it.
///
/// If the `Option` is a `None` rather than `Some`, the function is not called and the
/// `Option` stays the same.
///
/// ## Examples
///
/// ```gleam
/// map(over: Some(1), with: fn(x) { x + 1 })
/// // -> Some(2)
/// ```
///
/// ```gleam
/// map(over: None, with: fn(x) { x + 1 })
/// // -> None
/// ```
///
pub fn map(over option: Option(a), with fun: fn(a) -> b) -> Option(b) {
  case option {
    Some(x) -> Some(fun(x))
    None -> None
  }
}

/// Merges a nested `Option` into a single layer.
///
/// ## Examples
///
/// ```gleam
/// flatten(Some(Some(1)))
/// // -> Some(1)
/// ```
///
/// ```gleam
/// flatten(Some(None))
/// // -> None
/// ```
///
/// ```gleam
/// flatten(None)
/// // -> None
/// ```
///
pub fn flatten(option: Option(Option(a))) -> Option(a) {
  case option {
    Some(x) -> x
    None -> None
  }
}

/// Updates a value held within the `Some` of an `Option` by calling a given function
/// on it, where the given function also returns an `Option`. The two options are
/// then merged together into one `Option`.
///
/// If the `Option` is a `None` rather than `Some` the function is not called and the
/// option stays the same.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that return `Option`.
///
/// ## Examples
///
/// ```gleam
/// then(Some(1), fn(x) { Some(x + 1) })
/// // -> Some(2)
/// ```
///
/// ```gleam
/// then(Some(1), fn(x) { Some(#("a", x)) })
/// // -> Some(#("a", 1))
/// ```
///
/// ```gleam
/// then(Some(1), fn(_) { None })
/// // -> None
/// ```
///
/// ```gleam
/// then(None, fn(x) { Some(x + 1) })
/// // -> None
/// ```
///
pub fn then(option: Option(a), apply fun: fn(a) -> Option(b)) -> Option(b) {
  case option {
    Some(x) -> fun(x)
    None -> None
  }
}

/// Returns the first value if it is `Some`, otherwise returns the second value.
///
/// ## Examples
///
/// ```gleam
/// or(Some(1), Some(2))
/// // -> Some(1)
/// ```
///
/// ```gleam
/// or(Some(1), None)
/// // -> Some(1)
/// ```
///
/// ```gleam
/// or(None, Some(2))
/// // -> Some(2)
/// ```
///
/// ```gleam
/// or(None, None)
/// // -> None
/// ```
///
pub fn or(first: Option(a), second: Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second
  }
}

/// Returns the first value if it is `Some`, otherwise evaluates the given function for a fallback value.
///
/// ## Examples
///
/// ```gleam
/// lazy_or(Some(1), fn() { Some(2) })
/// // -> Some(1)
/// ```
///
/// ```gleam
/// lazy_or(Some(1), fn() { None })
/// // -> Some(1)
/// ```
///
/// ```gleam
/// lazy_or(None, fn() { Some(2) })
/// // -> Some(2)
/// ```
///
/// ```gleam
/// lazy_or(None, fn() { None })
/// // -> None
/// ```
///
pub fn lazy_or(first: Option(a), second: fn() -> Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second()
  }
}

fn do_values(list: List(Option(a)), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [first, ..rest] -> {
      let accumulate = fn(acc, item) {
        case item {
          Some(value) -> [value, ..acc]
          None -> acc
        }
      }
      accumulate(do_values(rest, acc), first)
    }
  }
}

/// Given a list of `Option`s,
/// returns only the values inside `Some`.
///
/// ## Examples
///
/// ```gleam
/// values([Some(1), None, Some(3)])
/// // -> [1, 3]
/// ```
///
pub fn values(options: List(Option(a))) -> List(a) {
  do_values(options, [])
}
