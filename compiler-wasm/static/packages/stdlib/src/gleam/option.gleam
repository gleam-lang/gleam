import gleam/list

/// Option represents a value that may be present or not. Some means the value is
/// present, None means the value is not.
///
/// This is Gleam's alternative to having a value that could be Null, as is
/// possible in some other languages.
///
pub type Option(a) {
  Some(a)
  None
}

/// Combines a list of options into a single option.
/// If all elements in the list are Some then returns a Some holding the list of values.
/// If any element is None then returns None.
///
/// ## Examples
///
/// ```
///    > all([Some(1), Some(2)])
///    Some([1, 2])
///
///    > all([Some(1), None])
///    None
/// ```
///
pub fn all(list: List(Option(a))) -> Option(List(a)) {
  list.fold_right(
    list,
    from: Some([]),
    with: fn(acc, item) {
      case acc, item {
        Some(values), Some(value) -> Some([value, ..values])
        _, _ -> None
      }
    },
  )
}

/// Checks whether the option is a Some value.
///
/// ## Examples
///
///    > is_some(Some(1))
///    True
///
///    > is_some(None)
///    False
///
pub fn is_some(option: Option(a)) -> Bool {
  option != None
}

/// Checks whether the option is a None value.
///
/// ## Examples
///
///    > is_none(Some(1))
///    False
///
///    > is_none(None)
///    True
///
pub fn is_none(option: Option(a)) -> Bool {
  option == None
}

/// Converts an Option type to a Result type
///
/// ## Examples
///
///    > to_result(Some(1), "some_error")
///    Ok(1)
///    > to_result(None, "some_error")
///    Error("some_error")
///
pub fn to_result(option: Option(a), e) -> Result(a, e) {
  case option {
    Some(a) -> Ok(a)
    _ -> Error(e)
  }
}

/// Converts a Result type to an Option type
///
/// ## Examples
///
///    > from_result(Ok(1))
///    Some(1)
///    > from_result(Error("some_error"))
///    None
///
pub fn from_result(result: Result(a, e)) -> Option(a) {
  case result {
    Ok(a) -> Some(a)
    _ -> None
  }
}

/// Extracts the value from an option, returning a default value if there is none.
///
/// ## Examples
///
///    > unwrap(Some(1), 0)
///    1
///
///    > unwrap(None, 0)
///    0
///
pub fn unwrap(option: Option(a), or default: a) -> a {
  case option {
    Some(x) -> x
    None -> default
  }
}

/// Updates a value held within the Some of an Option by calling a given function
/// on it.
///
/// If the option is a None rather than Some the function is not called and the
/// option stays the same.
///
/// ## Examples
///
///    > map(over: Some(1), with: fn(x) { x + 1 })
///    Some(2)
///
///    > map(over: None, with: fn(x) { x + 1 })
///    None
///
pub fn map(over option: Option(a), with fun: fn(a) -> b) -> Option(b) {
  case option {
    Some(x) -> Some(fun(x))
    None -> None
  }
}

/// Merges a nested Option into a single layer.
///
/// ## Examples
///
///    > flatten(Some(Some(1)))
///    Some(1)
///
///    > flatten(Some(None))
///    None
///
///    > flatten(None)
///    None
///
pub fn flatten(option: Option(Option(a))) -> Option(a) {
  case option {
    Some(x) -> x
    None -> None
  }
}

/// Updates a value held within the Some of an Option by calling a given function
/// on it, where the given function also returns an Option. The two Options are
/// then merged together into one Option.
///
/// If the Option is a None rather than Some the function is not called and the
/// Option stays the same.
///
/// This function is the equivalent of calling `map` followed by `flatten`, and
/// it is useful for chaining together multiple functions that return Options.
///
/// ## Examples
///
///    > then(Some(1), fn(x) { Some(x + 1) })
///    Some(2)
///
///    > then(Some(1), fn(x) { Some(#("a", x)) })
///    Some(#("a", 1))
///
///    > then(Some(1), fn(_) { None })
///    None
///
///    > then(None, fn(x) { Some(x + 1) })
///    None
///
pub fn then(option: Option(a), apply fun: fn(a) -> Option(b)) -> Option(b) {
  case option {
    Some(x) -> fun(x)
    None -> None
  }
}

/// Returns the first value if it is Some, otherwise return the second value.
///
/// ## Examples
///
///    > or(Some(1), Some(2))
///    Some(1)
///
///    > or(Some(1), None)
///    Some(1)
///
///    > or(None, Some(2))
///    Some(2)
///
///    > or(None, None)
///    None
///
pub fn or(first: Option(a), second: Option(a)) -> Option(a) {
  case first {
    Some(_) -> first
    None -> second
  }
}

/// Given a list of options
/// Return only the values inside Some
///
/// ## Examples
///
/// ```
/// > values([Some(1), None, Some(3)])
/// [1, 3]
/// ```
///
pub fn values(options: List(Option(a))) -> List(a) {
  list.filter_map(options, fn(op) { to_result(op, "") })
}
