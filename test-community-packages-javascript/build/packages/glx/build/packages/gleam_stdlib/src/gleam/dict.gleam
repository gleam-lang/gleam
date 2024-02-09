import gleam/option.{type Option}

/// A dictionary of keys and values.
///
/// Any type can be used for the keys and values of a dict, but all the keys
/// must be of the same type and all the values must be of the same type.
///
/// Each key can only be present in a dict once.
///
/// Dicts are not ordered in any way, and any unintentional ordering is not to
/// be relied upon in your code as it may change in future versions of Erlang
/// or Gleam.
///
/// See [the Erlang map module](https://erlang.org/doc/man/maps.html) for more
/// information.
///
pub type Dict(key, value)

/// Determines the number of key-value pairs in the dict.
/// This function runs in constant time and does not need to iterate the dict.
///
/// ## Examples
///
/// ```gleam
/// > new() |> size()
/// 0
/// ```
///
/// ```gleam
/// > new() |> insert("key", "value") |> size()
/// 1
/// ```
///
pub fn size(dict: Dict(k, v)) -> Int {
  do_size(dict)
}

@external(erlang, "maps", "size")
@external(javascript, "../gleam_stdlib.mjs", "map_size")
fn do_size(a: Dict(k, v)) -> Int

/// Converts the dict to a list of 2-element tuples `#(key, value)`, one for
/// each key-value pair in the dict.
///
/// The tuples in the list have no specific order.
///
/// ## Examples
///
/// ```gleam
/// > new() |> to_list()
/// []
/// ```
///
/// ```gleam
/// > new() |> insert("key", 0) |> to_list()
/// [#("key", 0)]
/// ```
///
pub fn to_list(dict: Dict(key, value)) -> List(#(key, value)) {
  do_to_list(dict)
}

@external(erlang, "maps", "to_list")
@external(javascript, "../gleam_stdlib.mjs", "map_to_list")
fn do_to_list(a: Dict(key, value)) -> List(#(key, value))

/// Converts a list of 2-element tuples `#(key, value)` to a dict.
///
/// If two tuples have the same key the last one in the list will be the one
/// that is present in the dict.
///
pub fn from_list(list: List(#(k, v))) -> Dict(k, v) {
  do_from_list(list)
}

@target(erlang)
@external(erlang, "maps", "from_list")
fn do_from_list(a: List(#(key, value))) -> Dict(key, value)

@target(javascript)
fn fold_list_of_pair(
  over list: List(#(k, v)),
  from initial: Dict(k, v),
) -> Dict(k, v) {
  case list {
    [] -> initial
    [x, ..rest] -> fold_list_of_pair(rest, insert(initial, x.0, x.1))
  }
}

@target(javascript)
fn do_from_list(list: List(#(k, v))) -> Dict(k, v) {
  fold_list_of_pair(list, new())
}

/// Determines whether or not a value present in the dict for a given key.
///
/// ## Examples
///
/// ```gleam
/// > new() |> insert("a", 0) |> has_key("a")
/// True
/// ```
///
/// ```gleam
/// > new() |> insert("a", 0) |> has_key("b")
/// False
/// ```
///
pub fn has_key(dict: Dict(k, v), key: k) -> Bool {
  do_has_key(key, dict)
}

@target(erlang)
@external(erlang, "maps", "is_key")
fn do_has_key(a: key, b: Dict(key, v)) -> Bool

@target(javascript)
fn do_has_key(key: k, dict: Dict(k, v)) -> Bool {
  get(dict, key) != Error(Nil)
}

/// Creates a fresh dict that contains no values.
///
pub fn new() -> Dict(key, value) {
  do_new()
}

@external(erlang, "maps", "new")
@external(javascript, "../gleam_stdlib.mjs", "new_map")
fn do_new() -> Dict(key, value)

/// Fetches a value from a dict for a given key.
///
/// The dict may not have a value for the key, so the value is wrapped in a
/// `Result`.
///
/// ## Examples
///
/// ```gleam
/// > new() |> insert("a", 0) |> get("a")
/// Ok(0)
/// ```
///
/// ```gleam
/// > new() |> insert("a", 0) |> get("b")
/// Error(Nil)
/// ```
///
pub fn get(from: Dict(key, value), get: key) -> Result(value, Nil) {
  do_get(from, get)
}

@external(erlang, "gleam_stdlib", "map_get")
@external(javascript, "../gleam_stdlib.mjs", "map_get")
fn do_get(a: Dict(key, value), b: key) -> Result(value, Nil)

/// Inserts a value into the dict with the given key.
///
/// If the dict already has a value for the given key then the value is
/// replaced with the new value.
///
/// ## Examples
///
/// ```gleam
/// > new() |> insert("a", 0) |> to_list
/// [#("a", 0)]
/// ```
///
/// ```gleam
/// > new() |> insert("a", 0) |> insert("a", 5) |> to_list
/// [#("a", 5)]
/// ```
///
pub fn insert(into dict: Dict(k, v), for key: k, insert value: v) -> Dict(k, v) {
  do_insert(key, value, dict)
}

@external(erlang, "maps", "put")
@external(javascript, "../gleam_stdlib.mjs", "map_insert")
fn do_insert(a: key, b: value, c: Dict(key, value)) -> Dict(key, value)

/// Updates all values in a given dict by calling a given function on each key
/// and value.
///
/// ## Examples
///
/// ```gleam
/// > [#(3, 3), #(2, 4)]
/// > |> from_list
/// > |> map_values(fn(key, value) { key * value })
/// [#(3, 9), #(2, 8)]
/// ```
///
pub fn map_values(in dict: Dict(k, v), with fun: fn(k, v) -> w) -> Dict(k, w) {
  do_map_values(fun, dict)
}

@target(erlang)
@external(erlang, "maps", "map")
fn do_map_values(a: fn(key, value) -> b, b: Dict(key, value)) -> Dict(key, b)

@target(javascript)
fn do_map_values(f: fn(key, value) -> b, dict: Dict(key, value)) -> Dict(key, b) {
  let f = fn(dict, k, v) { insert(dict, k, f(k, v)) }
  dict
  |> fold(from: new(), with: f)
}

/// Gets a list of all keys in a given dict.
///
/// Dicts are not ordered so the keys are not returned in any specific order. Do
/// not write code that relies on the order keys are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
/// ```gleam
/// > keys([#("a", 0), #("b", 1)])
/// ["a", "b"]
/// ```
///
pub fn keys(dict: Dict(keys, v)) -> List(keys) {
  do_keys(dict)
}

@target(erlang)
@external(erlang, "maps", "keys")
fn do_keys(a: Dict(keys, v)) -> List(keys)

@target(javascript)
fn reverse_and_concat(remaining, accumulator) {
  case remaining {
    [] -> accumulator
    [item, ..rest] -> reverse_and_concat(rest, [item, ..accumulator])
  }
}

@target(javascript)
fn do_keys_acc(list: List(#(k, v)), acc: List(k)) -> List(k) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [x, ..xs] -> do_keys_acc(xs, [x.0, ..acc])
  }
}

@target(javascript)
fn do_keys(dict: Dict(k, v)) -> List(k) {
  let list_of_pairs = to_list(dict)
  do_keys_acc(list_of_pairs, [])
}

/// Gets a list of all values in a given dict.
///
/// Dicts are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order values are returned by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// ## Examples
///
/// ```gleam
/// > values(from_list([#("a", 0), #("b", 1)]))
/// [0, 1]
/// ```
///
pub fn values(dict: Dict(k, values)) -> List(values) {
  do_values(dict)
}

@target(erlang)
@external(erlang, "maps", "values")
fn do_values(a: Dict(k, values)) -> List(values)

@target(javascript)
fn do_values_acc(list: List(#(k, v)), acc: List(v)) -> List(v) {
  case list {
    [] -> reverse_and_concat(acc, [])
    [x, ..xs] -> do_values_acc(xs, [x.1, ..acc])
  }
}

@target(javascript)
fn do_values(dict: Dict(k, v)) -> List(v) {
  let list_of_pairs = to_list(dict)
  do_values_acc(list_of_pairs, [])
}

/// Creates a new dict from a given dict, minus any entries that a given function
/// returns `False` for.
///
/// ## Examples
///
/// ```gleam
/// > from_list([#("a", 0), #("b", 1)])
/// > |> filter(fn(key, value) { value != 0 })
/// from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// > from_list([#("a", 0), #("b", 1)])
/// > |> filter(fn(key, value) { True })
/// from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn filter(
  in dict: Dict(k, v),
  keeping predicate: fn(k, v) -> Bool,
) -> Dict(k, v) {
  do_filter(predicate, dict)
}

@target(erlang)
@external(erlang, "maps", "filter")
fn do_filter(a: fn(key, value) -> Bool, b: Dict(key, value)) -> Dict(key, value)

@target(javascript)
fn do_filter(
  f: fn(key, value) -> Bool,
  dict: Dict(key, value),
) -> Dict(key, value) {
  let insert = fn(dict, k, v) {
    case f(k, v) {
      True -> insert(dict, k, v)
      _ -> dict
    }
  }
  dict
  |> fold(from: new(), with: insert)
}

/// Creates a new dict from a given dict, only including any entries for which the
/// keys are in a given list.
///
/// ## Examples
///
/// ```gleam
/// > from_list([#("a", 0), #("b", 1)])
/// > |> take(["b"])
/// from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// > from_list([#("a", 0), #("b", 1)])
/// > |> take(["a", "b", "c"])
/// from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn take(from dict: Dict(k, v), keeping desired_keys: List(k)) -> Dict(k, v) {
  do_take(desired_keys, dict)
}

@target(erlang)
@external(erlang, "maps", "with")
fn do_take(a: List(k), b: Dict(k, v)) -> Dict(k, v)

@target(javascript)
fn insert_taken(
  dict: Dict(k, v),
  desired_keys: List(k),
  acc: Dict(k, v),
) -> Dict(k, v) {
  let insert = fn(taken, key) {
    case get(dict, key) {
      Ok(value) -> insert(taken, key, value)
      _ -> taken
    }
  }
  case desired_keys {
    [] -> acc
    [x, ..xs] -> insert_taken(dict, xs, insert(acc, x))
  }
}

@target(javascript)
fn do_take(desired_keys: List(k), dict: Dict(k, v)) -> Dict(k, v) {
  insert_taken(dict, desired_keys, new())
}

/// Creates a new dict from a pair of given dicts by combining their entries.
///
/// If there are entries with the same keys in both dicts the entry from the
/// second dict takes precedence.
///
/// ## Examples
///
/// ```gleam
/// > let a = from_list([#("a", 0), #("b", 1)])
/// > let b = from_list([#("b", 2), #("c", 3)])
/// > merge(a, b)
/// from_list([#("a", 0), #("b", 2), #("c", 3)])
/// ```
///
pub fn merge(into dict: Dict(k, v), from new_entries: Dict(k, v)) -> Dict(k, v) {
  do_merge(dict, new_entries)
}

@target(erlang)
@external(erlang, "maps", "merge")
fn do_merge(a: Dict(k, v), b: Dict(k, v)) -> Dict(k, v)

@target(javascript)
fn insert_pair(dict: Dict(k, v), pair: #(k, v)) -> Dict(k, v) {
  insert(dict, pair.0, pair.1)
}

@target(javascript)
fn fold_inserts(new_entries: List(#(k, v)), dict: Dict(k, v)) -> Dict(k, v) {
  case new_entries {
    [] -> dict
    [x, ..xs] -> fold_inserts(xs, insert_pair(dict, x))
  }
}

@target(javascript)
fn do_merge(dict: Dict(k, v), new_entries: Dict(k, v)) -> Dict(k, v) {
  new_entries
  |> to_list
  |> fold_inserts(dict)
}

/// Creates a new dict from a given dict with all the same entries except for the
/// one with a given key, if it exists.
///
/// ## Examples
///
/// ```gleam
/// > delete([#("a", 0), #("b", 1)], "a")
/// from_list([#("b", 1)])
/// ```
///
/// ```gleam
/// > delete([#("a", 0), #("b", 1)], "c")
/// from_list([#("a", 0), #("b", 1)])
/// ```
///
pub fn delete(from dict: Dict(k, v), delete key: k) -> Dict(k, v) {
  do_delete(key, dict)
}

@external(erlang, "maps", "remove")
@external(javascript, "../gleam_stdlib.mjs", "map_remove")
fn do_delete(a: k, b: Dict(k, v)) -> Dict(k, v)

/// Creates a new dict from a given dict with all the same entries except any with
/// keys found in a given list.
///
/// ## Examples
///
/// ```gleam
/// > drop([#("a", 0), #("b", 1)], ["a"])
/// from_list([#("b", 2)])
/// ```
///
/// ```gleam
/// > delete([#("a", 0), #("b", 1)], ["c"])
/// from_list([#("a", 0), #("b", 1)])
/// ```
///
/// ```gleam
/// > drop([#("a", 0), #("b", 1)], ["a", "b", "c"])
/// from_list([])
/// ```
///
pub fn drop(from dict: Dict(k, v), drop disallowed_keys: List(k)) -> Dict(k, v) {
  case disallowed_keys {
    [] -> dict
    [x, ..xs] -> drop(delete(dict, x), xs)
  }
}

/// Creates a new dict with one entry updated using a given function.
///
/// If there was not an entry in the dict for the given key then the function
/// gets `None` as its argument, otherwise it gets `Some(value)`.
///
/// ## Example
///
/// ```gleam
/// > let increment = fn(x) {
/// >   case x {
/// >     Some(i) -> i + 1
/// >     None -> 0
/// >   }
/// > }
/// > let dict = from_list([#("a", 0)])
/// >
/// > update(dict, "a", increment)
/// from_list([#("a", 1)])
/// ```
///
/// ```gleam
/// > update(dict, "b", increment)
/// from_list([#("a", 0), #("b", 0)])
/// ```
///
pub fn update(
  in dict: Dict(k, v),
  update key: k,
  with fun: fn(Option(v)) -> v,
) -> Dict(k, v) {
  dict
  |> get(key)
  |> option.from_result
  |> fun
  |> insert(dict, key, _)
}

fn do_fold(list: List(#(k, v)), initial: acc, fun: fn(acc, k, v) -> acc) -> acc {
  case list {
    [] -> initial
    [#(k, v), ..rest] -> do_fold(rest, fun(initial, k, v), fun)
  }
}

/// Combines all entries into a single value by calling a given function on each
/// one.
///
/// Dicts are not ordered so the values are not returned in any specific order. Do
/// not write code that relies on the order entries are used by this function
/// as it may change in later versions of Gleam or Erlang.
///
/// # Examples
///
/// ```gleam
/// > let dict = from_list([#("a", 1), #("b", 3), #("c", 9)])
/// > fold(dict, 0, fn(accumulator, key, value) { accumulator + value })
/// 13
/// ```
///
/// ```gleam
/// > import gleam/string.{append}
/// > fold(dict, "", fn(accumulator, key, value) { append(accumulator, key) })
/// "abc"
/// ```
///
pub fn fold(
  over dict: Dict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  dict
  |> to_list
  |> do_fold(initial, fun)
}
