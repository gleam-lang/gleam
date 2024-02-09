import gleam/option.{type Option}
import gleam/dict

@deprecated("Please use the `gleam/dict` module instead")
pub type Map(key, value) =
  dict.Dict(key, value)

@deprecated("Please use the `gleam/dict` module instead")
pub fn size(map) -> Int {
  dict.size(map)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn to_list(map) -> List(#(key, value)) {
  dict.to_list(map)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn from_list(list: List(#(k, v))) {
  dict.from_list(list)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn has_key(map, key: k) -> Bool {
  dict.has_key(map, key)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn new() {
  dict.new()
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn get(from, get: key) -> Result(value, Nil) {
  dict.get(from, get)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn insert(into map, for key: k, insert value: v) {
  dict.insert(map, key, value)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn map_values(in map, with fun: fn(k, v) -> w) {
  dict.map_values(map, fun)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn keys(map) -> List(keys) {
  dict.keys(map)
}

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
fn do_keys(map) -> List(k) {
  let list_of_pairs =
    map
    |> to_list
  do_keys_acc(list_of_pairs, [])
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn values(map) -> List(values) {
  dict.values(map)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn filter(in map, keeping predicate: fn(k, v) -> Bool) {
  dict.filter(map, predicate)
}

@target(javascript)
fn do_filter(f: fn(key, value) -> Bool, map) {
  let insert = fn(map, k, v) {
    case f(k, v) {
      True -> insert(map, k, v)
      _ -> map
    }
  }
  map
  |> fold(from: new(), with: insert)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn take(from map, keeping desired_keys: List(k)) {
  dict.take(map, desired_keys)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn merge(into map, from new_entries) {
  dict.merge(map, new_entries)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn delete(from map, delete key: k) {
  dict.delete(map, key)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn drop(from map, drop disallowed_keys: List(k)) {
  dict.drop(map, disallowed_keys)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn update(in map, update key: k, with fun: fn(Option(v)) -> v) {
  dict.update(map, key, fun)
}

@deprecated("Please use the `gleam/dict` module instead")
pub fn fold(over map, from initial: acc, with fun: fn(acc, k, v) -> acc) -> acc {
  dict.fold(map, initial, fun)
}
