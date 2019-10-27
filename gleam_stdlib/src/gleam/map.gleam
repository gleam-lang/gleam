import gleam/any
import gleam/result
import gleam/list
import gleam/pair.{Pair}

pub external type Map(key, value);

pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

pub external fn to_list(Map(key, value)) -> List(Pair(key, value))
  = "maps" "to_list"

pub external fn from_list(List(Pair(key, value))) -> Map(key, value)
  = "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool
  = "maps" "is_key"

pub fn has_key(map, key) {
  is_key(key, map)
}

pub external fn new() -> Map(key, value)
  = "maps" "new"

pub external fn get(from: Map(key, value), get: key) -> Result(value, Nil)
  = "gleam_stdlib" "map_get";

external fn erl_insert(key, value, Map(key, value)) -> Map(key, value)
  = "maps" "put";

pub fn insert(into map, for key, insert value) {
  erl_insert(key, value, map)
}

external fn erl_map_values(fn(key, value) -> value, Map(key, value))
  -> Map(key, value)
  = "maps" "map";

pub fn map_values(in map, with fun) {
  erl_map_values(fun, map)
}

pub external fn keys(Map(keys, v)) -> List(keys)
  = "maps" "keys"

pub external fn values(Map(k, values)) -> List(values)
  = "maps" "values"

external fn erl_filter(fn(key, value) -> Bool, Map(key, value))
  -> Map(key, value)
  = "maps" "filter";

pub fn filter(in map, for predicate) {
  erl_filter(predicate, map)
}

external fn erl_take(List(k), Map(k, v)) -> Map(k, v) = "maps" "with"

pub fn take(from map, drop desired_keys) {
  erl_take(desired_keys, map)
}

pub external fn merge(into: Map(k, v), merge: Map(k, v)) -> Map(k, v) = "maps" "merge"

external fn erl_delete(k, Map(k, v)) -> Map(k, v) = "maps" "remove"

pub fn delete(from map, delete key) {
  erl_delete(key, map)
}

pub fn drop(from map, drop disallowed_keys) {
  list.fold(disallowed_keys, map, fn(key, acc) {
    delete(acc, key)
  })
}

pub fn update(in map, update key, with fun) {
  case get(map, key) {
  | Ok(value) -> insert(map, key, fun(Ok(value)))
  | Error(_) -> insert(map, key, fun(Error(Nil)))
  }
}

fn do_fold(list, initial, fun) {
  case list {
    | [] -> initial
    | [Pair(k, v) | tail] -> do_fold(tail, fun(k, v, initial), fun)
  }
}

pub fn fold(map, from initial, with fun) {
  let kvs = to_list(map)
  do_fold(kvs, initial, fun)
}
