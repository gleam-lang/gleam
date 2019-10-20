import gleam/any
import gleam/result
import gleam/list
import gleam/pair

pub external type Map(key, value);

pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

pub external fn to_list(Map(key, value)) -> List(pair.Pair(key, value))
  = "maps" "to_list"

pub external fn from_list(List(pair.Pair(key, value))) -> Map(key, value)
  = "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool
  = "maps" "is_key"

pub fn has_key(map, key) {
  is_key(key, map)
}

pub external fn new() -> Map(key, value)
  = "maps" "new"

pub external fn get(Map(key, value), key) -> Result(value, Nil)
  = "gleam_stdlib" "map_get";

external fn erl_insert(key, value, Map(key, value)) -> Map(key, value)
  = "maps" "put";

pub fn insert(map, key, value) {
  erl_insert(key, value, map)
}

external fn erl_map_values(fn(key, value) -> value, Map(key, value))
  -> Map(key, value)
  = "maps" "map";

pub fn map_values(map, fun) {
  erl_map_values(fun, map)
}

pub external fn keys(Map(keys, v)) -> List(keys)
  = "maps" "keys"

pub external fn values(Map(k, values)) -> List(values)
  = "maps" "values"

external fn erl_filter(fn(key, value) -> Bool, Map(key, value))
  -> Map(key, value)
  = "maps" "filter";

pub fn filter(map, fun) {
  erl_filter(fun, map)
}

external fn erl_take(List(k), Map(k, v)) -> Map(k, v) = "maps" "with"

pub fn take(map, keys) {
  erl_take(keys, map)
}

pub external fn merge(Map(k, v), Map(k, v)) -> Map(k, v) = "maps" "merge"

external fn erl_delete(k, Map(k, v)) -> Map(k, v) = "maps" "remove"

pub fn delete(map, key) {
  erl_delete(key, map)
}

pub fn drop(map, keys) {
  list.fold(keys, map, fn(key, acc) {
    delete(acc, key)
  })
}

pub fn update(map, key, f) {
  case get(map, key) {
  | Ok(value) -> insert(map, key, f(Ok(value)))
  | Error(_) -> insert(map, key, f(Error(Nil)))
  }
}

fn do_fold(list, acc, f) {
  case list {
    | [] -> acc
    | [pair.Pair(k, v) | tail] -> do_fold(tail, f(k, v, acc), f)
  }
}

pub fn fold(map, acc, f) {
  let kvs = to_list(map)
  do_fold(kvs, acc, f)
}
