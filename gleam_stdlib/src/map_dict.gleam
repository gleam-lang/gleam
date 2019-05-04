import any
import result
import list

pub external type MapDict(key, value);

pub enum NotFound =
  | NotFound

pub external fn size(MapDict(k, v)) -> Int
  = "maps" "size"

pub external fn to_list(MapDict(key, value)) -> List({key, value})
  = "maps" "to_list"

pub external fn from_list(List({key, value})) -> MapDict(key, value)
  = "maps" "from_list"

external fn is_key(key, MapDict(key, v)) -> Bool
  = "maps" "is_key"

pub fn has_key(map, key) {
  is_key(key, map)
}

pub external fn new() -> MapDict(key, value)
  = "maps" "new"

pub external fn fetch(MapDict(key, value), key) -> Result(value, NotFound)
  = "gleam__stdlib" "map_fetch";

external fn erl_put(key, value, MapDict(key, value)) -> MapDict(key, value)
  = "maps" "put";

pub fn put(map, key, value) {
  erl_put(key, value, map)
}

external fn erl_map_values(fn(key, value) -> value, MapDict(key, value))
  -> MapDict(key, value)
  = "maps" "map";

pub fn map_values(map, fun) {
  erl_map_values(fun, map)
}

pub external fn keys(MapDict(keys, v)) -> List(keys)
  = "maps" "keys"

pub external fn values(MapDict(k, values)) -> List(values)
  = "maps" "values"

external fn erl_filter(fn(key, value) -> Bool, MapDict(key, value))
  -> MapDict(key, value)
  = "maps" "filter";

pub fn filter(map, fun) {
  erl_filter(fun, map)
}

external fn erl_take(List(k), MapDict(k, v)) -> MapDict(k, v) = "maps" "with"

pub fn take(map, keys) {
  erl_take(keys, map)
}

pub external fn merge(MapDict(k, v), MapDict(k, v)) -> MapDict(k, v) = "maps" "merge"

external fn erl_delete(k, MapDict(k, v)) -> MapDict(k, v) = "maps" "remove"

pub fn delete(map, key) {
  erl_delete(key, map)
}

pub fn drop(map, keys) {
  list:fold(keys, map, fn(key, acc) {
    delete(acc, key)
  })
}

pub external type NotFound;

pub fn update(dict, key, f) {
  case fetch(dict, key) {
  | Ok(value) -> put(dict, key, f(Ok(value)))
  | Error(_) -> put(dict, key, f(Error(NotFound)))
  }
}
