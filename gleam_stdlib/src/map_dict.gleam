import any
import result

// TODO: update :: fn(MapDict(k, v), k, fn(Result(v, NotFound)) -> v) -> MapDict(k, v)
// TODO: merge :: fn(MapDict(k, v), MapDict(k, v)) -> MapDict(k, v)
// TODO: delete :: fn(MapDict(k, v), k) -> MapDict(k, v)

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

external fn erl_drop(List(k), MapDict(k, v)) -> MapDict(k, v) = "maps" "without"

pub fn drop(map, keys) {
  erl_drop(keys, map)
}
