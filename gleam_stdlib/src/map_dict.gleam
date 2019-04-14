import any
import result

// TODO: drop
// TODO: take
// TODO: update :: fn(Map(k, v), k, fn(Result(v, NotFound)) -> v) -> Map(k, v)

pub external type Map(key, value);

pub enum NotFound =
  | NotFound

pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

pub external fn to_list(Map(key, value)) -> List({key, value})
  = "maps" "to_list"

pub external fn from_list(List({key, value})) -> Map(key, value)
  = "maps" "from_list"

external fn is_key(key, Map(key, v)) -> Bool
  = "maps" "is_key"

pub fn has_key(map, key) {
  is_key(key, map)
}

pub external fn new() -> Map(key, value)
  = "maps" "new"

pub external fn fetch(Map(key, value), key) -> Result(value, NotFound)
  = "gleam__stdlib" "map_fetch";

external fn erl_put(key, value, Map(key, value)) -> Map(key, value)
  = "maps" "put";

pub fn put(map, key, value) {
  erl_put(key, value, map)
}

external fn erl_map_values(fn(key, value) -> value, Map(key, value)) -> Map(key, value)
  = "maps" "map";

pub fn map_values(map, fun) {
  erl_map_values(fun, map)
}

pub external fn keys(Map(keys, v)) -> List(keys)
  = "maps" "keys"

pub external fn values(Map(k, values)) -> List(values)
  = "maps" "values"

external fn erl_filter(fn(key, value) -> Bool, Map(key, value)) -> Map(key, value)
  = "maps" "filter";

pub fn filter(map, fun) {
  erl_filter(fun, map)
}
