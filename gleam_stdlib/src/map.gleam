import any
import result:Result
import expect

pub external type Map(key, value);

pub external fn new() -> Map(key, value))
  = 'maps' 'new'

test new {
  new()
    |> size
    |> expect:equal(_, 0)
}

pub external fn size(Map(_, _)) -> Int
  = 'maps' 'size'

test size {
  []
    |> from_list
    |> size
    |> expect:equal(_, 0)

  [
    {1, 1},
  ]
    |> from_list
    |> size
    |> expect:equal(_, 1)

  [
    {"", 1.0},
    {"", 2.0},
  ]
    |> from_list
    |> size
    |> expect:equal(_, 2)
}

external fn is_key(key, Map(key, _)) -> Bool
  = 'maps' 'is_key'

pub fn has_key(map, key) {
  is_key(key, map)
}

test has_key {
  []
    |> from_list
    |> has_key(_, 1)
    |> expect:false

  [
    {1, 0},
  ]
    |> from_list
    |> has_key(_, 1)
    |> expect:true

  [
    {4, 0},
    {1, 0},
  ]
    |> from_list
    |> has_key(_, 1)
    |> expect:true

  [
    {4, 0},
    {1, 0},
  ]
    |> from_list
    |> has_key(_, 0)
    |> expect:false
}

pub fn from_record(record: {r}) -> Map(Atom, any:Any) {
  any:unsafeCoerce(record)
}

test from_record {
  map = from_record({ name = "Jane" })

  map
    |> size
    |> expect:equal(_, 1)

  map
    |> expect:equal(_, from_list([{'name', "Jane"}]))
}

pub external fn to_list(Map(key, value)) -> List(Tuple(key, value))
  = 'maps' 'to_list'

pub external fn from_list(List(Tuple(key, value))) -> Map(key, value)
  = 'maps' 'from_list'

test from_list {
  proplist = [
    {4, 0},
    {1, 0},
  ]
  map = from_list(proplist)

  map
    |> size
    |> expect:equal(_, 2)

  map
    |> to_list
    |> expect:equal(_, proplist)
}

pub external fn fetch(Map(key, value), key) -> Result(Unit, value)
  = 'gleam__stdlib' 'map_fetch';

test fetch {
  proplist = [
    {4, 0},
    {1, 1},
  ]
  map = from_list(proplist)

  map
    |> fetch(_, 4)
    |> expect:equal(_, result:Ok(0))

  map
    |> fetch(_, 1)
    |> expect:equal(_, result:Ok(1))

  map
    |> fetch(_, 2)
    |> expect:equal(_, result:Error(()))
}

external fn erl_put(key, value, Map(key, value)) -> Map(key, value)
  = 'maps' 'put';

pub fn put(map, key, value) {
  erl_put(key, value, map)
}

test put {
  new()
    |> put(_, 'a', 0)
    |> put(_, 'b', 1)
    |> put(_, 'c', 2)
    |> expect:equal(_, result:Ok(from_list([{'a', 0}, {'b', 1}, {'c', 2}])))
}

external fn erl_map_values(fn(key, value) -> value, Map(key, value)) -> Map(key, value)
  = 'maps' 'map';

pub fn map_values(map, fun) {
  erl_map_values(fun, map)
}

test map_values {
  [
    {1, 0},
    {2, 1},
    {3, 2},
  ]
    |> from_list
    |> map_values(_, fn(k, v) { k + v })
    |> expect:equal(_, from_list([{1, 0}, {2, 3}, {3, 5}]))
}

pub external fn keys(Map(keys, _)) -> List(keys)
  = 'maps' 'keys'

test keys {
  [
    {'a', 0},
    {'b', 1},
    {'c', 2},
  ]
    |> from_list
    |> keys
    |> expect:equal(_, ['a', 'b', 'c']))
}

pub external fn values(Map(_, values)) -> List(values)
  = 'maps' 'values'

test values {
  [
    {'a', 0},
    {'b', 1},
    {'c', 2},
  ]
    |> from_list
    |> values
    |> expect:equal(_, [0, 1, 2]))
}

external fn erl_filter(fn(key, value) -> Bool, Map(key, value)) -> Map(key, value)
  = 'maps' 'filter';

pub fn filter(map, fun) {
  filter(fun, map)
}

test map_values {
  [
    {1, 0},
    {3, 2},
    {2, 1},
  ]
    |> from_list
    |> filter(_, fn(k, v) { k + v < 4 })
    |> expect:equal(_, from_list([{1, 0}, {2, 3}]))
}

