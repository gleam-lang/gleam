import any
import result
import expect

pub external type Map(key, value);

pub external fn new() -> Map(key, value)
  = "maps" "new"

// test new {
//   new()
//     |> new
//     |> expect:equal(_, [])
// }

pub external fn size(Map(k, v)) -> Int
  = "maps" "size"

// test size {
//   let _ = []
//     |> from_list
//     |> size
//     |> expect:equal(_, 0)

//   let _ = [
//       {1, 1},
//     ]
//     |> from_list
//     |> size
//     |> expect:equal(_, 1)

//   [
//       {"", 1.0},
//       {"", 2.0},
//   ]
//     |> from_list
//     |> size
//     |> expect:equal(_, 2)
// }

// external fn is_key(key, Map(key, v)) -> Bool
//   = "maps" "is_key"

// pub fn has_key(map, key) {
//   is_key(key, map)
// }

// test has_key {
//   let _ = []
//     |> from_list
//     |> has_key(_, 1)
//     |> expect:false

//   let _ = [
//       {1, 0},
//     ]
//     |> from_list
//     |> has_key(_, 1)
//     |> expect:true

//   let _ = [
//       {4, 0},
//       {1, 0},
//     ]
//     |> from_list
//     |> has_key(_, 1)
//     |> expect:true

//   [
//     {4, 0},
//     {1, 0},
//   ]
//     |> from_list
//     |> has_key(_, 0)
//     |> expect:false
// }

// pub fn from_record(record: {r}) -> Map(Atom, any:Any) {
//   any:unsafeCoerce(record)
// }

// test from_record {
//   let map = from_record({ name = "Jane" })

//   let _ = map
//     |> size
//     |> expect:equal(_, 1)

//   map
//     |> expect:equal(_, from_list([{"name", "Jane"}]))
// }

pub external fn to_list(Map(key, value)) -> List({key, value})
  = "maps" "to_list"

pub external fn from_list(List({key, value})) -> Map(key, value)
  = "maps" "from_list"

test from_list {
  let proplist = [
    {4, 0},
    {1, 0},
  ]
  let map = from_list(proplist)

  let _ = map
    |> size
    |> expect:equal(_, 2)

  map
    |> to_list
    |> expect:equal(_, proplist)
}

pub external fn fetch(Map(key, value), key) -> Result(a, value)
  = "gleam__stdlib" "map_fetch";

test fetch {
  let proplist = [
    {4, 0},
    {1, 1},
  ]
  let map = from_list(proplist)

  let _ = map
    |> fetch(_, 4)
    |> expect:equal(_, Ok(0))

  map
    |> fetch(_, 1)
    |> expect:equal(_, Ok(1))

  // map
  //   |> fetch(_, 2)
  //   |> expect:equal(_, Error(())
}

// external fn erl_put(key, value, Map(key, value)) -> Map(key, value)
//   = "maps" "put";

// pub fn put(map, key, value) {
//   erl_put(key, value, map)
// }

// test put {
//   new()
//     |> put(_, "a", 0)
//     |> put(_, "b", 1)
//     |> put(_, "c", 2)
//     |> expect:equal(_, Ok(from_list([{"a", 0}, {"b", 1}, {"c", 2}])))
// }

external fn erl_map_values(fn(key, value) -> value, Map(key, value)) -> Map(key, value)
  = "maps" "map";

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

pub external fn keys(Map(keys, v)) -> List(keys)
  = "maps" "keys"

test keys {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
    |> from_list
    |> keys
    |> expect:equal(_, ["a", "b", "c"])
}

pub external fn values(Map(k, values)) -> List(values)
  = "maps" "values"

test values {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
    |> from_list
    |> values
    |> expect:equal(_, [0, 1, 2])
}

external fn erl_filter(fn(key, value) -> Bool, Map(key, value)) -> Map(key, value)
  = "maps" "filter";

pub fn filter(map, fun) {
  filter(fun, map)
}
