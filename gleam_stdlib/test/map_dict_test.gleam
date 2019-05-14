import expect
import map_dict
import str

pub fn from_list_test() {
  [
    {4, 0},
    {1, 0},
  ]
  |> map_dict:from_list
  |> map_dict:size
  |> expect:equal(_, 2)
}

pub fn has_key_test() {
  []
  |> map_dict:from_list
  |> map_dict:has_key(_, 1)
  |> expect:false

  [
      {1, 0},
  ]
  |> map_dict:from_list
  |> map_dict:has_key(_, 1)
  |> expect:true

  [
      {4, 0},
      {1, 0},
  ]
  |> map_dict:from_list
  |> map_dict:has_key(_, 1)
  |> expect:true

  [
    {4, 0},
    {1, 0},
  ]
  |> map_dict:from_list
  |> map_dict:has_key(_, 0)
  |> expect:false
}

pub fn new_test() {
  map_dict:new()
  |> map_dict:size
  |> expect:equal(_, 0)

  map_dict:new()
  |> map_dict:to_list
  |> expect:equal(_, [])
}

pub fn fetch_test() {
  let proplist = [
    {4, 0},
    {1, 1},
  ]
  let m = map_dict:from_list(proplist)

  m
  |> map_dict:fetch(_, 4)
  |> expect:equal(_, Ok(0))

  m
  |> map_dict:fetch(_, 1)
  |> expect:equal(_, Ok(1))

  m
  |> map_dict:fetch(_, 2)
  |> expect:is_error
}

pub fn put_test() {
  map_dict:new()
  |> map_dict:put(_, "a", 0)
  |> map_dict:put(_, "b", 1)
  |> map_dict:put(_, "c", 2)
  |> expect:equal(_, map_dict:from_list([{"a", 0}, {"b", 1}, {"c", 2}]))
}

pub fn map_values_test() {
  [
    {1, 0},
    {2, 1},
    {3, 2},
  ]
  |> map_dict:from_list
  |> map_dict:map_values(_, fn(k, v) { k + v })
  |> expect:equal(_, map_dict:from_list([{1, 1}, {2, 3}, {3, 5}]))
}

pub fn keys_test() {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
  |> map_dict:from_list
  |> map_dict:keys
  |> expect:equal(_, ["a", "b", "c"])
}

pub fn values_test() {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
  |> map_dict:from_list
  |> map_dict:values
  |> expect:equal(_, [0, 1, 2])
}

pub fn take_test() {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
  |> map_dict:from_list
  |> map_dict:take(_, ["a", "b", "d"])
  |> expect:equal(_, map_dict:from_list([{"a", 0}, {"b", 1}]))
}

pub fn drop_test() {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
  |> map_dict:from_list
  |> map_dict:drop(_, ["a", "b", "d"])
  |> expect:equal(_, map_dict:from_list([{"c", 2}]))
}

pub fn merge_test() {
  let a = map_dict:from_list([
    {"a", 2},
    {"c", 4},
    {"d", 3},
  ])
  let b = map_dict:from_list([
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ])

  map_dict:merge(a, b)
  |> expect:equal(_, map_dict:from_list([
      {"a", 0},
      {"b", 1},
      {"c", 2},
      {"d", 3},
    ]))

  map_dict:merge(b, a)
  |> expect:equal(_, map_dict:from_list([
      {"a", 2},
      {"b", 1},
      {"c", 4},
      {"d", 3},
    ]))
}

pub fn delete_test() {
  [
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ]
  |> map_dict:from_list
  |> map_dict:delete(_, "a")
  |> map_dict:delete(_, "d")
  |> expect:equal(_, map_dict:from_list([{"b", 1}, {"c", 2}]))
}

pub fn update_test() {
  let dict = map_dict:from_list([
    {"a", 0},
    {"b", 1},
    {"c", 2},
  ])

  let inc_or_zero = fn(x) {
    case x {
    | Ok(i) -> i + 1
    | Error(_) -> 0
    }
  }

  dict
  |> map_dict:update(_, "a", inc_or_zero)
  |> expect:equal(_, map_dict:from_list([{"a", 1}, {"b", 1}, {"c", 2}]))

  dict
  |> map_dict:update(_, "b", inc_or_zero)
  |> expect:equal(_, map_dict:from_list([{"a", 0}, {"b", 2}, {"c", 2}]))

  dict
  |> map_dict:update(_, "z", inc_or_zero)
  |> expect:equal(_, map_dict:from_list([{"a", 0}, {"b", 1}, {"c", 2}, {"z", 0}]))
}

pub fn fold_test() {
  let dict = map_dict:from_list([
    {"a", 0},
    {"b", 1},
    {"c", 2},
    {"d", 3},
  ])

  let add = fn(_, v, acc) {
    v + acc
  }

  dict
  |> map_dict:fold(_, 0, add)
  |> expect:equal(_, 6)

  let concat = fn(k, _, acc) {
    str:append(acc, k)
  }

  dict
  |> map_dict:fold(_, "", concat)
  |> expect:equal(_, "abcd")

  map_dict:from_list([])
  |> map_dict:fold(_, 0, add)
  |> expect:equal(_, 0)
}
