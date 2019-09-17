import gleam/string
import gleam/expect
import gleam/map
import gleam/pair

pub fn from_list_test() {
  [
    pair.Pair(4, 0),
    pair.Pair(1, 0),
  ]
  |> map.from_list
  |> map.size
  |> expect.equal(_, 2)
}

pub fn has_key_test() {
  []
  |> map.from_list
  |> map.has_key(_, 1)
  |> expect.false

  [
      pair.Pair(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> expect.true

  [
      pair.Pair(4, 0),
      pair.Pair(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> expect.true

  [
    pair.Pair(4, 0),
    pair.Pair(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 0)
  |> expect.false
}

pub fn new_test() {
  map.new()
  |> map.size
  |> expect.equal(_, 0)

  map.new()
  |> map.to_list
  |> expect.equal(_, [])
}

pub fn fetch_test() {
  let proplist = [
    pair.Pair(4, 0),
    pair.Pair(1, 1),
  ]
  let m = map.from_list(proplist)

  m
  |> map.fetch(_, 4)
  |> expect.equal(_, Ok(0))

  m
  |> map.fetch(_, 1)
  |> expect.equal(_, Ok(1))

  m
  |> map.fetch(_, 2)
  |> expect.equal(_, Error(Nil))
}

pub fn put_test() {
  map.new()
  |> map.put(_, "a", 0)
  |> map.put(_, "b", 1)
  |> map.put(_, "c", 2)
  |> expect.equal(_, map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]))
}

pub fn map_values_test() {
  [
    pair.Pair(1, 0),
    pair.Pair(2, 1),
    pair.Pair(3, 2),
  ]
  |> map.from_list
  |> map.map_values(_, fn(k, v) { k + v })
  |> expect.equal(_, map.from_list([
    pair.Pair(1, 1),
    pair.Pair(2, 3),
    pair.Pair(3, 5),
  ]))
}

pub fn keys_test() {
  [
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]
  |> map.from_list
  |> map.keys
  |> expect.equal(_, ["a", "b", "c"])
}

pub fn values_test() {
  [
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]
  |> map.from_list
  |> map.values
  |> expect.equal(_, [0, 1, 2])
}

pub fn take_test() {
  [
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]
  |> map.from_list
  |> map.take(_, ["a", "b", "d"])
  |> expect.equal(_, map.from_list([pair.Pair("a", 0), pair.Pair("b", 1)]))
}

pub fn drop_test() {
  [
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]
  |> map.from_list
  |> map.drop(_, ["a", "b", "d"])
  |> expect.equal(_, map.from_list([pair.Pair("c", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([
    pair.Pair("a", 2),
    pair.Pair("c", 4),
    pair.Pair("d", 3),
  ])
  let b = map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ])

  map.merge(a, b)
  |> expect.equal(_, map.from_list([
      pair.Pair("a", 0),
      pair.Pair("b", 1),
      pair.Pair("c", 2),
      pair.Pair("d", 3),
    ]))

  map.merge(b, a)
  |> expect.equal(_, map.from_list([
      pair.Pair("a", 2),
      pair.Pair("b", 1),
      pair.Pair("c", 4),
      pair.Pair("d", 3),
    ]))
}

pub fn delete_test() {
  [
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]
  |> map.from_list
  |> map.delete(_, "a")
  |> map.delete(_, "d")
  |> expect.equal(_, map.from_list([pair.Pair("b", 1), pair.Pair("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ])

  let inc_or_zero = fn(x) {
    case x {
    | Ok(i) -> i + 1
    | Error(_) -> 0
    }
  }

  dict
  |> map.update(_, "a", inc_or_zero)
  |> expect.equal(_, map.from_list([
    pair.Pair("a", 1),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
  ]))

  dict
  |> map.update(_, "b", inc_or_zero)
  |> expect.equal(_, map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 2),
    pair.Pair("c", 2),
  ]))

  dict
  |> map.update(_, "z", inc_or_zero)
  |> expect.equal(_, map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
    pair.Pair("z", 0),
  ]))
}

pub fn fold_test() {
  let dict = map.from_list([
    pair.Pair("a", 0),
    pair.Pair("b", 1),
    pair.Pair("c", 2),
    pair.Pair("d", 3),
  ])

  let add = fn(_, v, acc) {
    v + acc
  }

  dict
  |> map.fold(_, 0, add)
  |> expect.equal(_, 6)

  let concat = fn(k, _, acc) {
    string.append(acc, k)
  }

  dict
  |> map.fold(_, "", concat)
  |> expect.equal(_, "abcd")

  map.from_list([])
  |> map.fold(_, 0, add)
  |> expect.equal(_, 0)
}
