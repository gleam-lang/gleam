import gleam/string
import gleam/expect
import gleam/map
import gleam/pair.{Pair}

pub fn from_list_test() {
  [
    Pair(4, 0),
    Pair(1, 0),
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
      Pair(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> expect.true

  [
      Pair(4, 0),
      Pair(1, 0),
  ]
  |> map.from_list
  |> map.has_key(_, 1)
  |> expect.true

  [
    Pair(4, 0),
    Pair(1, 0),
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

pub fn get_test() {
  let proplist = [
    Pair(4, 0),
    Pair(1, 1),
  ]
  let m = map.from_list(proplist)

  m
  |> map.get(_, 4)
  |> expect.equal(_, Ok(0))

  m
  |> map.get(_, 1)
  |> expect.equal(_, Ok(1))

  m
  |> map.get(_, 2)
  |> expect.equal(_, Error(Nil))
}

pub fn insert_test() {
  map.new()
  |> map.insert(_, "a", 0)
  |> map.insert(_, "b", 1)
  |> map.insert(_, "c", 2)
  |> expect.equal(_, map.from_list([
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]))
}

pub fn map_values_test() {
  [
    Pair(1, 0),
    Pair(2, 1),
    Pair(3, 2),
  ]
  |> map.from_list
  |> map.map_values(_, fn(k, v) { k + v })
  |> expect.equal(_, map.from_list([
    Pair(1, 1),
    Pair(2, 3),
    Pair(3, 5),
  ]))
}

pub fn keys_test() {
  [
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]
  |> map.from_list
  |> map.keys
  |> expect.equal(_, ["a", "b", "c"])
}

pub fn values_test() {
  [
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]
  |> map.from_list
  |> map.values
  |> expect.equal(_, [0, 1, 2])
}

pub fn take_test() {
  [
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]
  |> map.from_list
  |> map.take(_, ["a", "b", "d"])
  |> expect.equal(_, map.from_list([Pair("a", 0), Pair("b", 1)]))
}

pub fn drop_test() {
  [
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]
  |> map.from_list
  |> map.drop(_, ["a", "b", "d"])
  |> expect.equal(_, map.from_list([Pair("c", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([
    Pair("a", 2),
    Pair("c", 4),
    Pair("d", 3),
  ])
  let b = map.from_list([
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ])

  map.merge(a, b)
  |> expect.equal(_, map.from_list([
      Pair("a", 0),
      Pair("b", 1),
      Pair("c", 2),
      Pair("d", 3),
    ]))

  map.merge(b, a)
  |> expect.equal(_, map.from_list([
      Pair("a", 2),
      Pair("b", 1),
      Pair("c", 4),
      Pair("d", 3),
    ]))
}

pub fn delete_test() {
  [
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
  ]
  |> map.from_list
  |> map.delete(_, "a")
  |> map.delete(_, "d")
  |> expect.equal(_, map.from_list([Pair("b", 1), Pair("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
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
    Pair("a", 1),
    Pair("b", 1),
    Pair("c", 2),
  ]))

  dict
  |> map.update(_, "b", inc_or_zero)
  |> expect.equal(_, map.from_list([
    Pair("a", 0),
    Pair("b", 2),
    Pair("c", 2),
  ]))

  dict
  |> map.update(_, "z", inc_or_zero)
  |> expect.equal(_, map.from_list([
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
    Pair("z", 0),
  ]))
}

pub fn fold_test() {
  let dict = map.from_list([
    Pair("a", 0),
    Pair("b", 1),
    Pair("c", 2),
    Pair("d", 3),
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
