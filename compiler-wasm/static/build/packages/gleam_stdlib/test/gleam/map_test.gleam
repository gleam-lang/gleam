import gleam/string
import gleam/should
import gleam/map
import gleam/option.{None, Some}

pub fn from_list_test() {
  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.size
  |> should.equal(2)

  [#(1, 0), #(1, 1)]
  |> map.from_list
  |> should.equal(map.from_list([#(1, 1)]))
}

pub fn has_key_test() {
  []
  |> map.from_list
  |> map.has_key(1)
  |> should.be_false

  [#(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.has_key(1)
  |> should.be_true

  [#(4, 0), #(1, 0)]
  |> map.from_list
  |> map.has_key(0)
  |> should.be_false
}

pub fn new_test() {
  map.new()
  |> map.size
  |> should.equal(0)

  map.new()
  |> map.to_list
  |> should.equal([])
}

pub fn get_test() {
  let proplist = [#(4, 0), #(1, 1)]
  let m = map.from_list(proplist)

  m
  |> map.get(4)
  |> should.equal(Ok(0))

  m
  |> map.get(1)
  |> should.equal(Ok(1))

  m
  |> map.get(2)
  |> should.equal(Error(Nil))
}

pub fn insert_test() {
  map.new()
  |> map.insert("a", 0)
  |> map.insert("b", 1)
  |> map.insert("c", 2)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2)]))
}

pub fn map_values_test() {
  [#(1, 0), #(2, 1), #(3, 2)]
  |> map.from_list
  |> map.map_values(fn(k, v) { k + v })
  |> should.equal(map.from_list([#(1, 1), #(2, 3), #(3, 5)]))
}

pub fn keys_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.keys
  |> should.equal(["a", "b", "c"])
}

pub fn values_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.values
  |> should.equal([0, 1, 2])
}

pub fn take_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.take(["a", "b", "d"])
  |> should.equal(map.from_list([#("a", 0), #("b", 1)]))
}

pub fn drop_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.drop(["a", "b", "d"])
  |> should.equal(map.from_list([#("c", 2)]))
}

pub fn merge_same_key_test() {
  let a = map.from_list([#("a", 2)])
  let b = map.from_list([#("a", 0)])

  map.merge(a, b)
  |> should.equal(map.from_list([#("a", 0)]))

  map.merge(b, a)
  |> should.equal(map.from_list([#("a", 2)]))
}

pub fn merge_test() {
  let a = map.from_list([#("a", 2), #("c", 4), #("d", 3)])
  let b = map.from_list([#("a", 0), #("b", 1), #("c", 2)])

  map.merge(a, b)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)]))

  map.merge(b, a)
  |> should.equal(map.from_list([#("a", 2), #("b", 1), #("c", 4), #("d", 3)]))
}

pub fn delete_test() {
  [#("a", 0), #("b", 1), #("c", 2)]
  |> map.from_list
  |> map.delete("a")
  |> map.delete("d")
  |> should.equal(map.from_list([#("b", 1), #("c", 2)]))
}

pub fn update_test() {
  let dict = map.from_list([#("a", 0), #("b", 1), #("c", 2)])

  let inc_or_zero = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 0
    }
  }

  dict
  |> map.update("a", inc_or_zero)
  |> should.equal(map.from_list([#("a", 1), #("b", 1), #("c", 2)]))

  dict
  |> map.update("b", inc_or_zero)
  |> should.equal(map.from_list([#("a", 0), #("b", 2), #("c", 2)]))

  dict
  |> map.update("z", inc_or_zero)
  |> should.equal(map.from_list([#("a", 0), #("b", 1), #("c", 2), #("z", 0)]))
}

pub fn fold_test() {
  let dict = map.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])

  let add = fn(acc, _, v) { v + acc }

  dict
  |> map.fold(0, add)
  |> should.equal(6)

  let concat = fn(acc, k, _) { string.append(acc, k) }

  dict
  |> map.fold("", concat)
  |> should.equal("abcd")

  map.from_list([])
  |> map.fold(0, add)
  |> should.equal(0)
}
