import gleam/any
import gleam/atom
import gleam/list
import gleam/pair
import gleam/expect
import gleam/result
import gleam/map

pub fn string_test() {
  ""
  |> any.from
  |> any.string
  |> expect.equal(_, Ok(""))

  "Hello"
  |> any.from
  |> any.string
  |> expect.equal(_, Ok("Hello"))

  1
  |> any.from
  |> any.string
  |> expect.equal(_, Error("Expected a String, got `1`"))

  []
  |> any.from
  |> any.string
  |> expect.equal(_, Error("Expected a String, got `[]`"))
}

pub fn int_test() {
  1
  |> any.from
  |> any.int
  |> expect.equal(_, Ok(1))

  2
  |> any.from
  |> any.int
  |> expect.equal(_, Ok(2))

  1.0
  |> any.from
  |> any.int
  |> expect.equal(_, Error("Expected an Int, got `1.0`"))

  []
  |> any.from
  |> any.int
  |> expect.equal(_, Error("Expected an Int, got `[]`"))
}

pub fn float_test() {
  1.0
  |> any.from
  |> any.float
  |> expect.equal(_, Ok(1.0))

  2.2
  |> any.from
  |> any.float
  |> expect.equal(_, Ok(2.2))

  1
  |> any.from
  |> any.float
  |> expect.equal(_, Error("Expected a Float, got `1`"))

  []
  |> any.from
  |> any.float
  |> expect.equal(_, Error("Expected a Float, got `[]`"))
}

pub fn thunk_test() {
  fn() { 1 }
  |> any.from
  |> any.thunk
  |> expect.is_ok

  fn() { 1 }
  |> any.from
  |> any.thunk
  |> result.map(_, fn(f) { f() })
  |> expect.equal(_, Ok(any.from(1)))

  fn(x) { x }
  |> any.from
  |> any.thunk
  |> expect.is_error

  1
  |> any.from
  |> any.thunk
  |> expect.is_error

  []
  |> any.from
  |> any.thunk
  |> expect.is_error
}

pub fn bool_test() {
  True
  |> any.from
  |> any.bool
  |> expect.equal(_, Ok(True))

  False
  |> any.from
  |> any.bool
  |> expect.equal(_, Ok(False))

  1
  |> any.from
  |> any.bool
  |> expect.equal(_, Error("Expected a Bool, got `1`"))

  []
  |> any.from
  |> any.bool
  |> expect.equal(_, Error("Expected a Bool, got `[]`"))
}

pub fn atom_test() {
  ""
    |> atom.create_from_string
    |> any.from
    |> any.atom
    |> expect.equal(_, Ok(atom.create_from_string("")))

  "ok"
    |> atom.create_from_string
    |> any.from
    |> any.atom
    |> expect.equal(_, Ok(atom.create_from_string("ok")))

  1
    |> any.from
    |> any.atom
    |> expect.is_error

  []
    |> any.from
    |> any.atom
    |> expect.is_error
}

pub fn list_test() {
  []
  |> any.from
  |> any.list(_, any.string)
  |> expect.equal(_, Ok([]))

  []
  |> any.from
  |> any.list(_, any.int)
  |> expect.equal(_, Ok([]))

  [1, 2, 3]
  |> any.from
  |> any.list(_, any.int)
  |> expect.equal(_, Ok([1, 2, 3]))

  [[1], [2], [3]]
  |> any.from
  |> any.list(_, any.list(_, any.int))
  |> expect.equal(_, Ok([[1], [2], [3]]))

  1
  |> any.from
  |> any.list(_, any.string)
  |> expect.is_error

  1.0
  |> any.from
  |> any.list(_, any.int)
  |> expect.is_error

  [""]
  |> any.from
  |> any.list(_, any.int)
  |> expect.is_error

  [any.from(1), any.from("not an int")]
  |> any.from
  |> any.list(_, any.int)
  |> expect.is_error
}

pub fn pair_test() {
  pair.Pair(1, [])
  |> any.from
  |> any.pair
  |> expect.equal(_, Ok(pair.Pair(any.from(1), any.from([]))))

  pair.Pair("ok", "ok")
  |> any.from
  |> any.pair
  |> expect.equal(_, Ok(pair.Pair(any.from("ok"), any.from("ok"))))

  pair.Pair(1, 2.0)
  |> any.from
  |> any.pair
  |> result.then(_, fn(x) {
    x
    |> pair.first
    |> any.int
    |> result.map(_, fn(f) { pair.Pair(f, pair.second(x)) })
  })
  |> result.then(_, fn(x) {
    x
    |> pair.second
    |> any.float
    |> result.map(_, fn(f) { pair.Pair(pair.first(x), f) })
  })
  |> expect.equal(_, Ok(pair.Pair(1, 2.0)))
}

pub fn field_test() {
  let Ok(ok_atom) = atom.from_string("ok")
  let Ok(error_atom) = atom.from_string("error")

  map.new()
  |> map.put(_, ok_atom, 1)
  |> any.from
  |> any.field(_, ok_atom)
  |> expect.equal(_, Ok(any.from(1)))

  map.new()
  |> map.put(_, ok_atom, 3)
  |> map.put(_, error_atom, 1)
  |> any.from
  |> any.field(_, ok_atom)
  |> expect.equal(_, Ok(any.from(3)))

  map.new()
  |> any.from
  |> any.field(_, ok_atom)
  |> expect.is_error

  1
  |> any.from
  |> any.field(_, ok_atom)
  |> expect.is_error

  []
  |> any.from
  |> any.field(_, [])
  |> expect.is_error
}
