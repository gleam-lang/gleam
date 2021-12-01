import gleam/should
import gleam/pair

pub fn first_test() {
  #(1, 2)
  |> pair.first
  |> should.equal(1)

  #("abc", [])
  |> pair.first
  |> should.equal("abc")
}

pub fn second_test() {
  #(1, 2)
  |> pair.second
  |> should.equal(2)

  #("abc", [])
  |> pair.second
  |> should.equal([])
}

pub fn swap_test() {
  #(1, "2")
  |> pair.swap
  |> should.equal(#("2", 1))
}

pub fn map_first_test() {
  let inc = fn(a) { a + 1 }
  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  pair.map_first(#(8, 2), inc)
  |> should.equal(#(9, 2))

  pair.map_first(#(0, -2), inc)
  |> should.equal(#(1, -2))

  pair.map_first(#(-10, 20), inc)
  |> should.equal(#(-9, 20))
}

pub fn map_second_test() {
  let dec = fn(a) { a - 1 }
  pair.map_second(#(1, 2), dec)
  |> should.equal(#(1, 1))

  pair.map_second(#(8, 2), dec)
  |> should.equal(#(8, 1))

  pair.map_second(#(0, -2), dec)
  |> should.equal(#(0, -3))

  pair.map_second(#(-10, 20), dec)
  |> should.equal(#(-10, 19))
}
