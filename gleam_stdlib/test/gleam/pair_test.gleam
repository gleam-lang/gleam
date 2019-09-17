import gleam/expect
import gleam/pair

pub fn first_test() {
  pair.Pair(1, 2)
  |> pair.first
  |> expect.equal(_, 1)

  pair.Pair("abc", [])
  |> pair.first
  |> expect.equal(_, "abc")
}

pub fn second_test() {
  pair.Pair(1, 2)
  |> pair.second
  |> expect.equal(_, 2)

  pair.Pair("abc", [])
  |> pair.second
  |> expect.equal(_,[])
}

pub fn swap_test() {
  pair.Pair(1, "2")
  |> pair.swap
  |> expect.equal(_, pair.Pair("2", 1))
}
