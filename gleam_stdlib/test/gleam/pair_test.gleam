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

pub fn map_first_test() {
    let inc = fn(a) {
        a + 1
    }
    pair.map_first(pair.Pair(1, 2), inc)
    |> expect.equal(_, pair.Pair(2, 2))

    pair.map_first(pair.Pair(8,2), inc)
    |> expect.equal(_, pair.Pair(9, 2))

    pair.map_first(pair.Pair(0,-2), inc)
    |> expect.equal(_, pair.Pair(1, -2))

    pair.map_first(pair.Pair(-10, 20), inc)
    |> expect.equal(_, pair.Pair(-9, 20))
}

pub fn map_second_test() {
    let dec = fn(a) {
        a - 1
    }
    pair.map_second(pair.Pair(1, 2), dec)
    |> expect.equal(_, pair.Pair(1, 1))

    pair.map_second(pair.Pair(8,2), dec)
    |> expect.equal(_, pair.Pair(8, 1))

    pair.map_second(pair.Pair(0,-2), dec)
    |> expect.equal(_, pair.Pair(0, -3))

    pair.map_second(pair.Pair(-10, 20), dec)
    |> expect.equal(_, pair.Pair(-10, 19))
}
