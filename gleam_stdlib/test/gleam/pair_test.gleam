import gleam/expect
import gleam/pair.{Pair}

pub fn first_test() {
  Pair(1, 2)
  |> pair.first
  |> expect.equal(_, 1)

  Pair("abc", [])
  |> pair.first
  |> expect.equal(_, "abc")
}

pub fn second_test() {
  Pair(1, 2)
  |> pair.second
  |> expect.equal(_, 2)

  Pair("abc", [])
  |> pair.second
  |> expect.equal(_,[])
}

pub fn swap_test() {
  Pair(1, "2")
  |> pair.swap
  |> expect.equal(_, Pair("2", 1))
}

pub fn map_first_test() {
    let inc = fn(a) {
        a + 1
    }
    pair.map_first(Pair(1, 2), inc)
    |> expect.equal(_, Pair(2, 2))

    pair.map_first(Pair(8,2), inc)
    |> expect.equal(_, Pair(9, 2))

    pair.map_first(Pair(0,-2), inc)
    |> expect.equal(_, Pair(1, -2))

    pair.map_first(Pair(-10, 20), inc)
    |> expect.equal(_, Pair(-9, 20))
}

pub fn map_second_test() {
    let dec = fn(a) {
        a - 1
    }
    pair.map_second(Pair(1, 2), dec)
    |> expect.equal(_, Pair(1, 1))

    pair.map_second(Pair(8,2), dec)
    |> expect.equal(_, Pair(8, 1))

    pair.map_second(Pair(0,-2), dec)
    |> expect.equal(_, Pair(0, -3))

    pair.map_second(Pair(-10, 20), dec)
    |> expect.equal(_, Pair(-10, 19))
}
