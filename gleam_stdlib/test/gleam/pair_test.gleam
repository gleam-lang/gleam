import gleam/expect
import gleam/pair

pub fn first_test() {
  pair.Pair(1, 2)
  |> pair.first
  |> expect.equal(_, 1)
}

pub fn second_test() {
  pair.Pair(1, 2)
  |> pair.second
  |> expect.equal(_, 2)
}

pub fn swap_test() {
  pair.Pair(1, "2")
  |> pair.swap
  |> expect.equal(_, pair.Pair("2", 1))
}

// pub fn fetch_test() {
//   let proplist = [pair.Pair(0, "1"), pair.Pair(1, "2")]

//   proplist
//   |> pair.fetch(_, 0)
//   |> expect.equal(_, Ok("1"))

//   proplist
//   |> pair.fetch(_, 1)
//   |> expect.equal(_, Ok("2"))

//   proplist
//   |> pair.fetch(_, 2)
//   |> expect.is_error
// }
