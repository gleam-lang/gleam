import gleam/expect
import gleam/triple.{Triple}

pub fn first_test() {
  Triple(1, 2, 3)
  |> triple.first
  |> expect.equal(_, 1)

  Triple([], "abc", 3)
  |> triple.first
  |> expect.equal(_, [])
}

pub fn second_test() {
  Triple(1, 2, 3)
  |> triple.second
  |> expect.equal(_, 2)

  Triple([], "abc", 3)
  |> triple.second
  |> expect.equal(_, "abc")
}

pub fn third_test() {
  Triple(1, 2, 3)
  |> triple.third
  |> expect.equal(_, 3)

  Triple([], "abc", 3)
  |> triple.third
  |> expect.equal(_, 3)
}
