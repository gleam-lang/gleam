import gleam/expect
import gleam/triple

pub fn first_test() {
  triple.Triple(1, 2, 3)
  |> triple.first
  |> expect.equal(_, 1)

  triple.Triple([], "abc", 3)
  |> triple.first
  |> expect.equal(_, [])
}

pub fn second_test() {
  triple.Triple(1, 2, 3)
  |> triple.second
  |> expect.equal(_, 2)

  triple.Triple([], "abc", 3)
  |> triple.second
  |> expect.equal(_, "abc")
}

pub fn third_test() {
  triple.Triple(1, 2, 3)
  |> triple.third
  |> expect.equal(_, 3)

  triple.Triple([], "abc", 3)
  |> triple.third
  |> expect.equal(_, 3)
}
