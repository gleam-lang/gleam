import gleam/expect
import gleam/tuple

pub fn new_test() {
  tuple:new(1, 2)
  |> expect:equal(_, {1, 2})

  tuple:new(2, "3")
  |> expect:equal(_, {2, "3"})
}

pub fn first_test() {
  {1, 2}
  |> tuple:first
  |> expect:equal(_, 1)
}

pub fn second_test() {
  {1, 2}
  |> tuple:second
  |> expect:equal(_, 2)
}

pub fn swap_test() {
  {1, "2"}
  |> tuple:swap
  |> expect:equal(_, {"2", 1})
}

pub fn fetch_test() {
  let proplist = [{0, "1"}, {1, "2"}]

  proplist
  |> tuple:fetch(_, 0)
  |> expect:equal(_, Ok("1"))

  proplist
  |> tuple:fetch(_, 1)
  |> expect:equal(_, Ok("2"))

  proplist
  |> tuple:fetch(_, 2)
  |> expect:is_error
}
