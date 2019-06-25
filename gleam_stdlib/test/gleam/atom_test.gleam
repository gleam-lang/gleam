import gleam/atom
import gleam/expect

pub fn from_string_test() {
  "ok"
  |> atom:from_string
  |> expect:is_ok

  "expect"
  |> atom:from_string
  |> expect:is_ok

  "this is not an atom we have seen before"
  |> atom:from_string
  // |> expect:equal(_, Error(AtomNotLoaded))
  |> expect:is_error
}

pub fn create_from_string_test() {
  "ok"
  |> atom:create_from_string
  |> Ok
  |> expect:equal(_, atom:from_string("ok"))

  "expect"
  |> atom:create_from_string
  |> Ok
  |> expect:equal(_, atom:from_string("expect"))

  "this is another atom we have not seen before"
  |> atom:create_from_string
  |> Ok
  |> expect:equal(_, atom:from_string("this is another atom we have not seen before"))
}

pub fn to_string_test() {
  "ok"
  |> atom:create_from_string
  |> atom:to_string
  |> expect:equal(_, "ok")

  "expect"
  |> atom:create_from_string
  |> atom:to_string
  |> expect:equal(_, "expect")
}
