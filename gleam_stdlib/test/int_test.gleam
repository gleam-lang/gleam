import expect
import int

pub fn to_string() {
  123
  |> int:to_string
  |> expect:equal(_, "123")

  -123
  |> int:to_string
  |> expect:equal(_, "-123")

  0123
  |> int:to_string
  |> expect:equal(_, "123")
}

pub fn parse() {
  "123"
  |> int:parse
  |> expect:equal(_, Ok(123))

  "-123"
  |> int:parse
  |> expect:equal(_, Ok(-123))

  "0123"
  |> int:parse
  |> expect:equal(_, Ok(123))

  ""
  |> int:parse
  |> expect:is_error

  "what"
  |> int:parse
  |> expect:is_error

  "1.23"
  |> int:parse
  |> expect:is_error
}

pub fn to_base_string() {
  100
  |> int:to_base_string(_, 16)
  |> expect:equal(_, "64")

  -100
  |> int:to_base_string(_, 16)
  |> expect:equal(_, "-64")
}
