import expect
import float

pub fn parse_test() {
  "1.23"
  |> float:parse
  |> expect:equal(_, Ok(1.23))

  "5.0"
  |> float:parse
  |> expect:equal(_, Ok(5.0))

  "0.123456789"
  |> float:parse
  |> expect:equal(_, Ok(0.123456789))

  ""
  |> float:parse
  |> expect:is_error

  "what"
  |> float:parse
  |> expect:is_error

  "1"
  |> float:parse
  |> expect:is_error
}

pub fn to_string_test() {
  123.0
  |> float:to_string
  |> expect:equal(_, "123.0")

  -8.1
  |> float:to_string
  |> expect:equal(_, "-8.1")
}

pub fn ceiling_test() {
  8.1
  |> float:ceiling
  |> expect:equal(_, 9.0)

  -8.1
  |> float:ceiling
  |> expect:equal(_, -8.0)

  -8.0
  |> float:ceiling
  |> expect:equal(_, -8.0)
}

pub fn floor_test() {
  8.1
  |> float:floor
  |> expect:equal(_, 8.0)

  -8.1
  |> float:floor
  |> expect:equal(_, -9.0)

  -8.0
  |> float:floor
  |> expect:equal(_, -8.0)
}
