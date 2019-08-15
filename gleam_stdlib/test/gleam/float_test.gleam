import gleam/expect
import gleam/float
import gleam/order

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

pub fn compare_test() {
  float:compare(0., 0.)
  |> expect:equal(_, order:Eq)

  float:compare(0.1, 0.1)
  |> expect:equal(_, order:Eq)

  float:compare(0., 0.1)
  |> expect:equal(_, order:Lt)

  float:compare(-2., -1.9)
  |> expect:equal(_, order:Lt)

  float:compare(2., 1.9)
  |> expect:equal(_, order:Gt)

  float:compare(-1.9, -2.)
  |> expect:equal(_, order:Gt)
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

pub fn round_test() {
  8.1
  |> float:round
  |> expect:equal(_, 8)

  8.4
  |> float:round
  |> expect:equal(_, 8)

  8.499
  |> float:round
  |> expect:equal(_, 8)

  8.5
  |> float:round
  |> expect:equal(_, 9)

  -8.1
  |> float:round
  |> expect:equal(_, -8)

  -7.5
  |> float:round
  |> expect:equal(_, -8)
}

pub fn truncate_test() {
  8.1
  |> float:truncate
  |> expect:equal(_, 8)

  8.4
  |> float:truncate
  |> expect:equal(_, 8)

  8.499
  |> float:truncate
  |> expect:equal(_, 8)

  8.5
  |> float:truncate
  |> expect:equal(_, 8)

  -8.1
  |> float:truncate
  |> expect:equal(_, -8)

  -7.5
  |> float:truncate
  |> expect:equal(_, -7)
}
