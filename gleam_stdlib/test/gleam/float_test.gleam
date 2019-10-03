import gleam/expect
import gleam/float
import gleam/order

pub fn parse_test() {
  "1.23"
  |> float.parse
  |> expect.equal(_, Ok(1.23))

  "5.0"
  |> float.parse
  |> expect.equal(_, Ok(5.0))

  "0.123456789"
  |> float.parse
  |> expect.equal(_, Ok(0.123456789))

  ""
  |> float.parse
  |> expect.equal(_, Error(Nil))

  "what"
  |> float.parse
  |> expect.equal(_, Error(Nil))

  "1"
  |> float.parse
  |> expect.equal(_, Error(Nil))
}

pub fn to_string_test() {
  123.0
  |> float.to_string
  |> expect.equal(_, "123.0")

  -8.1
  |> float.to_string
  |> expect.equal(_, "-8.1")
}

pub fn compare_test() {
  float.compare(0., 0.)
  |> expect.equal(_, order.Eq)

  float.compare(0.1, 0.1)
  |> expect.equal(_, order.Eq)

  float.compare(0., 0.1)
  |> expect.equal(_, order.Lt)

  float.compare(-2., -1.9)
  |> expect.equal(_, order.Lt)

  float.compare(2., 1.9)
  |> expect.equal(_, order.Gt)

  float.compare(-1.9, -2.)
  |> expect.equal(_, order.Gt)
}

pub fn ceiling_test() {
  8.1
  |> float.ceiling
  |> expect.equal(_, 9.0)

  -8.1
  |> float.ceiling
  |> expect.equal(_, -8.0)

  -8.0
  |> float.ceiling
  |> expect.equal(_, -8.0)
}

pub fn floor_test() {
  8.1
  |> float.floor
  |> expect.equal(_, 8.0)

  -8.1
  |> float.floor
  |> expect.equal(_, -9.0)

  -8.0
  |> float.floor
  |> expect.equal(_, -8.0)
}

pub fn round_test() {
  8.1
  |> float.round
  |> expect.equal(_, 8)

  8.4
  |> float.round
  |> expect.equal(_, 8)

  8.499
  |> float.round
  |> expect.equal(_, 8)

  8.5
  |> float.round
  |> expect.equal(_, 9)

  -8.1
  |> float.round
  |> expect.equal(_, -8)

  -7.5
  |> float.round
  |> expect.equal(_, -8)
}

pub fn truncate_test() {
  8.1
  |> float.truncate
  |> expect.equal(_, 8)

  8.4
  |> float.truncate
  |> expect.equal(_, 8)

  8.499
  |> float.truncate
  |> expect.equal(_, 8)

  8.5
  |> float.truncate
  |> expect.equal(_, 8)

  -8.1
  |> float.truncate
  |> expect.equal(_, -8)

  -7.5
  |> float.truncate
  |> expect.equal(_, -7)
}

pub fn min_test() {
  float.min(0., 0.)
  |> expect.equal(_, 0.)

  float.min(0.3, 1.5)
  |> expect.equal(_, 0.3)

  float.min(1., 0.)
  |> expect.equal(_, 0.)

  float.min(-1.7, 2.5)
  |> expect.equal(_, -1.7)

  float.min(-2.2, -2.2)
  |> expect.equal(_, -2.2)

  float.min(-1., -1.)
  |> expect.equal(_, -1.)

  float.min(-1.1, -1.)
  |> expect.equal(_, -1.1)
}

pub fn max_test() {
  float.max(0., 0.)
  |> expect.equal(_, 0.)

  float.max(0.3, 1.5)
  |> expect.equal(_, 1.5)

  float.max(1., 0.)
  |> expect.equal(_, 1.)

  float.max(-1.7, 2.5)
  |> expect.equal(_, 2.5)

  float.max(-2.2, -2.2)
  |> expect.equal(_, -2.2)

  float.max(-1., -1.)
  |> expect.equal(_, -1.)

  float.max(-1.1, -1.)
  |> expect.equal(_, -1.)
}
