import gleam/should
import gleam/float
import gleam/order

pub fn parse_test() {
  "1.23"
  |> float.parse
  |> should.equal(Ok(1.23))

  "+1.23"
  |> float.parse
  |> should.equal(Ok(1.23))

  "-1.23"
  |> float.parse
  |> should.equal(Ok(-1.23))

  "5.0"
  |> float.parse
  |> should.equal(Ok(5.0))

  "0.123456789"
  |> float.parse
  |> should.equal(Ok(0.123456789))

  ""
  |> float.parse
  |> should.equal(Error(Nil))

  "what"
  |> float.parse
  |> should.equal(Error(Nil))

  "1"
  |> float.parse
  |> should.equal(Error(Nil))
}

pub fn to_string_test() {
  123.0
  |> float.to_string
  |> should.equal("123.0")

  -8.1
  |> float.to_string
  |> should.equal("-8.1")
}

pub fn clamp_test() {
  float.clamp(1.4, min: 1.3, max: 1.5)
  |> should.equal(1.4)

  float.clamp(1.2, min: 1.3, max: 1.5)
  |> should.equal(1.3)

  float.clamp(1.6, min: 1.3, max: 1.5)
  |> should.equal(1.5)
}

pub fn compare_test() {
  float.compare(0., 0.)
  |> should.equal(order.Eq)

  float.compare(0.1, 0.1)
  |> should.equal(order.Eq)

  float.compare(0., 0.1)
  |> should.equal(order.Lt)

  float.compare(-2., -1.9)
  |> should.equal(order.Lt)

  float.compare(2., 1.9)
  |> should.equal(order.Gt)

  float.compare(-1.9, -2.)
  |> should.equal(order.Gt)
}

pub fn ceiling_test() {
  8.1
  |> float.ceiling
  |> should.equal(9.0)

  -8.1
  |> float.ceiling
  |> should.equal(-8.0)

  -8.0
  |> float.ceiling
  |> should.equal(-8.0)
}

pub fn floor_test() {
  8.1
  |> float.floor
  |> should.equal(8.0)

  -8.1
  |> float.floor
  |> should.equal(-9.0)

  -8.0
  |> float.floor
  |> should.equal(-8.0)
}

pub fn round_test() {
  8.1
  |> float.round
  |> should.equal(8)

  8.4
  |> float.round
  |> should.equal(8)

  8.499
  |> float.round
  |> should.equal(8)

  8.5
  |> float.round
  |> should.equal(9)

  -8.1
  |> float.round
  |> should.equal(-8)

  -7.5
  |> float.round
  |> should.equal(-8)
}

pub fn truncate_test() {
  8.1
  |> float.truncate
  |> should.equal(8)

  8.4
  |> float.truncate
  |> should.equal(8)

  8.499
  |> float.truncate
  |> should.equal(8)

  8.5
  |> float.truncate
  |> should.equal(8)

  -8.1
  |> float.truncate
  |> should.equal(-8)

  -7.5
  |> float.truncate
  |> should.equal(-7)
}

pub fn min_test() {
  float.min(0., 0.)
  |> should.equal(0.)

  float.min(0.3, 1.5)
  |> should.equal(0.3)

  float.min(1., 0.)
  |> should.equal(0.)

  float.min(-1.7, 2.5)
  |> should.equal(-1.7)

  float.min(-2.2, -2.2)
  |> should.equal(-2.2)

  float.min(-1., -1.)
  |> should.equal(-1.)

  float.min(-1.1, -1.)
  |> should.equal(-1.1)
}

pub fn max_test() {
  float.max(0., 0.)
  |> should.equal(0.)

  float.max(0.3, 1.5)
  |> should.equal(1.5)

  float.max(1., 0.)
  |> should.equal(1.)

  float.max(-1.7, 2.5)
  |> should.equal(2.5)

  float.max(-2.2, -2.2)
  |> should.equal(-2.2)

  float.max(-1., -1.)
  |> should.equal(-1.)

  float.max(-1.1, -1.)
  |> should.equal(-1.)
}

pub fn absolute_value_test() {
  float.absolute_value(-1.0)
  |> should.equal(1.0)

  float.absolute_value(-20.6)
  |> should.equal(20.6)

  float.absolute_value(0.0)
  |> should.equal(0.0)

  float.absolute_value(1.0)
  |> should.equal(1.0)

  float.absolute_value(25.2)
  |> should.equal(25.2)
}

pub fn power_test() {
  float.power(2.0, 2.0)
  |> should.equal(4.0)

  float.power(-5.0, 3.0)
  |> should.equal(-125.0)

  float.power(10.5, 0.0)
  |> should.equal(1.0)

  float.power(16.0, 0.5)
  |> should.equal(4.0)

  float.power(2.0, -1.0)
  |> should.equal(0.5)
}

pub fn square_root_test() {
  float.square_root(4.0)
  |> should.equal(Ok(2.0))

  float.square_root(16.0)
  |> should.equal(Ok(4.0))

  float.square_root(0.0)
  |> should.equal(Ok(0.0))

  float.square_root(-4.0)
  |> should.equal(Error(Nil))
}

pub fn negate_test() {
  float.negate(-1.)
  |> should.equal(1.)

  float.negate(2.)
  |> should.equal(-2.)

  float.negate(0.)
  |> should.equal(0.)
}

pub fn sum_test() {
  float.sum([])
  |> should.equal(0.0)

  float.sum([1.0, 2.2, 3.3])
  |> should.equal(6.5)
}

pub fn product_test() {
  float.product([])
  |> should.equal(0.)

  float.product([4.])
  |> should.equal(4.)

  float.product([2.5, 3.2, 4.2])
  |> should.equal(33.6)
}
