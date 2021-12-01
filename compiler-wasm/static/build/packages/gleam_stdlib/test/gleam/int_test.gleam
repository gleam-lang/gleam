import gleam/should
import gleam/int
import gleam/order

pub fn absolute_value_test() {
  123
  |> int.absolute_value
  |> should.equal(123)

  -123
  |> int.absolute_value
  |> should.equal(123)
}

pub fn clamp_test() {
  int.clamp(40, min: 30, max: 50)
  |> should.equal(40)

  int.clamp(20, min: 30, max: 50)
  |> should.equal(30)

  int.clamp(60, min: 30, max: 50)
  |> should.equal(50)

  // If the bounds are reversed we return the min
  int.clamp(100, min: 50, max: 30)
  |> should.equal(50)
}

pub fn to_string_test() {
  123
  |> int.to_string
  |> should.equal("123")

  -123
  |> int.to_string
  |> should.equal("-123")

  123
  |> int.to_string
  |> should.equal("123")
}

pub fn parse_test() {
  "123"
  |> int.parse
  |> should.equal(Ok(123))

  "-123"
  |> int.parse
  |> should.equal(Ok(-123))

  "0123"
  |> int.parse
  |> should.equal(Ok(123))

  ""
  |> int.parse
  |> should.equal(Error(Nil))

  "what"
  |> int.parse
  |> should.equal(Error(Nil))

  "1.23"
  |> int.parse
  |> should.equal(Error(Nil))
}

pub fn to_base_string_test() {
  100
  |> int.to_base_string(16)
  |> should.equal(Ok("64"))

  -100
  |> int.to_base_string(16)
  |> should.equal(Ok("-64"))

  100
  |> int.to_base_string(1)
  |> should.equal(Error(int.InvalidBase))

  100
  |> int.to_base_string(37)
  |> should.equal(Error(int.InvalidBase))
}

pub fn to_base2_test() {
  100
  |> int.to_base2()
  |> should.equal("1100100")

  -100
  |> int.to_base2()
  |> should.equal("-1100100")
}

pub fn to_base8_test() {
  100
  |> int.to_base8()
  |> should.equal("144")

  -100
  |> int.to_base8()
  |> should.equal("-144")
}

pub fn to_base16_test() {
  100
  |> int.to_base16()
  |> should.equal("64")

  -100
  |> int.to_base16()
  |> should.equal("-64")

  43981
  |> int.to_base16()
  |> should.equal("ABCD")

  -43981
  |> int.to_base16()
  |> should.equal("-ABCD")
}

pub fn to_base36_test() {
  100
  |> int.to_base36()
  |> should.equal("2S")

  -100
  |> int.to_base36()
  |> should.equal("-2S")
}

pub fn to_float_test() {
  int.to_float(1)
  |> should.equal(1.)

  int.to_float(5)
  |> should.equal(5.)

  int.to_float(0)
  |> should.equal(0.)

  int.to_float(-5)
  |> should.equal(-5.)
}

pub fn compare_test() {
  int.compare(0, 0)
  |> should.equal(order.Eq)

  int.compare(1, 1)
  |> should.equal(order.Eq)

  int.compare(0, 1)
  |> should.equal(order.Lt)

  int.compare(-2, -1)
  |> should.equal(order.Lt)

  int.compare(2, 1)
  |> should.equal(order.Gt)

  int.compare(-1, -2)
  |> should.equal(order.Gt)
}

pub fn min_test() {
  int.min(0, 0)
  |> should.equal(0)

  int.min(0, 1)
  |> should.equal(0)

  int.min(1, 0)
  |> should.equal(0)

  int.min(-1, 2)
  |> should.equal(-1)

  int.min(2, -2)
  |> should.equal(-2)

  int.min(-1, -1)
  |> should.equal(-1)
}

pub fn max_test() {
  int.max(0, 0)
  |> should.equal(0)

  int.max(0, 1)
  |> should.equal(1)

  int.max(1, 0)
  |> should.equal(1)

  int.max(-1, 2)
  |> should.equal(2)

  int.max(2, -2)
  |> should.equal(2)

  int.max(-1, -1)
  |> should.equal(-1)
}

pub fn is_even_test() {
  int.is_even(0)
  |> should.be_true

  int.is_even(2)
  |> should.be_true

  int.is_even(-2)
  |> should.be_true

  int.is_even(10006)
  |> should.be_true

  int.is_even(1)
  |> should.be_false

  int.is_even(-3)
  |> should.be_false

  int.is_even(10005)
  |> should.be_false
}

pub fn is_odd_test() {
  int.is_odd(0)
  |> should.be_false

  int.is_odd(2)
  |> should.be_false

  int.is_odd(-2)
  |> should.be_false

  int.is_odd(10006)
  |> should.be_false

  int.is_odd(1)
  |> should.be_true

  int.is_odd(-3)
  |> should.be_true

  int.is_odd(10005)
  |> should.be_true
}

pub fn negate_test() {
  int.negate(-1)
  |> should.equal(1)

  int.negate(2)
  |> should.equal(-2)

  int.negate(0)
  |> should.equal(0)
}

pub fn sum_test() {
  int.sum([])
  |> should.equal(0)

  int.sum([1, 2, 3])
  |> should.equal(6)
}

pub fn product_test() {
  int.product([])
  |> should.equal(0)

  int.product([4])
  |> should.equal(4)

  int.product([1, 2, 3])
  |> should.equal(6)
}
