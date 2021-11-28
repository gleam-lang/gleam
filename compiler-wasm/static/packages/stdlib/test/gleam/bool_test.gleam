import gleam/bool
import gleam/order
import gleam/should

pub fn negate_test() {
  bool.negate(True)
  |> should.be_false

  bool.negate(False)
  |> should.be_true
}

pub fn nor_test() {
  bool.nor(False, False)
  |> should.be_true

  bool.nor(False, True)
  |> should.be_false

  bool.nor(True, False)
  |> should.be_false

  bool.nor(True, True)
  |> should.be_false
}

pub fn nand_test() {
  bool.nand(False, False)
  |> should.be_true

  bool.nand(False, True)
  |> should.be_true

  bool.nand(True, False)
  |> should.be_true

  bool.nand(True, True)
  |> should.be_false
}

pub fn exclusive_or_test() {
  bool.exclusive_or(True, True)
  |> should.be_false

  bool.exclusive_or(False, False)
  |> should.be_false

  bool.exclusive_or(True, False)
  |> should.be_true

  bool.exclusive_or(False, True)
  |> should.be_true
}

pub fn exclusive_nor_test() {
  bool.exclusive_nor(False, False)
  |> should.be_true

  bool.exclusive_nor(False, True)
  |> should.be_false

  bool.exclusive_nor(True, False)
  |> should.be_false

  bool.exclusive_nor(True, True)
  |> should.be_true
}

pub fn compare_test() {
  bool.compare(True, True)
  |> should.equal(order.Eq)

  bool.compare(True, False)
  |> should.equal(order.Gt)

  bool.compare(False, False)
  |> should.equal(order.Eq)

  bool.compare(False, True)
  |> should.equal(order.Lt)
}

pub fn max_test() {
  bool.max(True, True)
  |> should.equal(True)

  bool.max(True, False)
  |> should.equal(True)

  bool.max(False, False)
  |> should.equal(False)

  bool.max(False, True)
  |> should.equal(True)
}

pub fn min_test() {
  bool.min(True, True)
  |> should.equal(True)

  bool.min(True, False)
  |> should.equal(False)

  bool.min(False, False)
  |> should.equal(False)

  bool.min(False, True)
  |> should.equal(False)
}

pub fn to_int_test() {
  bool.to_int(True)
  |> should.equal(1)

  bool.to_int(False)
  |> should.equal(0)
}
