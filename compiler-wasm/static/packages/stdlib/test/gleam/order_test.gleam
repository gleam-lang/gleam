import gleam/should
import gleam/order.{Eq, Gt, Lt}

pub fn reverse_test() {
  order.reverse(Lt)
  |> should.equal(Gt)

  order.reverse(Eq)
  |> should.equal(Eq)

  order.reverse(Gt)
  |> should.equal(Lt)
}

pub fn to_int_test() {
  order.to_int(Lt)
  |> should.equal(-1)

  order.to_int(Eq)
  |> should.equal(0)

  order.to_int(Gt)
  |> should.equal(1)
}

pub fn compare_test() {
  order.compare(Lt, Lt)
  |> should.equal(Eq)

  order.compare(Lt, Eq)
  |> should.equal(Lt)

  order.compare(Lt, Gt)
  |> should.equal(Lt)

  order.compare(Eq, Lt)
  |> should.equal(Gt)

  order.compare(Eq, Eq)
  |> should.equal(Eq)

  order.compare(Eq, Gt)
  |> should.equal(Lt)

  order.compare(Gt, Lt)
  |> should.equal(Gt)

  order.compare(Gt, Eq)
  |> should.equal(Gt)

  order.compare(Gt, Gt)
  |> should.equal(Eq)
}

pub fn max_test() {
  order.max(Lt, Lt)
  |> should.equal(Lt)

  order.max(Lt, Eq)
  |> should.equal(Eq)

  order.max(Lt, Gt)
  |> should.equal(Gt)

  order.max(Eq, Lt)
  |> should.equal(Eq)

  order.max(Eq, Eq)
  |> should.equal(Eq)

  order.max(Eq, Gt)
  |> should.equal(Gt)

  order.max(Gt, Lt)
  |> should.equal(Gt)

  order.max(Gt, Eq)
  |> should.equal(Gt)

  order.max(Gt, Gt)
  |> should.equal(Gt)
}

pub fn min_test() {
  order.min(Lt, Lt)
  |> should.equal(Lt)

  order.min(Lt, Eq)
  |> should.equal(Lt)

  order.min(Lt, Gt)
  |> should.equal(Lt)

  order.min(Eq, Lt)
  |> should.equal(Lt)

  order.min(Eq, Eq)
  |> should.equal(Eq)

  order.min(Eq, Gt)
  |> should.equal(Eq)

  order.min(Gt, Lt)
  |> should.equal(Lt)

  order.min(Gt, Eq)
  |> should.equal(Eq)

  order.min(Gt, Gt)
  |> should.equal(Gt)
}
