import gleam/expect
import gleam/order.{Lt, Eq, Gt}

pub fn reverse_test() {
  order.reverse(Lt)
  |> expect.equal(_, Gt)

  order.reverse(order.Eq)
  |> expect.equal(_, order.Eq)

  order.reverse(Gt)
  |> expect.equal(_, Lt)
}

pub fn to_int_test() {
  order.to_int(Lt)
  |> expect.equal(_, -1)

  order.to_int(order.Eq)
  |> expect.equal(_, 0)

  order.to_int(Gt)
  |> expect.equal(_, 1)
}

pub fn compare_test() {
  order.compare(Lt, Lt)
  |> expect.equal(_, order.Eq)

  order.compare(Lt, order.Eq)
  |> expect.equal(_, Lt)

  order.compare(Lt, Gt)
  |> expect.equal(_, Lt)

  order.compare(order.Eq, Lt)
  |> expect.equal(_, Gt)

  order.compare(order.Eq, order.Eq)
  |> expect.equal(_, order.Eq)

  order.compare(order.Eq, Gt)
  |> expect.equal(_, Lt)

  order.compare(Gt, Lt)
  |> expect.equal(_, Gt)

  order.compare(Gt, order.Eq)
  |> expect.equal(_, Gt)

  order.compare(Gt, Gt)
  |> expect.equal(_, order.Eq)
}

pub fn max_test() {
  order.max(Lt, Lt)
  |> expect.equal(_, Lt)

  order.max(Lt, order.Eq)
  |> expect.equal(_, order.Eq)

  order.max(Lt, Gt)
  |> expect.equal(_, Gt)

  order.max(order.Eq, Lt)
  |> expect.equal(_, order.Eq)

  order.max(order.Eq, order.Eq)
  |> expect.equal(_, order.Eq)

  order.max(order.Eq, Gt)
  |> expect.equal(_, Gt)

  order.max(Gt, Lt)
  |> expect.equal(_, Gt)

  order.max(Gt, order.Eq)
  |> expect.equal(_, Gt)

  order.max(Gt, Gt)
  |> expect.equal(_, Gt)
}

pub fn min_test() {
  order.min(Lt, Lt)
  |> expect.equal(_, Lt)

  order.min(Lt, order.Eq)
  |> expect.equal(_, Lt)

  order.min(Lt, Gt)
  |> expect.equal(_, Lt)

  order.min(order.Eq, Lt)
  |> expect.equal(_, Lt)

  order.min(order.Eq, order.Eq)
  |> expect.equal(_, order.Eq)

  order.min(order.Eq, Gt)
  |> expect.equal(_, order.Eq)

  order.min(Gt, Lt)
  |> expect.equal(_, Lt)

  order.min(Gt, order.Eq)
  |> expect.equal(_, order.Eq)

  order.min(Gt, Gt)
  |> expect.equal(_, Gt)
}
