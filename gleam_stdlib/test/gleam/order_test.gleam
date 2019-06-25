import gleam/expect
import gleam/order

pub fn reverse_test() {
  order:reverse(order:Lt)
  |> expect:equal(_, order:Gt)

  order:reverse(order:Eq)
  |> expect:equal(_, order:Eq)

  order:reverse(order:Gt)
  |> expect:equal(_, order:Lt)
}

pub fn to_int_test() {
  order:to_int(order:Lt)
  |> expect:equal(_, -1)

  order:to_int(order:Eq)
  |> expect:equal(_, 0)

  order:to_int(order:Gt)
  |> expect:equal(_, 1)
}

pub fn compare_test() {
  order:compare(order:Lt, order:Lt)
  |> expect:equal(_, order:Eq)

  order:compare(order:Lt, order:Eq)
  |> expect:equal(_, order:Lt)

  order:compare(order:Lt, order:Gt)
  |> expect:equal(_, order:Lt)

  order:compare(order:Eq, order:Lt)
  |> expect:equal(_, order:Gt)

  order:compare(order:Eq, order:Eq)
  |> expect:equal(_, order:Eq)

  order:compare(order:Eq, order:Gt)
  |> expect:equal(_, order:Lt)

  order:compare(order:Gt, order:Lt)
  |> expect:equal(_, order:Gt)

  order:compare(order:Gt, order:Eq)
  |> expect:equal(_, order:Gt)

  order:compare(order:Gt, order:Gt)
  |> expect:equal(_, order:Eq)
}

pub fn max_test() {
  order:max(order:Lt, order:Lt)
  |> expect:equal(_, order:Lt)

  order:max(order:Lt, order:Eq)
  |> expect:equal(_, order:Eq)

  order:max(order:Lt, order:Gt)
  |> expect:equal(_, order:Gt)

  order:max(order:Eq, order:Lt)
  |> expect:equal(_, order:Eq)

  order:max(order:Eq, order:Eq)
  |> expect:equal(_, order:Eq)

  order:max(order:Eq, order:Gt)
  |> expect:equal(_, order:Gt)

  order:max(order:Gt, order:Lt)
  |> expect:equal(_, order:Gt)

  order:max(order:Gt, order:Eq)
  |> expect:equal(_, order:Gt)

  order:max(order:Gt, order:Gt)
  |> expect:equal(_, order:Gt)
}

pub fn min_test() {
  order:min(order:Lt, order:Lt)
  |> expect:equal(_, order:Lt)

  order:min(order:Lt, order:Eq)
  |> expect:equal(_, order:Lt)

  order:min(order:Lt, order:Gt)
  |> expect:equal(_, order:Lt)

  order:min(order:Eq, order:Lt)
  |> expect:equal(_, order:Lt)

  order:min(order:Eq, order:Eq)
  |> expect:equal(_, order:Eq)

  order:min(order:Eq, order:Gt)
  |> expect:equal(_, order:Eq)

  order:min(order:Gt, order:Lt)
  |> expect:equal(_, order:Lt)

  order:min(order:Gt, order:Eq)
  |> expect:equal(_, order:Eq)

  order:min(order:Gt, order:Gt)
  |> expect:equal(_, order:Gt)
}
