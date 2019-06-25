import gleam/bool
import gleam/order
import gleam/expect

pub fn negate_test() {
  bool:negate(True)
  |> expect:false

  bool:negate(False)
  |> expect:true
}

pub fn compare_test() {
  bool:compare(True, True)
  |> expect:equal(_, order:Eq)

  bool:compare(True, False)
  |> expect:equal(_, order:Gt)

  bool:compare(False, False)
  |> expect:equal(_, order:Eq)

  bool:compare(False, True)
  |> expect:equal(_, order:Lt)
}

pub fn max_test() {
  bool:max(True, True)
  |> expect:equal(_, True)

  bool:max(True, False)
  |> expect:equal(_, True)

  bool:max(False, False)
  |> expect:equal(_, False)

  bool:max(False, True)
  |> expect:equal(_, True)
}

pub fn min_test() {
  bool:min(True, True)
  |> expect:equal(_, True)

  bool:min(True, False)
  |> expect:equal(_, False)

  bool:min(False, False)
  |> expect:equal(_, False)

  bool:min(False, True)
  |> expect:equal(_, False)
}

pub fn to_int_test() {
  bool:to_int(True)
  |> expect:equal(_, 1)

  bool:to_int(False)
  |> expect:equal(_, 0)
}
