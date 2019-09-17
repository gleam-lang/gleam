import gleam/order
import gleam/pair

pub fn negate(bool) {
  case bool {
  | True -> False
  | False -> True
  }
}

pub fn compare(a, b) {
  case pair.Pair(a, b) {
  | pair.Pair(True, True) -> order.Eq
  | pair.Pair(True, False) -> order.Gt
  | pair.Pair(False, False) -> order.Eq
  | pair.Pair(False, True) -> order.Lt
  }
}

pub fn max(a, b) {
  case a {
  | True -> True
  | False -> b
  }
}

pub fn min(a, b) {
  case a {
  | False -> False
  | True -> b
  }
}

pub fn to_int(bool) {
  case bool {
  | False -> 0
  | True -> 1
  }
}
