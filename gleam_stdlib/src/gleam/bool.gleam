import gleam/order

pub fn negate(bool) {
  case bool {
  | True -> False
  | False -> True
  }
}

pub fn compare(a, b) {
  case struct(a, b) {
  | struct(True, True) -> order:Eq
  | struct(True, False) -> order:Gt
  | struct(False, False) -> order:Eq
  | struct(False, True) -> order:Lt
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
