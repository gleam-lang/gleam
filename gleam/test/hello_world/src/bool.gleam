pub enum Bool =
  | True
  | False

pub enum Order =
  | Lt
  | Eq
  | Gt

pub fn not(bool) {
  case bool {
  | True -> False
  | False -> True
  }
}

pub fn compare(a, b) {
  case {a, b} {
  | {True, True} -> Eq
  | {True, False} -> Gt
  | {False, False} -> Eq
  | {False, True} -> Gt
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
