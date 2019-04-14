pub enum Order =
  | Lt
  | Eq
  | Gt
;

pub fn reverse(order) {
  case order {
  | Lt -> Gt
  | Eq -> Eq
  | Gt -> Lt
  }
}

pub fn to_int(order) {
  case order {
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1
  }
}

pub fn compare(a, b) {
  case {a, b} {
  | {Lt, Lt} -> Eq
  | {Lt, _} -> Lt
  | {Eq, Eq} -> Eq
  | {Gt, Gt} -> Eq
  | {Eq, Gt} -> Lt
  | _ -> Gt
  }
}

pub fn max(a, b) {
  case {a, b} {
  | {Gt, _} -> Gt
  | {Eq, Lt} -> Eq
  | _ -> b
  }
}

pub fn min(a, b) {
  case {a, b} {
  | {Lt, _} -> Lt
  | {Eq, Gt} -> Eq
  | _ -> b
  }
}
