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
  case struct(a, b) {
  | struct(Lt, Lt) -> Eq
  | struct(Lt, _) -> Lt
  | struct(Eq, Eq) -> Eq
  | struct(Gt, Gt) -> Eq
  | struct(Eq, Gt) -> Lt
  | _ -> Gt
  }
}

pub fn max(a, b) {
  case struct(a, b) {
  | struct(Gt, _) -> Gt
  | struct(Eq, Lt) -> Eq
  | _ -> b
  }
}

pub fn min(a, b) {
  case struct(a, b) {
  | struct(Lt, _) -> Lt
  | struct(Eq, Gt) -> Eq
  | _ -> b
  }
}
