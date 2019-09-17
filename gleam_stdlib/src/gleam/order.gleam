import gleam/pair

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
  case pair.Pair(a, b) {
  | pair.Pair(Lt, Lt) -> Eq
  | pair.Pair(Lt, _) -> Lt
  | pair.Pair(Eq, Eq) -> Eq
  | pair.Pair(Gt, Gt) -> Eq
  | pair.Pair(Eq, Gt) -> Lt
  | _ -> Gt
  }
}

pub fn max(a, b) {
  case pair.Pair(a, b) {
  | pair.Pair(Gt, _) -> Gt
  | pair.Pair(Eq, Lt) -> Eq
  | _ -> b
  }
}

pub fn min(a, b) {
  case pair.Pair(a, b) {
  | pair.Pair(Lt, _) -> Lt
  | pair.Pair(Eq, Gt) -> Eq
  | _ -> b
  }
}
