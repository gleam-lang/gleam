import assert

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

test reverse {
  reverse(Lt) |> assert:equal(_, Gt)
  reverse(Eq) |> assert:equal(_, Eq)
  reverse(Gt) |> assert:equal(_, Lt)
}

pub fn to_int(order) {
  case order {
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1
  }
}

test to_int {
  to_int(Lt) |> assert:equal(_, -1)
  to_int(Eq) |> assert:equal(_, 0)
  to_int(Gt) |> assert:equal(_, 1)
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

test compare {
  compare(Lt, Lt) |> assert:equal(_, Eq)
  compare(Lt, Eq) |> assert:equal(_, Lt)
  compare(Lt, Gt) |> assert:equal(_, Lt)
  compare(Eq, Lt) |> assert:equal(_, Gt)
  compare(Eq, Eq) |> assert:equal(_, Eq)
  compare(Eq, Gt) |> assert:equal(_, Lt)
  compare(Gt, Lt) |> assert:equal(_, Gt)
  compare(Gt, Eq) |> assert:equal(_, Gt)
  compare(Gt, Gt) |> assert:equal(_, Eq)
}

pub fn max(a, b) {
  case {a, b} {
  | {Gt, _} -> Gt
  | {Eq, Lt} -> Eq
  | _ -> b
  }
}

test max {
  max(Lt, Lt) |> assert:equal(_, Lt)
  max(Lt, Eq) |> assert:equal(_, Eq)
  max(Lt, Gt) |> assert:equal(_, Gt)
  max(Eq, Lt) |> assert:equal(_, Eq)
  max(Eq, Eq) |> assert:equal(_, Eq)
  max(Eq, Gt) |> assert:equal(_, Gt)
  max(Gt, Lt) |> assert:equal(_, Gt)
  max(Gt, Eq) |> assert:equal(_, Gt)
  max(Gt, Gt) |> assert:equal(_, Gt)
}

pub fn min(a, b) {
  case {a, b} {
  | {Lt, _} -> Lt
  | {Eq, Gt} -> Eq
  | _ -> b
  }
}

test min {
  min(Lt, Lt) |> assert:equal(_, Lt)
  min(Lt, Eq) |> assert:equal(_, Lt)
  min(Lt, Gt) |> assert:equal(_, Lt)
  min(Eq, Lt) |> assert:equal(_, Lt)
  min(Eq, Eq) |> assert:equal(_, Eq)
  min(Eq, Gt) |> assert:equal(_, Eq)
  min(Gt, Lt) |> assert:equal(_, Lt)
  min(Gt, Eq) |> assert:equal(_, Eq)
  min(Gt, Gt) |> assert:equal(_, Gt)
}
