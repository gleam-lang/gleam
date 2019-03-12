import expect

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
  reverse(Lt) |> expect:equal(_, Gt)
  reverse(Eq) |> expect:equal(_, Eq)
  reverse(Gt) |> expect:equal(_, Lt)
}

pub fn to_int(order) {
  case order {
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1
  }
}

test to_int {
  to_int(Lt) |> expect:equal(_, -1)
  to_int(Eq) |> expect:equal(_, 0)
  to_int(Gt) |> expect:equal(_, 1)
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
  compare(Lt, Lt) |> expect:equal(_, Eq)
  compare(Lt, Eq) |> expect:equal(_, Lt)
  compare(Lt, Gt) |> expect:equal(_, Lt)
  compare(Eq, Lt) |> expect:equal(_, Gt)
  compare(Eq, Eq) |> expect:equal(_, Eq)
  compare(Eq, Gt) |> expect:equal(_, Lt)
  compare(Gt, Lt) |> expect:equal(_, Gt)
  compare(Gt, Eq) |> expect:equal(_, Gt)
  compare(Gt, Gt) |> expect:equal(_, Eq)
}

pub fn max(a, b) {
  case {a, b} {
  | {Gt, _} -> Gt
  | {Eq, Lt} -> Eq
  | _ -> b
  }
}

test max {
  max(Lt, Lt) |> expect:equal(_, Lt)
  max(Lt, Eq) |> expect:equal(_, Eq)
  max(Lt, Gt) |> expect:equal(_, Gt)
  max(Eq, Lt) |> expect:equal(_, Eq)
  max(Eq, Eq) |> expect:equal(_, Eq)
  max(Eq, Gt) |> expect:equal(_, Gt)
  max(Gt, Lt) |> expect:equal(_, Gt)
  max(Gt, Eq) |> expect:equal(_, Gt)
  max(Gt, Gt) |> expect:equal(_, Gt)
}

pub fn min(a, b) {
  case {a, b} {
  | {Lt, _} -> Lt
  | {Eq, Gt} -> Eq
  | _ -> b
  }
}

test min {
  min(Lt, Lt) |> expect:equal(_, Lt)
  min(Lt, Eq) |> expect:equal(_, Lt)
  min(Lt, Gt) |> expect:equal(_, Lt)
  min(Eq, Lt) |> expect:equal(_, Lt)
  min(Eq, Eq) |> expect:equal(_, Eq)
  min(Eq, Gt) |> expect:equal(_, Eq)
  min(Gt, Lt) |> expect:equal(_, Lt)
  min(Gt, Eq) |> expect:equal(_, Eq)
  min(Gt, Gt) |> expect:equal(_, Gt)
}
