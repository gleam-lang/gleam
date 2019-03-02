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
  let _ = reverse(Lt) |> expect:equal(_, Gt)
  let _ = reverse(Eq) |> expect:equal(_, Eq)
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
  let _ = to_int(Lt) |> expect:equal(_, -1)
  let _ = to_int(Eq) |> expect:equal(_, 0)
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
  let _ = compare(Lt, Lt) |> expect:equal(_, Eq)
  let _ = compare(Lt, Eq) |> expect:equal(_, Lt)
  let _ = compare(Lt, Gt) |> expect:equal(_, Lt)
  let _ = compare(Eq, Lt) |> expect:equal(_, Gt)
  let _ = compare(Eq, Eq) |> expect:equal(_, Eq)
  let _ = compare(Eq, Gt) |> expect:equal(_, Lt)
  let _ = compare(Gt, Lt) |> expect:equal(_, Gt)
  let _ = compare(Gt, Eq) |> expect:equal(_, Gt)
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
  let _ = max(Lt, Lt) |> expect:equal(_, Lt)
  let _ = max(Lt, Eq) |> expect:equal(_, Eq)
  let _ = max(Lt, Gt) |> expect:equal(_, Gt)
  let _ = max(Eq, Lt) |> expect:equal(_, Eq)
  let _ = max(Eq, Eq) |> expect:equal(_, Eq)
  let _ = max(Eq, Gt) |> expect:equal(_, Gt)
  let _ = max(Gt, Lt) |> expect:equal(_, Gt)
  let _ = max(Gt, Eq) |> expect:equal(_, Gt)
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
  let _ = min(Lt, Lt) |> expect:equal(_, Lt)
  let _ = min(Lt, Eq) |> expect:equal(_, Lt)
  let _ = min(Lt, Gt) |> expect:equal(_, Lt)
  let _ = min(Eq, Lt) |> expect:equal(_, Lt)
  let _ = min(Eq, Eq) |> expect:equal(_, Eq)
  let _ = min(Eq, Gt) |> expect:equal(_, Eq)
  let _ = min(Gt, Lt) |> expect:equal(_, Lt)
  let _ = min(Gt, Eq) |> expect:equal(_, Eq)
  min(Gt, Gt) |> expect:equal(_, Gt)
}
