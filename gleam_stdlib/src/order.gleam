import assert

pub enum Order =
  | LT
  | EQ
  | GT
;

pub fn reverse(order) {
  case order {
  | LT -> GT
  | EQ -> EQ
  | GT -> LT
  }
}

test reverse {
  reverse(LT) |> assert:equal(_, GT)
  reverse(EQ) |> assert:equal(_, EQ)
  reverse(GT) |> assert:equal(_, LT)
}

pub fn to_int(order) {
  case order {
  | LT -> -1
  | EQ -> 0
  | GT -> 1
  }
}

test to_int {
  to_int(LT) |> assert:equal(_, -1)
  to_int(EQ) |> assert:equal(_, 0)
  to_int(GT) |> assert:equal(_, 1)
}

pub fn compare(a, b) {
  case {a, b} {
  | {LT, LT} -> EQ
  | {LT, _} -> LT
  | {EQ, EQ} -> EQ
  | {GT, GT} -> EQ
  | {EQ, GT} -> LT
  | _ -> GT
  }
}

test compare {
  compare(LT, LT) |> assert:equal(_, EQ)
  compare(LT, EQ) |> assert:equal(_, LT)
  compare(LT, GT) |> assert:equal(_, LT)
  compare(EQ, LT) |> assert:equal(_, GT)
  compare(EQ, EQ) |> assert:equal(_, EQ)
  compare(EQ, GT) |> assert:equal(_, LT)
  compare(GT, LT) |> assert:equal(_, GT)
  compare(GT, EQ) |> assert:equal(_, GT)
  compare(GT, GT) |> assert:equal(_, EQ)
}

pub fn max(a, b) {
  case {a, b} {
  | {GT, _} -> GT
  | {EQ, LT} -> EQ
  | _ -> b
  }
}

test max {
  max(LT, LT) |> assert:equal(_, LT)
  max(LT, EQ) |> assert:equal(_, EQ)
  max(LT, GT) |> assert:equal(_, GT)
  max(EQ, LT) |> assert:equal(_, EQ)
  max(EQ, EQ) |> assert:equal(_, EQ)
  max(EQ, GT) |> assert:equal(_, GT)
  max(GT, LT) |> assert:equal(_, GT)
  max(GT, EQ) |> assert:equal(_, GT)
  max(GT, GT) |> assert:equal(_, GT)
}

pub fn min(a, b) {
  case {a, b} {
  | {LT, _} -> LT
  | {EQ, GT} -> EQ
  | _ -> b
  }
}

test min {
  min(LT, LT) |> assert:equal(_, LT)
  min(LT, EQ) |> assert:equal(_, LT)
  min(LT, GT) |> assert:equal(_, LT)
  min(EQ, LT) |> assert:equal(_, LT)
  min(EQ, EQ) |> assert:equal(_, EQ)
  min(EQ, GT) |> assert:equal(_, EQ)
  min(GT, LT) |> assert:equal(_, LT)
  min(GT, EQ) |> assert:equal(_, EQ)
  min(GT, GT) |> assert:equal(_, GT)
}
