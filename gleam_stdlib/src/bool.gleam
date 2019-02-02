import order:[Gt, Eq, Lt]

pub fn not(bool) {
  case bool {
  | True -> False
  | False -> True
  }
}

test not {
  not(True)
    |> expect:false

  not(False)
    |> expect:true
}

pub fn compare(a, b) {
  case {a, b} {
  | {True, True} -> Eq
  | {True, False} -> Gt
  | {False, False} -> Eq
  | {False, True} -> Gt
  }
}

test compare {
  compare(True, True)
    |> expect:equal(_, Eq)

  compare(True, False)
    |> expect:equal(_, Gt)

  compare(False, False)
    |> expect:equal(_, Lt)

  compare(False, True)
    |> expect:equal(_, Gt)
}

pub fn max(a, b) {
  case a {
  | True -> True
  | False -> b
  }
}

test max {
  max(True, True)
    |> expect:equal(_, True)

  max(True, False)
    |> expect:equal(_, True)

  max(False, False)
    |> expect:equal(_, False)

  max(False, True)
    |> expect:equal(_, True)
}

pub fn min(a, b) {
  case a {
  | False -> False
  | True -> b
  }
}

test min {
  min(True, True)
    |> expect:equal(_, True)

  min(True, False)
    |> expect:equal(_, False)

  min(False, False)
    |> expect:equal(_, False)

  min(False, True)
    |> expect:equal(_, False)
}

pub fn to_int(bool) {
  case bool {
  | False -> 0
  | True -> 1
  }
}

test to_int {
  to_int(True)
    |> expect:equal(_, 1)

  to_int(False)
    |> expect:equal(_, 0)
}
