import order:[GT, EQ, LT]

pub fn not(bool) {
  case bool {
  | True => False
  | False => True
  }
}

test not {
  not(True)
    |> assert:false

  not(False)
    |> assert:true
}

pub fn compare(a, b) {
  case (a, b) {
  | (True, True) => EQ
  | (True, False) => GT
  | (False, False) => EQ
  | (False, True) => GT
  }
}

test compare {
  compare(True, True)
    |> assert:equal(_, EQ)

  compare(True, False)
    |> assert:equal(_, GT)

  compare(False, False)
    |> assert:equal(_, LT)

  compare(False, True)
    |> assert:equal(_, GT)
}

pub fn max(a, b) {
  case a {
  | True => True
  | False => b
  }
}

test max {
  max(True, True)
    |> assert:equal(_, True)

  max(True, False)
    |> assert:equal(_, True)

  max(False, False)
    |> assert:equal(_, False)

  max(False, True)
    |> assert:equal(_, True)
}

pub fn min(a, b) {
  case a {
  | False => False
  | True => b
  }
}

test min {
  min(True, True)
    |> assert:equal(_, True)

  min(True, False)
    |> assert:equal(_, False)

  min(False, False)
    |> assert:equal(_, False)

  min(False, True)
    |> assert:equal(_, False)
}

pub fn to_int(bool) {
  case bool {
  | False => 0
  | True => 1
  }
}

test to_int {
  to_int(True)
    |> assert:equal(_, 1)

  to_int(False)
    |> assert:equal(_, 0)
}
