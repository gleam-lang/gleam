import Order:Order

pub enum Bool =
  | True
  | False

import Bool:*

pub fn not(bool) {
  case bool {
  | True => False
  | False => True
  }
}

test not {
  not(True)
    |> Assert:false

  not(False)
    |> Assert:true
}

pub fn compare(a, b) {
  case (a, b) {
  | (True, True) => Order:EQ
  | (True, False) => Order:GT
  | (False, False) => Order:EQ
  | (False, True) => Order:GT
  }
}

test compare {
  compare(True, True)
    |> Assert:equal(_, Order:EQ)

  compare(True, False)
    |> Assert:equal(_, Order:GT)

  compare(False, False)
    |> Assert:equal(_, Order:LT)

  compare(False, True)
    |> Assert:equal(_, Order:GT)
}

pub fn max(a, b) {
  case a {
  | True => True
  | False => b
  }
}

test max {
  max(True, True)
    |> Assert:equal(_, True)

  max(True, False)
    |> Assert:equal(_, True)

  max(False, False)
    |> Assert:equal(_, False)

  max(False, True)
    |> Assert:equal(_, True)
}

pub fn min(a, b) {
  case a {
  | False => False
  | True => b
  }
}

test min {
  min(True, True)
    |> Assert:equal(_, True)

  min(True, False)
    |> Assert:equal(_, False)

  min(False, False)
    |> Assert:equal(_, False)

  min(False, True)
    |> Assert:equal(_, False)
}

pub fn to_int(bool) {
  case bool {
  | False => 0
  | True => 1
  }
}

test to_int {
  to_int(True)
    |> Assert:equal(_, 1)

  to_int(False)
    |> Assert:equal(_, 0)
}
