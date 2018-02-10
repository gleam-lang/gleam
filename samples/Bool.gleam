module Bool

export Bool(..), not/1, compare/2, max/2, min/2

import Order exposing Order(_)


type Bool
  = True
  | False

let not(bool) =
  match bool
  | True => False
  | False => True

test "not/1" =
  not(True) |> Assert.false
  not(False) |> Assert.true

let compare(a, b) =
  match (a, b)
  | (True, True) => EQ
  | (True, False) => GT
  | (False, False) => LT
  | (False, True) => GT

test "compare/2" =
  compare(True, True) |> Assert.equal(EQ)
  compare(True, False) |> Assert.equal(GT)
  compare(False, False) |> Assert.equal(LT)
  compare(False, True) |> Assert.equal(GT)

let max(a, b) =
  match a
  | True => True
  | False => b

test "max/2" =
  max(True, True) |> Assert.equal(True)
  max(True, False) |> Assert.equal(True)
  max(False, False) |> Assert.equal(False)
  max(False, True) |> Assert.equal(True)

let min(a, b) =
  match a
  | False => False
  | True => b

test "min/2" =
  min(True, True) |> Assert.equal(True)
  min(True, False) |> Assert.equal(False)
  min(False, False) |> Assert.equal(False)
  min(False, True) |> Assert.equal(False)

let to_int(bool) =
  match bool
  | False => 0
  | True => 1

test "to_int/1" =
  to_int(True) |> Assert.equal(1)
  to_int(False) |> Assert.equal(0)
