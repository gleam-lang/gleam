module Bool exposing Bool(..), not/1, compare/2, max/2, min/2

import Order exposing Order(_)

type Bool
  = True
  | False

fn not(bool) =
  case bool
  | True => False
  | False => True

test not =
  not(True) |> Assert.false
  not(False) |> Assert.true

fn compare(a, b) =
  case (a, b)
  | (True, True) => EQ
  | (True, False) => GT
  | (False, False) => EQ
  | (False, True) => GT

test compare =
  compare(True, True) |> Assert.equal(_, EQ)
  compare(True, False) |> Assert.equal(_, GT)
  compare(False, False) |> Assert.equal(_, LT)
  compare(False, True) |> Assert.equal(_, GT)

fn max(a, b) =
  case a
  | True => True
  | False => b

test max =
  max(True, True) |> Assert.equal(_, True)
  max(True, False) |> Assert.equal(_, True)
  max(False, False) |> Assert.equal(_, False)
  max(False, True) |> Assert.equal(_, True)

fn min(a, b) =
  case a
  | False => False
  | True => b

test min =
  min(True, True) |> Assert.equal(_, True)
  min(True, False) |> Assert.equal(_, False)
  min(False, False) |> Assert.equal(_, False)
  min(False, True) |> Assert.equal(_, False)

fn to_int(bool) =
  case bool
  | False => 0
  | True => 1

test to_int =
  to_int(True) |> Assert.equal(_, 1)
  to_int(False) |> Assert.equal(_, 0)
