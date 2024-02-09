//// A set of functions for bitwise operations on integers.

/// Calculates the bitwise AND of its arguments.
pub fn and(x: Int, y: Int) -> Int {
  do_and(x, y)
}

if erlang {
  external fn do_and(Int, Int) -> Int =
    "erlang" "band"
}

if javascript {
  external fn do_and(Int, Int) -> Int =
    "../gleam_bitwise.mjs" "and"
}

/// Calculates the bitwise NOT of its argument.
pub fn not(x: Int) -> Int {
  do_not(x)
}

if erlang {
  external fn do_not(Int) -> Int =
    "erlang" "bnot"
}

if javascript {
  external fn do_not(Int) -> Int =
    "../gleam_bitwise.mjs" "not"
}

/// Calculates the bitwise OR of its arguments.
pub fn or(x: Int, y: Int) -> Int {
  do_or(x, y)
}

if erlang {
  external fn do_or(Int, Int) -> Int =
    "erlang" "bor"
}

if javascript {
  external fn do_or(Int, Int) -> Int =
    "../gleam_bitwise.mjs" "or"
}

/// Calculates the bitwise XOR of its arguments.
pub fn exclusive_or(x: Int, y: Int) -> Int {
  do_exclusive_or(x, y)
}

if erlang {
  external fn do_exclusive_or(Int, Int) -> Int =
    "erlang" "bxor"
}

if javascript {
  external fn do_exclusive_or(Int, Int) -> Int =
    "../gleam_bitwise.mjs" "exclusive_or"
}

/// Calculates the result of an arithmetic left bitshift.
pub fn shift_left(x: Int, y: Int) -> Int {
  do_shift_left(x, y)
}

if erlang {
  external fn do_shift_left(Int, Int) -> Int =
    "erlang" "bsl"
}

if javascript {
  external fn do_shift_left(Int, Int) -> Int =
    "../gleam_bitwise.mjs" "shift_left"
}

/// Calculates the result of an arithmetic right bitshift.
pub fn shift_right(x: Int, y: Int) -> Int {
  do_shift_right(x, y)
}

if erlang {
  external fn do_shift_right(Int, Int) -> Int =
    "erlang" "bsr"
}

if javascript {
  external fn do_shift_right(Int, Int) -> Int =
    "../gleam_bitwise.mjs" "shift_right"
}
