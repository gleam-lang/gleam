// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

use crate::assert_erl;

// https://github.com/gleam-lang/gleam/issues/1675
#[test]
fn alternative_pattern_variable_rewriting() {
    assert_erl!(
        "
pub fn myfun(mt) {
  case mt {
    1 | _ ->
      1
      |> Ok
  }
  1
  |> Ok
}
"
    )
}

#[test]
fn case_defining_some_variables_that_are_later_shadowed() {
    assert_erl!(
        "
pub fn go(x) {
  case x {
    1 -> {
      let a = 1
      Nil
    }
    2 -> {
      let a = 2
      Nil
    }
    _ -> Nil
  }

  let a = 3
  a + 1
}
"
    )
}

#[test]
fn case_defining_some_variables_that_are_later_shadowed_2() {
    assert_erl!(
        "
pub fn go(x) {
  case x {
    1 | 2 -> {
      let a = 1
      Nil
    }
    3 -> {
      let a = 2
      Nil
    }
    _ -> Nil
  }

  let a = 4
  a + 1
}
"
    )
}

#[test]
fn multiple_alternative_branches_with_same_variable_names() {
    assert_erl!(
        "
pub fn go(x) {
  case x {
    1 as x | x -> {
      let a = 1 + x
      Nil
    }
    3 as x | x -> {
      let a = 2 + x
      Nil
    }
  }

  let a = 3
  a + 1
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn positive_zero_pattern() {
    assert_erl!(
        "
pub fn main(x) {
  case x {
    0.0 -> 1
    _ -> 2
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/2349
#[test]
fn negative_zero_pattern() {
    assert_erl!(
        "
pub fn main(x) {
  case x {
    -0.0 -> 1
    _ -> 2
  }
}
"
    )
}

#[test]
fn not() {
    assert_erl!(
        r#"pub fn main(x, y) {
  case x {
    _ if !y -> 0
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn not_two() {
    assert_erl!(
        r#"pub fn main(x, y) {
  case x {
    _ if !y && !x -> 0
    _ -> 1
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/2657
#[test]
fn spread_empty_list() {
    assert_erl!(
        r#"
pub fn main() {
  case [] {
    [..] -> 1
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/2657
#[test]
fn spread_empty_list_assigning() {
    assert_erl!(
        r#"
pub fn main() {
  case [] {
    [..rest] -> rest
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5055
#[test]
fn alternative_patter_with_string_alias() {
    assert_erl!(
        r#"
pub fn main(x) {
  case x {
    "a" as letter <> _ | "b" as letter <> _ -> letter
    _ -> "wibble"
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/5115
#[test]
fn aliased_string_prefix_pattern_referenced_in_guard() {
    assert_erl!(
        r#"
pub fn main(x) {
  case x {
    "a" as letter <> _ if letter == x -> letter
    _ -> "wibble"
  }
}
"#,
    );
}

#[test]
fn list_with_tail_used_in_guard() {
    assert_erl!(
        r#"
pub fn go(x: List(Int), y: List(Int)) {
  case x {
    [1, ..x] if [1, 2, ..x] == y -> True
    _ -> False
  }
}"#
    )
}

#[test]
fn string_prefix_pattern_assigns_variable() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "a" as letter <> rest
    | "b" as letter <> rest -> letter <> rest
    _ -> ""
  }
}
"#
    )
}

#[test]
fn nested_string_prefix_pattern_assigns_variable() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    ["a" as letter <> rest, ..]
    | ["b" as letter <> rest, ..] -> letter <> rest
    _ -> ""
  }
}
"#
    )
}

#[test]
fn string_prefix_pattern_used_in_guard_assigns_variable() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "a" as letter <> rest
    | "b" as letter <> rest if letter == "c" -> letter <> rest
    _ -> ""
  }
}
"#
    )
}

#[test]
fn string_prefix_pattern_used_in_guard_assigns_variable_2() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "a" as letter <> rest
    | "b" as rest <> letter if letter == "c" && rest == "b" -> letter <> rest
    _ -> ""
  }
}
"#
    )
}

#[test]
fn string_prefix_pattern_used_in_guard_assigns_variable_3() {
    assert_erl!(
        r#"
pub fn go(x) {
  case x {
    "a" as rest <> _
    | "b" <> rest if rest == "c" -> Nil
    _ -> Nil
  }
}
"#
    )
}
