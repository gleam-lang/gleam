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
