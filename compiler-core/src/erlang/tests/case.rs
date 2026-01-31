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
fn local_variable_record_access_variant_inference() {
    assert_erl!(
        r#"
pub type User {
  User(name: String, age: Int)
  Guest
}

pub fn main() {
  let user = User("Gleam", 42)
  
  case user {
    User(..) -> user.name
    Guest -> "Guest"
  }
}
"#
    );
}

#[test]
fn local_variable_as_case_subject_shadows_const() {
    assert_erl!(
        r#"
pub type Wibble {
  Wibble
  Wobble(int: Int)
}

const wibble = Wibble

pub fn main() {
  echo wibble
  // This 'wibble' shadows the const
  let wibble = Wobble(42)
  case wibble {
    // This matches the local variable, not the const
    Wobble(_) -> wibble
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/5261
#[test]
fn case_with_record_const_as_subject_with_record_constructor_clause_and_referencing_same_const_inside_clause_consequence()
 {
    assert_erl!(
        r#"
pub type Wibble {
  Wibble
  Wobble(int: Int)
}

const wibble = Wibble

pub fn main() {
  case wibble {
    Wobble(_) -> wibble
    wobble -> wibble
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/5261
#[test]
fn const_record_access_variant_inference() {
    assert_erl!(
        r#"
pub type Wibble {
  Wibble
  Wobble(int: Int)
}

const wibble = Wobble(42)

pub fn main() {
  case wibble {
    Wibble -> 24
    Wobble(_) -> wibble.int
  }
}
"#
    )
}
