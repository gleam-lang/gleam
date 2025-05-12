use crate::assert_js;

#[test]
fn case_on_error() {
    assert_js!(
        r#"
fn a_result() { Error(1) }

pub fn main() {
  case a_result() {
    Error(_) -> 1
    _ -> 2
  }
}"#
    );
}

#[test]
fn tuple_and_guard() {
    assert_js!(
        r#"
fn go(x) {
  case #(1, 2) {
    #(1, a) if a == 2 -> 1
    #(_, _) -> 2
  }
}
"#,
    )
}

#[test]
fn guard_variable_only_brought_into_scope_when_needed() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    // We want `a` to be defined before the guard check, and
    // `b` to be defined only if the predicate on a matches!
    [a, b] if a == 1 -> a + b
    _ -> 2
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/4221
#[test]
fn guard_variable_only_brought_into_scope_when_needed_1() {
    assert_js!(
        r#"
pub fn main() {
  case 1 {
    i if i == 1 -> True
    i if i < 2 -> True
    _ -> False
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/1187
#[test]
fn pointless() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    _ -> x
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1188
#[test]
fn following_todo() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    True -> todo
    _ -> 1
  }
}
"#,
    )
}

#[test]
fn multi_subject_catch_all() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_or() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, _ | _, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_no_catch_all() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, _ -> 1
    _, True -> 2
    False, False -> 0
  }
}
"#,
    )
}

#[test]
fn multi_subject_subject_assignments() {
    assert_js!(
        r#"
fn go() {
  case True, False {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
    )
}

#[test]
fn assignment() {
    assert_js!(
        r#"
fn go(x) {
  let y = case x {
    True -> 1
    _ -> 0
  }
  y
}
"#,
    )
}

#[test]
fn preassign_assignment() {
    assert_js!(
        r#"
fn go(x) {
  let y = case x() {
    True -> 1
    _ -> 0
  }
  y
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1237
#[test]
fn pipe() {
    assert_js!(
        r#"
fn go(x, f) {
  case x |> f {
    0 -> 1
    _ -> 2
  }
}
"#,
    )
}

#[test]
fn result() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    Ok(_) -> 1
    Error(_) -> 0
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1506
#[test]
fn called_case() {
    assert_js!(
        r#"
fn go(x, y) {
  case x {
    0 -> y
    _ -> y
  }()
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/1978
#[test]
fn case_local_var_in_tuple() {
    assert_js!(
        r#"
fn go(x, y) {
  let z = False
  case True {
    x if #(x, z) == #(True, False) -> x
    _ -> False
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2665
#[test]
fn case_branches_guards_are_wrapped_in_parentheses() {
    assert_js!(
        r#"
fn anything() -> a {
  case [] {
    [a] if False || True -> a
    _ -> anything()
  }
}
"#,
    )
}

// https://github.com/gleam-lang/gleam/issues/2759
#[test]
fn nested_string_prefix_match() {
    assert_js!(
        r#"
fn main() {
  case Ok(["a", "b c", "d"]) {
    Ok(["a", "b " <> _, "d"]) -> 1
    _ -> 1
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2759
#[test]
fn nested_string_prefix_match_that_would_crash_on_js() {
    assert_js!(
        r#"
fn main() {
  case Ok(["b c", "d"]) {
    Ok(["b " <> _, "d"]) -> 1
    _ -> 1
  }
}
"#
    );
}

#[test]
fn slicing_is_handled_properly_with_multiple_branches() {
    assert_js!(
        r#"
pub fn main() {
  case "12345" {
    "0" <> rest -> rest
    "123" <> rest -> rest
    _ -> ""
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3379
#[test]
fn single_clause_variables() {
    assert_js!(
        r#"
pub fn main() {
  let text = "first defined"
  case "defined again" {
    text -> Nil
  }
  let text = "a third time"
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3379
#[test]
fn single_clause_variables_assigned() {
    assert_js!(
        r#"
pub fn main() {
  let text = "first defined"
  let other = case "defined again" {
    text -> Nil
  }
  let text = "a third time"
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/3894
#[test]
fn nested_string_prefix_assignment() {
    assert_js!(
        r#"
type Wibble {
  Wibble(wobble: String)
}

pub fn main() {
  let tmp = Wibble(wobble: "wibble")
  case tmp {
    Wibble(wobble: "w" as wibble <> rest) -> wibble <> rest
    _ -> panic
  }
}
"#
    )
}

#[test]
fn deeply_nested_string_prefix_assignment() {
    assert_js!(
        r#"
type Wibble {
  Wibble(Wobble)
}
type Wobble {
  Wobble(wabble: Wabble)
}
type Wabble {
  Wabble(tuple: #(Int, String))
}

pub fn main() {
  let tmp = Wibble(Wobble(Wabble(#(42, "wibble"))))
  case tmp {
    Wibble(Wobble(Wabble(#(_int, "w" as wibble <> rest)))) -> wibble <> rest
    _ -> panic
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/4383
#[test]
fn record_update_in_pipeline_in_case_clause() {
    assert_js!(
        "
type Wibble {
  Wibble(wibble: Int, wobble: Int)
}

fn identity(x) {
  x
}

fn go(x) {
  case x {
    Wibble(1, _) -> Wibble(..x, wibble: 4) |> identity
    Wibble(_, 3) -> Wibble(..x, wobble: 10) |> identity
    _ -> panic
  }
}
"
    );
}

#[test]
fn pattern_matching_on_aliased_result_constructor() {
    assert_js!(
        "
import gleam.{Error as E, Ok as O}

pub fn go(x) {
  case x {
    E(_) -> 1
    O(_) -> 2
  }
}
"
    );
}

#[test]
fn bit_array_assignment_utf16() {
    assert_js!(
        r#"
pub fn main() {
  case <<"Hello":utf16>> {
    <<"Hello" as m:utf16>> -> m
    _ -> ""
  }
}
"#
    );
}

#[test]
fn bit_array_assignment_utf32() {
    assert_js!(
        r#"
pub fn main() {
  case <<"Hello":utf32>> {
    <<"Hello" as m:utf32>> -> m
    _ -> ""
  }
}
"#
    );
}

#[test]
fn bit_array_assignment_utf16_little_endian() {
    assert_js!(
        r#"
pub fn main() {
  case <<"Hello":utf16-little>> {
    <<"Hello" as m:utf16-little>> -> m
    _ -> ""
  }
}
"#
    );
}

#[test]
fn bit_array_assignment_utf32_little_endian() {
    assert_js!(
        r#"
pub fn main() {
  case <<"Hello":utf32-little>> {
    <<"Hello" as m:utf32-little>> -> m
    _ -> ""
  }
}
"#
    );
}
