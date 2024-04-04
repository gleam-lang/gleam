use crate::assert_js;

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
