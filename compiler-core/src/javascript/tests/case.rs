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
    0 -> Nil
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
