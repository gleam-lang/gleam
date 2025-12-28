use crate::{assert_js, assert_ts_def};

#[test]
fn list_literals() {
    assert_js!(
        r#"
pub fn go(x) {
  []
  [1]
  [1, 2]
  [1, 2, ..x]
}
"#,
    );
}

#[test]
fn long_list_literals() {
    assert_js!(
        r#"
pub fn go() {
  [111111111111111111111111111111111111111111111111111111111111111111111111]
  [11111111111111111111111111111111111111111111, 1111111111111111111111111111111111111111111]
}
"#,
    );
}

#[test]
fn multi_line_list_literals() {
    assert_js!(
        r#"
pub fn go(x) {
    [{True 1}]
}
"#,
    );
}

#[test]
fn list_constants() {
    assert_js!(
        r#"
pub const a = []
pub const b = [1, 2, 3]
"#,
    );
}

#[test]
fn list_constants_typescript() {
    assert_ts_def!(
        r#"
pub const a = []
pub const b = [1, 2, 3]
"#,
    );
}

#[test]
fn list_destructuring() {
    assert_js!(
        r#"
pub fn go(x, y) {
  let assert [] = x
  let assert [a] = x
  let assert [1, 2] = x
  let assert [_, #(3, b)] = y
  let assert [head, ..tail] = y
}
"#,
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
pub fn go() {
  [] == [1]
  [] != [1]
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
pub fn go(xs) {
  case xs {
    [] -> 0
    [_] -> 1
    [_, _] -> 2
    _ -> 9999
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/2904
#[test]
fn tight_empty_list() {
    assert_js!(
        r#"
pub fn go(func) {
  let huuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuge_variable = []
}
"#,
    );
}
