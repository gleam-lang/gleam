use crate::assert_erl;

#[test]
fn shadow_let() {
    // https://github.com/gleam-lang/gleam/issues/333
    assert_erl!(
        r#"
pub fn go(a) {
  case a {
    99 -> {
      let a = a
      1
    }
    _ -> a
  }
}"#
    );
}

#[test]
fn shadow_param() {
    // https://github.com/gleam-lang/gleam/issues/772
    assert_erl!(
        "pub fn main(board) {
fn(board) { board }
  board
}"
    );
}

#[test]
fn shadow_and_call() {
    // https://github.com/gleam-lang/gleam/issues/762
    assert_erl!(
        r#"
pub fn main(x) {
  fn(x) { x }(x)
}
"#
    );
}

#[test]
fn shadow_pipe() {
    assert_erl!(
        r#"
pub fn main(x) {
  x
  |> fn(x) { x }
}
"#
    );
}

#[test]
fn discarded() {
    // https://github.com/gleam-lang/gleam/issues/788
    assert_erl!(
        r#"pub fn go() {
  let _r = 1
  let _r = 2
  Nil
}"#
    );
}

#[test]
fn anon_external_fun_name_escaping() {
    // https://github.com/gleam-lang/gleam/issues/1418
    assert_erl!(
        r#"external fn func() -> Nil = "one.two" "three.four"

pub fn main() {
  func
}"#
    );
}
