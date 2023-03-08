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

#[test]
fn module_const_vars() {
    assert_erl!(
        "const int = 42
const int_alias = int
pub fn use_int_alias() { int_alias }

fn int_identity(i: Int) { i }
const int_identity_alias: fn(Int) -> Int = int_identity
pub fn use_int_identity_alias() { int_identity_alias(42) }

const compound: #(Int, fn(Int) -> Int, fn(Int) -> Int) = #(int, int_identity, int_identity_alias)
pub fn use_compound() { compound.1(compound.0) }
"
    )
}

#[test]
fn blocks_are_scopes() {
    assert_erl!(
        "
pub fn main() {
  let x = 1
  {
    let x = 2
  }
  x
}
"
    )
}
