use crate::assert_erl;

#[test]
fn clever_pipe_rewriting() {
    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#
    );

    // a |> b(c)
    assert_erl!(
        r#"
pub fn apply(f: fn(a, Int) -> b, a: a) { a |> f(1) }
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/952
#[test]
fn block_expr_into_pipe() {
    assert_erl!(
        r#"fn id(a) { a }
pub fn main() {
  {
    let x = 1
    x
  }
  |> id
}"#
    );
}

#[test]
fn pipe_in_list() {
    assert_erl!(
        "pub fn x(f) {
  [
    1 |> f
  ]
}"
    );
}

#[test]
fn pipe_in_tuple() {
    assert_erl!(
        "pub fn x(f) {
  #(
    1 |> f
  )
}"
    );
}

#[test]
fn pipe_in_case_subject() {
    assert_erl!(
        "pub fn x(f) {
  case 1 |> f {
    x -> x
  }
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1379
#[test]
fn pipe_in_spread() {
    assert_erl!(
        "pub type X {
  X(a: Int, b: Int)
}

fn id(x) {
  x
}
        
pub fn main(x) {
  X(..x, a: 1 |> id)
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1385
#[test]
fn pipe_in_eq() {
    assert_erl!(
        "fn id(x) {
  x
}
        
pub fn main() {
    1 == 1 |> id
}"
    );
}
