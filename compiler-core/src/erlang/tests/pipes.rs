use crate::assert_erl;

#[test]
fn clever_pipe_rewriting() {
    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#
    );
}

#[test]
fn clever_pipe_rewriting1() {
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
fn pipe_in_record_update() {
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

// https://github.com/gleam-lang/gleam/issues/1863
#[test]
fn call_pipeline_result() {
    assert_erl!(
        r#"
pub fn main() {
  { 1 |> add }(1)
}

pub fn add(x) {
  fn(y) { x + y }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1931
#[test]
fn pipe_in_call() {
    assert_erl!(
        r#"
pub fn main() {
  123
  |> two(
    1 |> two(2),
    _,
  )
}

pub fn two(a, b) {
  a
}
"#
    );
}

#[test]
fn multiple_pipes() {
    assert_erl!(
        "
pub fn main() {
  1 |> x |> x
  2 |> x |> x
}

fn x(x) { x }
"
    );
}
