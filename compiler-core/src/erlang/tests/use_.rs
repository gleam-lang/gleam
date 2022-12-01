use crate::assert_erl;

#[test]
fn arity_1() {
    assert_erl!(
        r#"
pub fn main() {
  use <- pair()
  123
}

fn pair(f) {
  let x = f()
  #(x, x)
}
"#,
    )
}

#[test]
fn arity_2() {
    assert_erl!(
        r#"
pub fn main() {
  use <- pair(1.0)
  123
}

fn pair(x, f) {
  let y = f()
  #(x, y)
}
"#,
    )
}

#[test]
fn arity_3() {
    assert_erl!(
        r#"
pub fn main() {
  use <- trip(1.0, "")
  123
}

fn trip(x, y, f) {
  let z = f()
  #(x, y, z)
}
"#,
    )
}

#[test]
fn no_callback_body() {
    assert_erl!(
        r#"
pub fn main() {
  let thingy = fn(f) { f() }
  use <- thingy()
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1863
#[test]
fn pipeline_that_returns_fn() {
    assert_erl!(
        r#"
pub fn main() {
  use <- 1 |> add
  1
}

pub fn add(x) {
  fn(f) { f() + x }
}
"#
    );
}
