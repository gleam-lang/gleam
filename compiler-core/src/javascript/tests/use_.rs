use crate::assert_js;

#[test]
fn arity_1() {
    assert_js!(
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
    assert_js!(
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
    assert_js!(
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
    assert_js!(
        r#"
pub fn main() {
  let thingy = fn(f) { f() }
  use <- thingy()
}
"#
    );
}

#[test]
fn patterns() {
    assert_js!(
        r#"
pub fn main() {
  use Box(x) <- apply(Box(1))
  x
}

type Box(a) {
  Box(a)
}

fn apply(arg, fun) {
  fun(arg)
}
"#
    );
}
