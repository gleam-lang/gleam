use crate::{assert_error, assert_infer, assert_module_infer, assert_warning};

#[test]
fn arity_1() {
    assert_module_infer!(
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
        vec![("main", "fn() -> #(Int, Int)")],
    )
}

#[test]
fn arity_2() {
    assert_module_infer!(
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
        vec![("main", "fn() -> #(Float, Int)")],
    )
}

#[test]
fn arity_3() {
    assert_module_infer!(
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
        vec![("main", "fn() -> #(Float, String, Int)")],
    )
}

#[test]
fn call_is_variable() {
    assert_infer!(
        r#"
let call = fn(f) { f() }
use <- call
123
"#,
        "Int"
    );
}

#[test]
fn call_is_literal() {
    assert_infer!(
        r#"
use <- fn(f) { f() }
123.0
"#,
        "Float"
    );
}

#[test]
fn call_is_capture() {
    assert_infer!(
        r#"
let f = fn(a, b) { a() + b }
use <- f(_, 123)
123
"#,
        "Int"
    );
}

#[test]
fn invalid_call_is_number() {
    assert_error!(
        r#"
use <- 123
123
"#
    );
}

#[test]
fn no_callback_body() {
    assert_warning!(
        r#"
pub fn main() {
  let thingy = fn(f) { f() }
  use <- thingy()
}
"#
    );
}

#[test]
fn invalid_callback_type() {
    assert_error!(
        r#"
let x = fn(f) { f() + 1 }
use <- x()
Nil
"#
    );
}

#[test]
fn discard() {
    assert_infer!(
        r#"
let x = fn(f) { f(123) }
use _ <- x()
Nil
"#,
        "Nil",
    );
}

#[test]
fn discard_named() {
    assert_infer!(
        r#"
let x = fn(f) { f(123) }
use _wibble <- x()
Nil
"#,
        "Nil",
    );
}

#[test]
fn just_use_in_fn_body() {
    assert_warning!(
        r#"
pub fn main() {
  use <- wibble()
}

fn wibble(f) {
  f()
}
"#
    );
}

#[test]
fn labels() {
    assert_module_infer!(
        r#"
pub fn main() {
  use x <- apply(arg: 1)
  x
}

fn apply(fun fun, arg arg) {
  fun(arg)
}
"#,
        vec![("main", "fn() -> Int")],
    );
}

#[test]
fn patterns() {
    assert_module_infer!(
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
"#,
        vec![("main", "fn() -> Int")],
    );
}

#[test]
fn multiple_patterns() {
    assert_module_infer!(
        r#"
pub fn main() {
  use Box(x), Box(y), Box(z) <- apply(Box(1))
  x + y + z
}

type Box(a) {
  Box(a)
}

fn apply(arg, fun) {
  fun(arg, arg, arg)
}
"#,
        vec![("main", "fn() -> Int")],
    );
}
