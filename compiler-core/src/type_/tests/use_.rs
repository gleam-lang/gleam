use crate::{assert_error, assert_infer, assert_module_error, assert_module_infer, assert_warning};

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
fn wrong_arity() {
    assert_error!(
        r#"
let f = fn(callback) { callback(1, 2) }
use <- f
123
"#
    );
}

#[test]
fn use_with_function_that_doesnt_take_callback_as_last_arg_1() {
    assert_error!(
        r#"
let f = fn(a) { a + 1 }
use <- f
123
"#
    );
}

#[test]
fn use_with_function_that_doesnt_take_callback_as_last_arg_2() {
    assert_error!(
        r#"
let f = fn() { 1 }
use <- f
123
"#
    );
}

#[test]
fn use_with_function_that_doesnt_take_callback_as_last_arg_3() {
    assert_error!(
        r#"
let f = fn(a, b) { a + b }
use <- f(1)
123
"#
    );
}

#[test]
fn wrong_arity_less_than_required() {
    assert_error!(
        r#"
let f = fn(a, b) { 1 }
use <- f
123
"#
    );
}

#[test]
fn wrong_arity_less_than_required_2() {
    assert_error!(
        r#"
let f = fn(a, b, c) { 1 }
use <- f(1)
123
"#
    );
}

#[test]
fn wrong_arity_more_than_required() {
    assert_error!(
        r#"
let f = fn(a, b) { 1 }
use <- f(1, 2)
123
"#
    );
}

#[test]
fn wrong_arity_more_than_required_2() {
    assert_error!(
        r#"
let f = fn(a, b) { 1 }
use <- f(1, 2, 3)
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
fn invalid_callback_type_2() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f() }
use <- x()
let n = 1
n + 2
"#
    );
}

#[test]
fn invalid_callback_type_3() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f() }
let y = fn(f) { 1 + f() }
use <- x()
use <- y()
let n = 1
n + 1
"#
    );
}

#[test]
fn invalid_callback_type_4() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f() }
let y = fn(f) { 1 + f() }
let z = fn(f) { 1.0 +. f() }
use <- x()
use <- y()
let n = 1
use <- z()
1.0
"#
    );
}

#[test]
fn wrong_callback_arity() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f() }
use _ <- x()
"Giacomo!"
"#
    );
}

#[test]
fn wrong_callback_arity_2() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f(1) }
use <- x()
"Giacomo!"
"#
    );
}

#[test]
fn wrong_callback_arity_3() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f(1) }
use _, _ <- x()
"Giacomo!"
"#
    );
}

#[test]
fn wrong_callback_arg() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f(1) }
use n <- x()
n <> "Giacomo!"
"#
    );
}

#[test]
fn wrong_callback_arg_with_wrong_annotation() {
    assert_error!(
        r#"
let x = fn(f) { "Hello, " <> f(1) }
use n: String <- x()
n <> "Giacomo!"
"#
    );
}

#[test]
fn wrong_callback_arg_2() {
    assert_module_error!(
        r#"
pub type Box {
  Box(Int)
}

pub fn main() {
  let x = fn(f) { "Hello, " <> f(Box(1)) }
  use Box("hi") <- x()
  "Giacomo!"
}
"#
    );
}

#[test]
fn wrong_callback_arg_3() {
    assert_module_error!(
        r#"
pub type Box {
  Box(Int)
}

pub fn main() {
  let x = fn(f) { "Hello, " <> f(1) }
  use Box(1) <- x()
  "Giacomo!"
}
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

#[test]
fn typed_pattern() {
    assert_module_infer!(
        r#"
pub fn main() {
  use Box(x): Box(Int), Box(y), Box(z) <- apply(Box(1))
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

#[test]
fn typed_pattern_wrong_type() {
    assert_module_error!(
        r#"
pub fn main() {
  use Box(x): Box(Bool), Box(y), Box(z) <- apply(Box(1))
  x + y + z
}

type Box(a) {
  Box(a)
}

fn apply(arg, fun) {
  fun(arg, arg, arg)
}
"#
    );
}
