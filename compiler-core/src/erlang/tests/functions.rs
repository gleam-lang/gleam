use crate::assert_erl;

#[test]
fn function_as_value() {
    assert_erl!(
        r#"
fn other() {
  Nil
}

pub fn main() {
  other
}
"#
    );
}

#[test]
fn nested_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other

pub fn main() {
  other.wibble
}
"#
    );
}

#[test]
fn nested_unqualified_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble}

pub fn main() {
  wibble
}
"#
    );
}

#[test]
fn nested_aliased_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble as wobble}

pub fn main() {
  wobble
}
"#
    );
}

#[test]
fn function_called() {
    assert_erl!(
        r#"
pub fn main() {
  main()
}
"#
    );
}

#[test]
fn nested_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other

pub fn main() {
  other.wibble()
}
"#
    );
}

#[test]
fn nested_unqualified_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble}

pub fn main() {
  wibble()
}
"#
    );
}

#[test]
fn nested_aliased_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble as wobble}

pub fn main() {
  wobble()
}
"#
    );
}

#[test]
fn labelled_argument_ordering() {
    // https://github.com/gleam-lang/gleam/issues/3671
    assert_erl!(
        "
type A { A }
type B { B }
type C { C }
type D { D }

fn wibble(a a: A, b b: B, c c: C, d d: D) {
  Nil
}

pub fn main() {
  wibble(A, C, D, b: B)
  wibble(A, C, D, b: B)
  wibble(B, C, D, a: A)
  wibble(B, C, a: A, d: D)
  wibble(B, C, d: D, a: A)
  wibble(B, D, a: A, c: C)
  wibble(B, D, c: C, a: A)
  wibble(C, D, b: B, a: A)
}
"
    );
}

#[test]
fn unused_private_functions() {
    assert_erl!(
        r#"
pub fn main() -> Int {
  used()
}

fn used() -> Int {
  123
}

fn unused1() -> Int {
  unused2()
}

fn unused2() -> Int {
  used()
}

fn unused3() -> Int {
  used()
}
"#
    );
}
