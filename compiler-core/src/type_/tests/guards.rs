// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

use crate::{assert_module_error, assert_module_infer};

#[test]
fn nested_record_access() {
    assert_module_infer!(
        r#"
pub type A {
  A(b: B)
}

pub type B {
  B(c: C)
}

pub type C {
  C(d: Bool)
}

pub fn a(a: A) {
  case a {
    _ if a.b.c.d -> 1
    _ -> 0
  }
}
"#,
        vec![
            ("A", "fn(B) -> A"),
            ("B", "fn(C) -> B"),
            ("C", "fn(Bool) -> C"),
            ("a", "fn(A) -> Int"),
        ],
    );
}

#[test]
fn string_variable_access() {
    assert_module_error!(
        r#"
pub fn a(a: String) {
  case a {
    _ if a.b -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn qualified_record() {
    assert_module_infer!(
        ("wibble", "pub type Wibble { Wibble Wobble }"),
        "
import wibble

pub fn main(wibble: wibble.Wibble) {
  case wibble {
    w if w == wibble.Wobble -> True
    _ -> False
  }
}
",
        vec![("main", "fn(Wibble) -> Bool")]
    );
}

#[test]
fn qualified_record_with_arguments() {
    assert_module_infer!(
        (
            "wibble",
            "pub type Wibble { Wibble(Int) Wobble(Int, Float) }"
        ),
        "
import wibble

pub fn main(wibble: wibble.Wibble) {
  case wibble {
    w if w == wibble.Wobble(1, 3.8) -> True
    _ -> False
  }
}
",
        vec![("main", "fn(Wibble) -> Bool")]
    );
}

#[test]
fn constant_with_wrong_type_in_guard() {
    assert_module_error!(
        r#"
pub type Wibble {
  Wibble(String)
}

pub const constant = Wibble("Hi")

pub fn main() {
  let wibble = "wibble"
  case 1 {
    _ if wibble == constant -> Nil
    _ -> Nil
  }
}
"#
    );
}

#[test]
fn unqualified_imported_constant_with_wrong_type_in_guard() {
    assert_module_error!(
        ("wibble", "pub const constant = 1"),
        r#"
import wibble.{constant}

pub fn main() {
  let wibble = "wibble"
  case 1 {
    _ if wibble == constant -> Nil
    _ -> Nil
  }
}
"#
    );
}
