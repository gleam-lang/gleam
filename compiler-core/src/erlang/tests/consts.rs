// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 The Gleam contributors

use crate::assert_erl;

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = X

pub fn main() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2163
#[test]
fn record_constructor_in_tuple() {
    assert_erl!(
        r#"
pub type X {
  X(Int)
}

pub const z = #(X)

pub fn main() {
  z
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2179
#[test]
fn const_type_variable() {
    assert_erl!(
        r#"
fn identity(a: a) -> a {
  a
}

const id: fn(a) -> a = identity
"#
    );
}

#[test]
fn const_generalise() {
    assert_erl!(
        r#"
fn identity(a: a) -> a {
a
}

const id  = identity

pub fn main(){
  let num  = id(1)
  let word = id("Word")
}"#
    );
}

#[test]
fn pub_const_equal_to_private_function() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub const id = identity
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn pub_const_equal_to_record_with_nested_private_function_field() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub type Mapper(b) {
            Mapper(fn(b) -> b)
          }

          pub type Funcs(b) {
            Funcs(mapper: Mapper(b))
          }

          pub const id_mapper = Funcs(Mapper(identity))
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_private_function() {
    assert_erl!(
        r#"
              fn identity(a) {
                a
              }

              pub const id = identity
        "#
    );
}

#[test]
fn use_unqualified_pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_qualified_pub_const_equal_to_record_with_private_function_field() {
    assert_erl!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_private_in_internal() {
    assert_erl!(
        r#"
              fn identity(a) {
                a
              }

              pub type Mapper(b) {
                Mapper(fn(b) -> b)
              }

              @internal
              pub const id_mapper = Mapper(identity)
        "#
    );
}

#[test]
fn use_private_in_list() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub const funcs = [identity]
        "#
    )
}

#[test]
fn use_private_in_tuple() {
    assert_erl!(
        r#"
          fn identity(a) {
            a
          }

          pub const funcs = #(identity)
        "#
    )
}

#[test]
fn list_prepend() {
    assert_erl!(
        "
const wibble = [2, 3, 4]
pub const wobble = [0, 1, ..wibble]

pub fn main() {
  wobble
}
"
    );
}

#[test]
fn list_prepend_from_other_module() {
    assert_erl!(
        ("thepackage", "mod", "pub const wibble = [2, 3, 4]"),
        "
import mod

pub const wobble = [0, 1, ..mod.wibble]

pub fn main() {
  wobble
}
"
    );
}

#[test]
fn list_prepend_literal() {
    assert_erl!(
        "
pub const wibble = [0, 1, ..[2, 3, 4]]

pub fn main() {
  wibble
}
"
    );
}

#[test]
fn string_constant_from_another_module_is_concatenated_correctly() {
    assert_erl!(
        ("dep", "mod", "pub const wibble = \"wibble!\""),
        r#"
import mod

pub fn go(x) {
  x <> "-" <> mod.wibble
}
"#
    );
}

#[test]
fn string_constant_from_another_module_is_concatenated_correctly_2() {
    assert_erl!(
        ("dep", "mod", "pub const wibble = \"wibble!\""),
        r#"
import mod.{wibble}

pub fn go(x) {
  x <> "-" <> wibble
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/6045
#[test]
fn constant_string_in_bit_array_segment_test_with_utf8_option() {
    assert_erl!(
        r#"
const message = "hello"
pub fn main() {
  let _ = <<message:utf8>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6045
#[test]
fn constant_string_in_bit_array_segment_test_with_utf16_option() {
    assert_erl!(
        r#"
const message = "hello"
pub fn main() {
  let _ = <<message:utf16>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_bit_array_segment_test_with_utf16_option() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module.{wibble}

const wobble = wibble

pub fn main() {
  let _ = <<wobble:utf16>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_bit_array_segment_test_with_utf16_option_2() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module

const wobble = some_module.wibble
const wubble = wobble

pub fn main() {
  let _ = <<wubble:utf16>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_bit_array_segment_test_with_utf8_option() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module.{wibble}

const wobble = wibble

pub fn main() {
  let _ = <<wobble:utf8>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_bit_array_segment_test_with_utf8_option_2() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module

const wobble = some_module.wibble
const wubble = wobble

pub fn main() {
  let _ = <<wubble:utf8>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_aliased_string_in_bit_array_segment_test_with_utf8_option() {
    assert_erl!(
        r#"
const wibble = "a"
const wobble = wibble

pub fn main() {
  let _ = <<wobble:utf8>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_aliased_string_in_bit_array_segment_test_with_utf16_option() {
    assert_erl!(
        r#"
const wibble = "a"
const wobble = wibble

pub fn main() {
  let _ = <<wobble:utf16>>
  Nil
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_string_concatenation() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module

const wobble = some_module.wibble
const wubble = wobble

pub fn main() {
  let _ = wubble <> "b"
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_imported_aliased_string_in_string_concatenation_in_guard() {
    assert_erl!(
        (
            "some_module",
            "some_module",
            r#"
pub const wibble = "a"
            "#
        ),
        r#"
import some_module

const wobble = some_module.wibble
const wubble = wobble

pub fn go(x) {
  case x {
    x if x == wubble <> "b" -> todo
    _ -> todo
  }
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/6086
#[test]
fn constant_aliased_string_in_concatenation() {
    assert_erl!(
        r#"
const wibble = "a"
const wobble = wibble

pub fn main() {
  let _ = wobble <> "b"
}
"#
    )
}

#[test]
fn multiple_remote_constants_non_alphabetical_order() {
    assert_erl!(
        (
            "dep",
            "mod",
            "
pub const b = 2
pub const a = 1
pub const c = 3
"
        ),
        r#"
import mod

pub fn go(x) {
  case x {
    _ if x == mod.b -> "b"
    _ if x == mod.a -> "a"
    _ if x == mod.c -> "c"
    _ -> "dunno"
  }
}
"#
    );
}

#[test]
fn multiple_remote_constants_non_alphabetical_order_unqualified() {
    assert_erl!(
        (
            "dep",
            "mod",
            "
pub const b = 2
pub const a = 1
pub const c = 3
"
        ),
        r#"
import mod.{b, c, a}

pub fn go(x) {
  case x {
    _ if x == b -> "b"
    _ if x == a -> "a"
    _ if x == c -> "c"
    _ -> "dunno"
  }
}
"#
    );
}

#[test]
fn constant_string_concatenation_with_qualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod
pub const greeting = "Hello, " <> mod.name <> "!"
"#
    );
}

#[test]
fn constant_string_concatenation_with_unqualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod.{name}
pub const greeting = "Hello, " <> name <> "!"
"#
    );
}

#[test]
fn guard_string_concatenation_with_qualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod

pub fn go(x) {
  case x {
    _ if "Hello, " <> mod.name <> "!" == "" -> Nil
    _ -> Nil
  }
}
"#
    );
}

#[test]
fn guard_string_concatenation_with_unqualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod.{name}

pub fn go(x) {
  case x {
    _ if "Hello, " <> name <> "!" == "" -> Nil
    _ -> Nil
  }
}
"#
    );
}

#[test]
fn string_concatenation_with_qualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod

pub fn wibble() {
  "Hello, " <> mod.name <> "!"
}
"#
    );
}

#[test]
fn string_concatenation_with_unqualified_remote_constant() {
    assert_erl!(
        (
            "dep",
            "mod",
            r#"
pub const name = "Giacomo"
"#
        ),
        r#"
import mod.{name}

pub fn wibble() {
  "Hello, " <> name <> "!"
}

"#
    );
}
