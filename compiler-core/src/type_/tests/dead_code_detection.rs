use crate::{assert_no_warnings, assert_warning};

#[test]
fn unused_recursive_function() {
    assert_warning!(
        "
fn unused(value: Int) -> Int {
  case value {
    0 -> 0
    _ -> unused(value - 1)
  }
}
"
    );
}

#[test]
fn unused_mutually_recursive_functions() {
    assert_warning!(
        "
fn wibble(value: Int) -> Int {
  wobble(value)
}

fn wobble(value: Int) -> Int {
  wibble(value)
}
"
    );
}

#[test]
fn constant_only_referenced_by_unused_function() {
    assert_warning!(
        "
const value = 10

fn unused() {
  value
}
"
    );
}

#[test]
fn constant_only_referenced_by_unused_constant() {
    assert_warning!(
        "
const value = 10

const value_twice = #(value, value)
"
    );
}

#[test]
fn constant_referenced_by_public_constant() {
    assert_no_warnings!(
        "
const value = 10

pub const value_twice = #(value, value)
"
    );
}

#[test]
fn type_variant_only_referenced_by_unused_function() {
    assert_warning!(
        "
type Wibble {
  Wibble
  Wobble
}

fn unused() {
  Wibble
}

pub fn used() {
  let _ = Wobble
  Nil
}
"
    );
}

#[test]
fn type_marked_as_used_if_variant_used() {
    assert_no_warnings!(
        "
type PrivateType {
  PrivateConstructor
}

pub fn public_function() {
  let _constructor = PrivateConstructor
  Nil
}
"
    );
}

#[test]
fn type_and_variant_unused() {
    assert_warning!(
        "
type PrivateType {
  PrivateConstructor
}
"
    );
}

#[test]
fn type_used_by_public_alias() {
    assert_no_warnings!(
        "
type PrivateType

pub type PublicAlias = PrivateType
"
    );
}

#[test]
fn imported_module_only_referenced_by_unused_function() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

fn unused() {
  wibble.Wibble
}
"
    );
}

#[test]
fn imported_module_alias_only_referenced_by_unused_function() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble as wobble

fn unused() {
  wobble.Wibble
}
"
    );
}

#[test]
fn imported_module_alias_only_referenced_by_unused_function_with_unqualified() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble.{type Wibble} as wobble

fn unused() {
  wobble.Wibble
}

pub fn main() -> Wibble {
  panic
}
"
    );
}

#[test]
fn imported_module_used_by_public_function() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

pub fn main() {
  wibble.Wibble(4)
}
"
    );
}

#[test]
fn imported_module_used_in_type() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

pub fn main() -> wibble.Wibble {
  panic
}
"
    );
}

#[test]
fn imported_module_used_by_public_constant() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

pub const value = wibble.Wibble(42)
"
    );
}

#[test]
fn imported_module_used_in_type_variant() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

pub type Wobble {
  Wobble(w: wibble.Wibble)
}
"
    );
}

#[test]
fn imported_module_used_in_type_alias() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble

pub type Wobble = wibble.Wibble
"
    );
}

#[test]
fn imported_value_only_referenced_by_unused_function() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(Int)
}
"
        ),
        "
import wibble.{Wibble}

fn unused() {
  Wibble
}
"
    );
}

#[test]
fn imported_type_only_referenced_by_unused_function() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble
"
        ),
        "
import wibble.{type Wibble}

fn unused() -> Wibble {
  panic
}
"
    );
}

#[test]
fn imported_value_used_by_public_function() {
    assert_no_warnings!(
        ("thepackage", "wibble", "pub type Wibble { Wibble }"),
        "
import wibble.{Wibble}

pub fn main() {
  Wibble
}
"
    );
}

#[test]
fn imported_type_used_by_public_function() {
    assert_no_warnings!(
        ("thepackage", "wibble", "pub type Wibble { Wibble }"),
        "
import wibble.{type Wibble}

pub fn main() -> Wibble {
  wibble.Wibble
}
"
    );
}

#[test]
fn imported_type_used_by_public_function_parameter() {
    assert_no_warnings!(
        ("thepackage", "wibble", "pub type Wibble { Wibble }"),
        "
import wibble.{type Wibble}

pub fn main(a: Wibble) {
  a
}
"
    );
}

#[test]
fn unused_type_alias() {
    assert_warning!(
        "
type Wibble = Int
"
    );
}

#[test]
fn private_type_alias_only_referenced_by_unused_function() {
    assert_warning!(
        "
type Wibble = Int

fn unused() -> Wibble {
  10
}
"
    );
}

#[test]
fn private_type_alias_underlying_type_referenced_by_public_function() {
    assert_warning!(
        "
type Wibble = Int

pub fn used() -> Int {
  10
}
"
    );
}

#[test]
fn private_type_alias_referenced_by_public_function() {
    assert_no_warnings!(
        "
type Wibble = Int

pub fn used() -> Wibble {
  10
}
"
    );
}

#[test]
fn shadowed_imported_value_marked_unused() {
    assert_warning!(
        (
            "wibble",
            "
pub const wibble = 1
"
        ),
        "
import wibble.{wibble}

pub const wibble = 2
"
    );
}

#[test]
fn used_shadowed_imported_value() {
    assert_warning!(
        (
            "thepackage",
            "wibble",
            "
pub const wibble = 1
"
        ),
        "
import wibble.{wibble}

pub const wibble = wibble
"
    );
}

#[test]
fn imported_module_marked_unused_when_shadowed_in_record_access() {
    assert_warning!(
        (
            "wibble",
            "
pub const wibble = 1
"
        ),
        "
import wibble

type Wibble {
  Wibble(wobble: Int)
}

pub fn main() {
  let wibble = Wibble(10)
  // This does not reference the `wibble` module!
  wibble.wobble
}
"
    );
}

#[test]
fn local_variable_marked_unused_when_shadowed_in_module_access() {
    assert_warning!(
        (
            "wibble",
            "
pub const wibble = 1
"
        ),
        "
import wibble

pub fn main() {
  let wibble = 10
  // This does not reference the `wibble` variable!
  wibble.wibble
}
"
    );
}

#[test]
// https://github.com/gleam-lang/gleam/issues/3552
fn constructor_used_if_type_alias_shadows_it() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble(String)
}
"
        ),
        r#"
import wibble.{Wibble}

type Wibble =
  wibble.Wibble

pub fn main() {
  Wibble("hello")
}
"#
    );
}

#[test]
fn imported_type_and_constructor_with_same_name() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble
}
"
        ),
        "
import wibble.{type Wibble, Wibble}

pub fn main() {
  Wibble
}
"
    );
}

#[test]
fn imported_type_and_constructor_with_same_name2() {
    assert_warning!(
        (
            "wibble",
            "
pub type Wibble {
  Wibble
}
"
        ),
        "
import wibble.{type Wibble, Wibble}

pub fn main() -> Wibble {
  wibble.Wibble
}
"
    );
}

#[test]
fn imported_type_and_constructor_with_same_name3() {
    assert_no_warnings!(
        (
            "thepackage",
            "wibble",
            "
pub type Wibble {
  Wibble
}
"
        ),
        "
import wibble.{type Wibble, Wibble}

pub fn main() -> Wibble {
  Wibble
}
"
    );
}
