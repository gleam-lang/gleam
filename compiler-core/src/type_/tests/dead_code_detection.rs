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
