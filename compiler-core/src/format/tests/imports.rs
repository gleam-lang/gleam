use crate::{assert_format, assert_format_rewrite};

#[test]
fn types_and_values() {
    assert_format!(
        "import one/two.{type Abc, type Bcd, Abc, Bcd, abc, bcd}
"
    );
}

#[test]
fn discarded_import() {
    assert_format!(
        "import one/two as _three
"
    );
}

#[test]
fn discarded_import_with_unqualified() {
    assert_format!(
        "import one/two.{type Abc, Bcd, abc} as _three
"
    );
}

#[test]
fn imports_are_sorted_alphabetically() {
    assert_format_rewrite!(
        "import c import a/a import a/c import b import a/ab import a",
        "import a
import a/a
import a/ab
import a/c
import b
import c
"
    );
}

#[test]
fn gleam_packages_are_always_on_top() {
    assert_format_rewrite!(
        "import a import gleam/list import gleam/int",
        "import gleam/int
import gleam/list
import a
"
    );
}

#[test]
fn import_groups_are_respected() {
    assert_format_rewrite!(
        "import group_one/a
import group_one/b
// another group
import group_two/wobble
import group_two/wibble
// yet another group
import group_three/b
import group_three/c
import group_three/a
",
        "import group_one/a
import group_one/b

// another group
import group_two/wibble
import group_two/wobble

// yet another group
import group_three/a
import group_three/b
import group_three/c
"
    );
}

#[test]
fn empty_lines_define_different_groups() {
    assert_format_rewrite!(
        "import c
@target(javascript)
import b

import a

import gleam/string
import gleam/list",
        "@target(javascript)
import b
import c

import a

import gleam/list
import gleam/string
"
    );
}

#[test]
fn import_groups_with_empty_lines_and_comments() {
    assert_format_rewrite!(
        "import c
@target(javascript)
import b

import a
// third group
import gleam/string
import gleam/list

import wobble
import wibble
",
        "@target(javascript)
import b
import c

import a

// third group
import gleam/list
import gleam/string

import wibble
import wobble
"
    );
}
