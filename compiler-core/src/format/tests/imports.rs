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
        "import c
@target(javascript)
import b
// Another group
import a",
        "@target(javascript)
import b
import c

// Another group
import a
"
    );
}
