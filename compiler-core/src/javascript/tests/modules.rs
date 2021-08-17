// TODO: convert to snapshots

use crate::assert_js;
use crate::javascript::tests::CURRENT_PACKAGE;

#[test]
fn empty_module() {
    // Renders an export statement to ensure it's an ESModule
    assert_js!("", "export {};\n");
}

#[test]
fn unqualified_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"pub fn launch() { 1 }"#
        ),
        r#"import rocket_ship.{launch}
pub fn go() { launch() }
"#,
    );
}

#[test]
fn aliased_unqualified_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"pub fn launch() { 1 }"#
        ),
        r#"import rocket_ship.{launch as boom_time}
pub fn go() { boom_time() }
"#,
    );
}

#[test]
fn multiple_unqualified_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"
pub fn a() { 1 }
pub fn b() { 2 }"#
        ),
        r#"import rocket_ship.{a,b as bb}
pub fn go() { a() + bb() }
"#,
    );
}

#[test]
fn constant() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"pub const x = 1"#
        ),
        r#"
import rocket_ship
pub fn go() { rocket_ship.x }
"#,
    );
}

#[test]
fn alias_constant() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"pub const x = 1"#
        ),
        r#"
import rocket_ship as boop
pub fn go() { boop.x }
"#,
    );
}

#[test]
fn alias_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["rocket_ship".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"
import rocket_ship as boop
pub fn go() { boop.go() }
"#,
    );
}

#[test]
fn nested_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["one".to_string(), "two".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"import one/two
pub fn go() { two.go() }"#,
    );
}

#[test]
fn nested_nested_fn_call() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["one".to_string(), "two".to_string(), "three".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"import one/two/three
pub fn go() { three.go() }"#,
    );
}

#[test]
fn different_package_import() {
    assert_js!(
        (
            "other_package",
            vec!["one".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"import one
pub fn go() { one.go() }
"#,
    );
}

#[test]
fn nested_same_package() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["one".to_string(), "two".to_string(), "three".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"import one/two/three
pub fn go() { three.go() }
"#,
    );
}

#[test]
fn same_import_multiple_times() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["one".to_string(), "two".to_string(), "three".to_string()],
            r#"pub fn one() { 1 } pub fn two() { 2 }"#
        ),
        r#"import one/two/three.{one}
import one/two/three.{two}

pub fn go() { one() + two() }
"#,
    );
}
