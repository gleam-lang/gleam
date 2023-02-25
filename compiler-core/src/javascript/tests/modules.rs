use crate::javascript::tests::CURRENT_PACKAGE;
use crate::{assert_js, assert_ts_def};

#[test]
fn empty_module() {
    // Renders an export statement to ensure it's an ESModule
    assert_js!("", "export {}\n");
}

#[test]
fn unqualified_fn_call() {
    assert_js!(
        (CURRENT_PACKAGE, "rocket_ship", r#"pub fn launch() { 1 }"#),
        r#"import rocket_ship.{launch}
pub fn go() { launch() }
"#,
    );
}

#[test]
fn aliased_unqualified_fn_call() {
    assert_js!(
        (CURRENT_PACKAGE, "rocket_ship", r#"pub fn launch() { 1 }"#),
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
            "rocket_ship",
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
        (CURRENT_PACKAGE, "rocket_ship", r#"pub const x = 1"#),
        r#"
import rocket_ship
pub fn go() { rocket_ship.x }
"#,
    );
}

#[test]
fn alias_aliased_constant() {
    assert_js!(
        (CURRENT_PACKAGE, "rocket_ship", r#"pub const x = 1"#),
        r#"
import rocket_ship.{ x as y }
const z = y
"#,
    );
}

#[test]
fn renamed_module() {
    assert_js!(
        (CURRENT_PACKAGE, "x", r#"pub const v = 1"#),
        r#"
import x as y
const z = y.v
"#,
    );
}

#[test]
fn nested_module_constant() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            "rocket_ship/launcher",
            r#"pub const x = 1"#
        ),
        r#"
import rocket_ship/launcher
pub fn go() { launcher.x }
"#,
    );
}

#[test]
fn alias_constant() {
    assert_js!(
        (CURRENT_PACKAGE, "rocket_ship", r#"pub const x = 1"#),
        r#"
import rocket_ship as boop
pub fn go() { boop.x }
"#,
    );
}

#[test]
fn alias_fn_call() {
    assert_js!(
        (CURRENT_PACKAGE, "rocket_ship", r#"pub fn go() { 1 }"#),
        r#"
import rocket_ship as boop
pub fn go() { boop.go() }
"#,
    );
}

#[test]
fn nested_fn_call() {
    assert_js!(
        (CURRENT_PACKAGE, "one/two", r#"pub fn go() { 1 }"#),
        r#"import one/two
pub fn go() { two.go() }"#,
    );
}

#[test]
fn nested_nested_fn_call() {
    assert_js!(
        (CURRENT_PACKAGE, "one/two/three", r#"pub fn go() { 1 }"#),
        r#"import one/two/three
pub fn go() { three.go() }"#,
    );
}

#[test]
fn different_package_import() {
    assert_js!(
        ("other_package", "one", r#"pub fn go() { 1 }"#),
        r#"import one
pub fn go() { one.go() }
"#,
    );
}

#[test]
fn nested_same_package() {
    assert_js!(
        (CURRENT_PACKAGE, "one/two/three", r#"pub fn go() { 1 }"#),
        r#"import one/two/three
pub fn go() { three.go() }
"#,
    );
}

#[test]
fn imported_external_types_dont_get_rendered() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            "one/two/three",
            r#"pub external type External"#
        ),
        r#"import one/two/three.{External}

pub fn go() { 1 }
"#,
    );
}

#[test]
fn imported_custom_types_dont_get_rendered() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            "one/two/three",
            r#"pub type Custom { One Two }"#
        ),
        r#"import one/two/three.{Custom, One, Two}

pub fn go() -> List(Custom) { [One, Two] }
"#,
    );
}

#[test]
fn imported_custom_types_do_get_rendered_in_typescript() {
    assert_ts_def!(
        (
            CURRENT_PACKAGE,
            "one/two/three",
            r#"pub type Custom { One Two }"#
        ),
        r#"import one/two/three.{Custom, One, Two}

pub fn go() -> List(Custom) { [One, Two] }
"#,
    );
}

#[test]
fn imported_external_types_dont_get_rendered_with_value_of_same_name() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            "one/two/three",
            r#"pub external type Thingy"#
        ),
        r#"import one/two/three.{Thingy}

type Dup { Thingy }

pub fn go(x: Thingy) -> List(Thingy) { [x, x] }
"#,
    );
}
