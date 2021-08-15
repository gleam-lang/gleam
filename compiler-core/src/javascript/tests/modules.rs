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
        r#"import * as RocketShip from "../rocket_ship.js";
import { launch } from "../rocket_ship.js";

export function go() {
  return launch();
}
"#
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
        r#"import * as RocketShip from "../rocket_ship.js";
import { launch as boom_time } from "../rocket_ship.js";

export function go() {
  return boom_time();
}
"#
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
        r#"import * as RocketShip from "../rocket_ship.js";
import { a, b as bb } from "../rocket_ship.js";

export function go() {
  return a() + bb();
}
"#
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
        r#"import * as RocketShip from "../rocket_ship.js";

export function go() {
  return RocketShip.x;
}
"#
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
        r#"import * as Boop from "../rocket_ship.js";

export function go() {
  return Boop.x;
}
"#
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
        r#"import * as Boop from "../rocket_ship.js";

export function go() {
  return Boop.go();
}
"#
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
        r#"import * as Two from "../one/two.js";

export function go() {
  return Two.go();
}
"#
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
        r#"import * as Three from "../one/two/three.js";

export function go() {
  return Three.go();
}
"#
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
        r#"import * as One from "other_package/one.js";

export function go() {
  return One.go();
}
"#
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
        r#"import * as Three from "../one/two/three.js";

export function go() {
  return Three.go();
}
"#
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
        r#"import * as Three from "../one/two/three.js";
import { one, two } from "../one/two/three.js";

export function go() {
  return one() + two();
}
"#
    );
}
