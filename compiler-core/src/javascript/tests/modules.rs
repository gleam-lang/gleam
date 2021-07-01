use crate::assert_js;
use crate::javascript::tests::CURRENT_PACKAGE;

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
        r#""use strict";

import * as RocketShip from "../rocket_ship.js";
const { launch } = RocketShip;

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
        r#""use strict";

import * as RocketShip from "../rocket_ship.js";
const { launch: boom_time } = RocketShip;

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
        r#""use strict";

import * as RocketShip from "../rocket_ship.js";
const { a, b: bb } = RocketShip;

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
        r#""use strict";

import * as RocketShip from "../rocket_ship.js";

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
        r#""use strict";

import * as Boop from "../rocket_ship.js";

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
        r#""use strict";

import * as Boop from "../rocket_ship.js";

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
        r#""use strict";

import * as Two from "../one/two.js";

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
        r#""use strict";

import * as Three from "../one/two/three.js";

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
        r#""use strict";

import * as One from "other_package/one.js";

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
        r#""use strict";

import * as Three from "../one/two/three.js";

export function go() {
  return Three.go();
}
"#
    );
}
