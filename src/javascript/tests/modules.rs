use crate::assert_js;

#[test]
fn importing_a_module() {
    assert_js!(
        (
            vec!["rocket_ship".to_string()],
            r#"
pub const c = 299_792_458

pub fn launch() {
  Ok("launched")
}
pub fn fuel(amount: Int) {
  Ok("fueled")
}
  "#
        ),
        r#"
import rocket_ship
import rocket_ship as foo
import rocket_ship.{launch as boom_time, fuel}
pub fn go() {
    rocket_ship.fuel(100)
    boom_time()
    rocket_ship.c
}
"#,
        r#""use strict";

import * as rocket_ship from "./rocket_ship.js";

import * as foo from "./rocket_ship.js";

import * as rocket_ship from "./rocket_ship.js";
const { launch: boom_time, fuel } = rocket_ship;

export function go() {
  rocket_ship.fuel(100);
  boom_time();
  return rocket_ship.c;
}
"#
    );
}

#[test]
fn nested_module_function_call() {
    assert_js!(
        (
            vec!["one".to_string(), "two".to_string()],
            r#"pub fn go() { 1 }"#
        ),
        r#"import one/two
pub fn go() { two.go() }"#,
        r#""use strict";

import * as two from "./one/two.js";

export function go() {
  return two.go();
}
"#
    );
}
